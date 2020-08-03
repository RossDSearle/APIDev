library(asdreader)
library(stringr)
library(DBI)
library(odbc)
library(XML)
library(xml2)
library(htmlTable)
library(plyr);library(signal);library(pls);library(ithir);library(MASS)
library(Rook)



submitSpectra <- function(specType, specPath, latitude, longitude, upperDepth, lowerDepth, user){

    
    sp <- storeSpectraFile(specType, specPath, latitude, longitude, upperDepth, lowerDepth, user)
    
     spec <- loadSpectraFromFile(sp, specType)
     
     specDF <- convertSpectraToDataframe(spec)
     insertSpecIntoDB(specDF, specType, specPath, latitude, longitude, upperDepth, lowerDepth, user)

    if(str_to_upper(specType) == 'ASD'){
      outdf <-  predictASDValues(spectra = spec)
    }else{
        stop("This spectra type not supported as yet")
    }

    
    #return("hi")
   return(outdf)

}

convertSpectraToDataframe <- function(spectra){
  
  wavelengths <- as.numeric(names(spectra))
  outDF <- data.frame(wavelength=wavelengths, value=unlist(spectra))
}

loadSpectraFromFile <- function(specPath, specType){
  
  if(file.exists(specPath)){
    
    if(str_to_upper(specType) == 'ASD'){
        spectra <- as.data.frame(asdreader::get_spectra(sp, type = "reflectance"))
    }else{
      stop("This spectra type not supported as yet")
    }
    
  }else{
    stop("There was a problem loading the supplied spectra : file does not exist")
  }
 
  return(spectra)
}

storeSpectraFile <- function(type, specPath, latitude, longitude, upperDepth, lowerDepth, user){
  
  specPath = specPath
  toDir <- paste0(spectraStore, '/Library/Uploads/', user)
  if(!dir.exists(toDir)){dir.create(toDir, recursive = T)}
  toPath <- paste0(toDir, '/', user, '_',Sys.Date(), '_', latitude , '_', longitude, '_', upperDepth, '_',lowerDepth,'.', str_to_lower(type))
  
  print(specPath)
  print(toPath)
  res <- file.copy(specPath, toPath , overwrite = T )
  
  if(res){
    return(toPath)
  }else{
    stop('Error copying file into the Spectral library')
  }
}



###########################   Spectra Specific Functions  #####################################


trimSpec <- function(spectra, wavlimits) {
  datawavs <- as.numeric(names(spectra))
  limits <- which(datawavs %in% wavlimits)
  kept_index <- seq(limits[1], limits[2], 1)
  trimmed_spectra <- spectra[, kept_index]
  kept_names <- datawavs[kept_index]
  colnames(trimmed_spectra) <- kept_names
  return(trimmed_spectra)}


predictASDValues <- function(spectra){
  
 outDF <- data.frame(att=character(), value=numeric(), cv=numeric())
  
  for (i in 1:nrow(availAtts)) {
    
    att <- availAtts$modelNames[i]
    modelpath <- availAtts$models[i]
    
    val <- getASDAttributeValue(att = att, spectra = spectra) 
    recDF <- data.frame(att=att, value=val$Value, cv=val$CV)
    outDF <- rbind(outDF, recDF)
    
  }  
 
 return(outDF)
}




getASDAttributeValue <- function(att, spectra){
  
  modelName <- availAtts[availAtts$modelNames==att, 2]
  modelPath <- paste0(spectraStore, '/Models/', modelName)
  model <- readRDS(modelPath)
  
  nc<-5 # no. components to use
  # to predict value based on a given spectra (calibration data)
  spectra <- trimSpec(spectra, wavlimits = c(453,2500))
  modelDat <- data.frame(TV = 0, NIR = I(as.matrix(spectra)))
  soil.pls_predict <- predict(model, ncomp = nc, newdata = modelDat)
  
  rVal = NULL
  rVal$Value <- soil.pls_predict[1,1,1]
  rVal$CV <- model$validation$adj[1]
  return (rVal)
}




insertSpecIntoDB <- function(specDF, type, specPath, latitude, longitude, upperDepth, lowerDepth, user) {

  specID <- paste0( user, '_',Sys.Date(), '_', latitude , '_', longitude, '_', upperDepth, '_',lowerDepth,'_', str_to_lower(type), '_', as.integer(Sys.time()))
  

con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = msqlDriver,
                      Server   = msqlServer,
                      Database = msqlDatabase,
                      UID      = msqlUID,
                      PWD      = msqlPWD
                      
)

newSIDsql <- paste0("select max(CAST(s_id AS int)) from sites where agency_code='", DEF_agency_code, "' and proj_code='", DEF_proj_code, "'"  )
#newSIDsql <- paste0("select * from sites where agency_code='", DEF_agency_code, "' and proj_code='", DEF_proj_code, "'"  )

qry <- dbSendQuery(con, newSIDsql)
newIDt <- dbFetch(qry)
newID <- as.numeric(newIDt[1,1]) +1


# INSERT INTOprojects(agency_code, proj_code, proj_name) VALUES ('601', 'spectest', 'This is a test proj' );
# 
sitesSQL <- paste0("INSERT INTO sites(agency_code, proj_code, s_id) VALUES ('", DEF_agency_code, "', '", DEF_proj_code, "', '", newID, "' );")
dbExecute(con, sitesSQL) 

obsSQL <- paste0("INSERT INTO OBSERVATIONS(agency_code, proj_code, s_id, o_id, o_latitude_GDA94, o_longitude_GDA94) 
VALUES ('", DEF_agency_code, "', '", DEF_proj_code, "', '", newID, "', '1', ", longitude, ",", latitude, " );")
dbExecute(con, obsSQL)

# 
horSQL <- paste0("INSERT INTO HORIZONS(agency_code, proj_code, s_id, o_id, h_no, h_upper_depth, h_lower_depth) 
VALUES ('", DEF_agency_code, "', 'spectest', '", newID, "', '1', 1, ", upperDepth, ", ", lowerDepth,");")
dbExecute(con, horSQL)

sampSQL <- paste0("INSERT INTO SAMPLES(agency_code, proj_code, s_id, o_id, h_no, samp_no, samp_upper_depth, samp_lower_depth) 
VALUES ('", DEF_agency_code, "', 'spectest', '", newID, "', '1', 1, 1, ", upperDepth, ", ", lowerDepth,");")
dbExecute(con, sampSQL) 


newSIDArchsql <- paste0("select max(CAST(spec_id AS int)) from ARCHIVE_SAMPLES where agency_code='", DEF_agency_code, "' and proj_code='", DEF_proj_code, "'"  )
qry <- dbSendQuery(con, newSIDArchsql)
newaIDt <- dbFetch(qry)
newaID <- as.numeric(newaIDt[1,1]) + 1


archiveSQL <- paste0("INSERT INTO ARCHIVE_SAMPLES(agency_code, proj_code, s_id, o_id, h_no, samp_no, jar_no, samp_type, [location], [weight],[>2mm], spec_id, subsample_date, subsample_tray)
VALUES ('", DEF_agency_code, "', 'spectest', '", newID, "', '1', 1, 1, 1, 'xx', 'here', 123, 1,", newaID, ", '', '');")
dbExecute(con, archiveSQL) 

toDir <- paste0(spectraStore, '/Library/Uploads/', user)
toPath <- paste0(toDir, '/', user, '_',Sys.Date(), '_', latitude , '_', longitude, '_', upperDepth, '_',lowerDepth,'.', str_to_lower(type))
metaSQL <- paste0("INSERT INTO spectra_meta([SpectraNum], [SpectraID], [DataPath], [Type] ,[Username])
VALUES (",newaID, ",'", specID, "','",toPath, "','", type ,  "', '", user, "')")
dbExecute(con, metaSQL) 


indf <- data.frame(spectraID=newaID, specDF)
DBI::dbAppendTable(con, 'Spectra',  indf)


}

