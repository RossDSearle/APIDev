library(asdreader)
library(stringr)
library(DBI)
library(odbc)
library(XML)
library(xml2)
library(htmlTable)
library(plyr);library(signal);library(pls);library(ithir);library(MASS)
library(Rook)
library(lubridate)




getSpectaFilePath <- function(spectraID){
  
  con <- DBI::dbConnect(odbc::odbc(), Driver = msqlDriver, Server = msqlServer,  Database = msqlDatabase, UID = msqlUID, PWD = msqlPWD)
  
  sql <- paste0("SELECT * from SpectraMeta WHERE SpectraMeta.SpectraID = ", spectraID, "")
  
  qry <- dbSendQuery(con, sql)
  metadataDF <- dbFetch(qry)
  dbClearResult(qry)
  dbDisconnect(con)
  
  return(metadataDF)
  
}





getFullSpectraInfo <- function(SpecID){
  
  con <- DBI::dbConnect(odbc::odbc(), Driver = msqlDriver, Server = msqlServer,  Database = msqlDatabase, UID = msqlUID, PWD = msqlPWD)
  
  sql <- paste0("SELECT SpectraMeta.SpectraID, SITES.agency_code, SITES.proj_code, SITES.s_id, OBSERVATIONS.o_id, HORIZONS.h_no, SAMPLES.samp_no, SpectraMeta.Type, SpectraMeta.Username, 
  SpectraMeta.SubmitTime, SpectraMeta.Lattitude, SpectraMeta.Longitude, SpectraMeta.UpperDepth, SpectraMeta.LowerDepth, SpectraMeta.OriginalName
  FROM            SITES INNER JOIN
  OBSERVATIONS ON SITES.agency_code = OBSERVATIONS.agency_code AND SITES.proj_code = OBSERVATIONS.proj_code AND SITES.s_id = OBSERVATIONS.s_id INNER JOIN
  HORIZONS ON OBSERVATIONS.agency_code = HORIZONS.agency_code AND OBSERVATIONS.proj_code = HORIZONS.proj_code AND OBSERVATIONS.s_id = HORIZONS.s_id AND 
  OBSERVATIONS.o_id = HORIZONS.o_id INNER JOIN
  SAMPLES ON HORIZONS.agency_code = SAMPLES.agency_code AND HORIZONS.proj_code = SAMPLES.proj_code AND HORIZONS.s_id = SAMPLES.s_id AND HORIZONS.o_id = SAMPLES.o_id AND 
  HORIZONS.h_no = SAMPLES.h_no INNER JOIN
  ARCHIVE_SAMPLES ON SAMPLES.agency_code = ARCHIVE_SAMPLES.agency_code AND SAMPLES.proj_code = ARCHIVE_SAMPLES.proj_code AND SAMPLES.s_id = ARCHIVE_SAMPLES.s_id AND 
  SAMPLES.o_id = ARCHIVE_SAMPLES.o_id AND SAMPLES.h_no = ARCHIVE_SAMPLES.h_no AND SAMPLES.samp_no = ARCHIVE_SAMPLES.samp_no INNER JOIN
  SpectraMeta ON ARCHIVE_SAMPLES.spec_id = SpectraMeta.SpectraID 
  WHERE SpectraMeta.SpectraID = ", SpecID, "")
  
  qry <- dbSendQuery(con, sql)
  metadataDF <- dbFetch(qry)
  dbClearResult(qry)
  dbDisconnect(con)
  
  specDF <- getSpectraData(SpecID = metadataDF$SpectraID)
  SoilValues <- getLabData(agencyCode=metadataDF$agency_code, projCode=metadataDF$proj_code, sid=metadataDF$s_id, o_id=metadataDF$o_id, h_no=metadataDF$h_no, samp_no=metadataDF$samp_no)
  
  t1 <- t(metadataDF)             
  t2 <- data.frame(Attribute=rownames(t1), Value=t1[,1], row.names = NULL)
  
  odf <- NULL
  odf$Metadata <- t2
  odf$SoilValues <- SoilValues
  odf$Spectrum <- specDF

  return(odf)
}

getLabData <- function( agencyCode, projCode, sid, o_id, h_no, samp_no){
  
  con <- DBI::dbConnect(odbc::odbc(), Driver = msqlDriver, Server = msqlServer,  Database = msqlDatabase, UID = msqlUID, PWD = msqlPWD)
  
  sql <- paste0("select * from LAB_RESULTS where agency_code='", agencyCode, "' and proj_code='", projCode, "' and s_id='", sid, "' and o_id=" ,o_id, " and h_no=", h_no, " and samp_no=", samp_no, " and labr_no=1")
  qry <- dbSendQuery(con, sql)
  df <- dbFetch(qry)
  dbClearResult(qry)
  dbDisconnect(con)
  
  t <- df[, c(8, 10)]
  t2 <- merge(t, availAtts, by.x='labm_code', by.y='labcodes')
  t3 <- t2[, c(3,2,1)]
  colnames(t3) <- c('att', 'value', 'labCode')

  return(t3)
  
}

getSpectraData <- function(SpecID){
  
  con <- DBI::dbConnect(odbc::odbc(), Driver = msqlDriver, Server = msqlServer,  Database = msqlDatabase, UID = msqlUID, PWD = msqlPWD)
  
  sql <- paste0('select wavelength, value from Spectra where spectraID = ', SpecID)
  qry <- dbSendQuery(con, sql)
  df <- dbFetch(qry)
  dbClearResult(qry)
  dbDisconnect(con)
  
  return(df)
  
}

getAvalailableSpectra <- function(projCode='SpecDemo', userName='DemoUser'){
  
  con <- DBI::dbConnect(odbc::odbc(), Driver = msqlDriver, Server = msqlServer,  Database = msqlDatabase, UID = msqlUID, PWD = msqlPWD)
  
  sql <- paste0("SELECT        SpectraMeta.SpectraID, SITES.agency_code, SITES.proj_code, SITES.s_id, OBSERVATIONS.o_id, HORIZONS.h_no, SAMPLES.samp_no, SpectraMeta.DataPath, SpectraMeta.Type, SpectraMeta.Username, 
  SpectraMeta.SubmitTime, SpectraMeta.Lattitude, SpectraMeta.Longitude, SpectraMeta.UpperDepth, SpectraMeta.LowerDepth, SpectraMeta.OriginalName
  FROM            SITES INNER JOIN
  OBSERVATIONS ON SITES.agency_code = OBSERVATIONS.agency_code AND SITES.proj_code = OBSERVATIONS.proj_code AND SITES.s_id = OBSERVATIONS.s_id INNER JOIN
  HORIZONS ON OBSERVATIONS.agency_code = HORIZONS.agency_code AND OBSERVATIONS.proj_code = HORIZONS.proj_code AND OBSERVATIONS.s_id = HORIZONS.s_id AND 
  OBSERVATIONS.o_id = HORIZONS.o_id INNER JOIN
  SAMPLES ON HORIZONS.agency_code = SAMPLES.agency_code AND HORIZONS.proj_code = SAMPLES.proj_code AND HORIZONS.s_id = SAMPLES.s_id AND HORIZONS.o_id = SAMPLES.o_id AND 
  HORIZONS.h_no = SAMPLES.h_no INNER JOIN
  ARCHIVE_SAMPLES ON SAMPLES.agency_code = ARCHIVE_SAMPLES.agency_code AND SAMPLES.proj_code = ARCHIVE_SAMPLES.proj_code AND SAMPLES.s_id = ARCHIVE_SAMPLES.s_id AND 
  SAMPLES.o_id = ARCHIVE_SAMPLES.o_id AND SAMPLES.h_no = ARCHIVE_SAMPLES.h_no AND SAMPLES.samp_no = ARCHIVE_SAMPLES.samp_no INNER JOIN
  SpectraMeta ON ARCHIVE_SAMPLES.spec_id = SpectraMeta.SpectraID 
  WHERE SITES.proj_code = '", projCode, "' and SpectraMeta.Username = '", userName, "'")
  
  qry <- dbSendQuery(con, sql)
  df <- dbFetch(qry)
  
  str_trim(df)
  
  dbClearResult(qry)
  dbDisconnect(con)
  
  return(df)
}






submitSpectra <- function(specType, origName, specPath, latitude, longitude, upperDepth, lowerDepth, userName){
    
  print(specPath)
  
    submitTime <- Sys.time()
    specID <- paste0( userName, '_',Sys.Date(), '_', latitude , '_', longitude, '_', upperDepth, '_',lowerDepth,'_', str_to_upper(specType), '_', as.integer(submitTime), '_', origName)

    TospecPath <- paste0(spectraStore, '/Library/Uploads/',userName, '/',  specID)
    
    print(specPath)
    print(TospecPath)
    
    #specPath <- '/tmp/RtmpeuhGxJ/Multipartbbed4b3443'
    
    
    sp <- storeSpectraFile(specPath, TospecPath)
    
      spec <- loadSpectraFromFile(TospecPath, specType)
      
      
      specDF <- convertSpectraToDataframe(spec)
      dbInfo <- insertSpecIntoDB(specID, specDF, specType, TospecPath, latitude, longitude, upperDepth, lowerDepth, userName, submitTime, origName)

     
     if(str_to_upper(specType) == 'ASD'){
       outdf <-  predictASDValues(spectra = spec)
       insertLabResultsIntoNatsoil(dbInfo, outdf)
     }else{
         stop("This spectra type not supported as yet")
     }
      
      wavCnt <- nrow(specDF)
      minVal <- min(specDF$value)
      maxVal <- max(specDF$value)
      minWav <- min(specDF$wavelength)
      maxWav <- max(specDF$wavelength)

      mFields <- c( 'SpectraID', 'OriginalFileName', 'TimeSubmitted', 'SpectraType','UserName', 'Latitude', 'Longitude', 'UpperDepth', 'LowerDepth', 'WavelengthsCount', 'MinimumWavelength', 'MaximumWavelength',  'MinimumValue' , 'MaximumValue' )
      mVals <- c(dbInfo$specID, origName, submitTime, specType, userName,  latitude, longitude, upperDepth, lowerDepth , wavCnt, minWav, maxWav, minVal, maxVal)
      metadataDF <- data.frame(Attribute=mFields,Value=mVals)
      
    odf <- NULL
    odf$Metadata <- metadataDF
    odf$SoilValues <- outdf
    odf$Spectrum <- specDF

   return(odf)

}

convertSpectraToDataframe <- function(spectra){
  
  wavelengths <- as.numeric(names(spectra))
  outDF <- data.frame(wavelength=wavelengths, value=unlist(spectra))
}

loadSpectraFromFile <- function(specPath, specType){
  
  
  print('###########   HERE1   ############')
  if(file.exists(specPath)){
    
    if(str_to_upper(specType) == 'ASD'){
        spectra <- as.data.frame(asdreader::get_spectra(specPath, type = "reflectance"))
    }else{
      stop("This spectra type not supported as yet")
    }
    
  }else{
    stop("There was a problem loading the supplied spectra : file does not exist")
  }
 
  return(spectra)
}

storeSpectraFile <- function(FromspecPath, TospecPath){
  
  toDir <- dirname(TospecPath)
  if(!dir.exists(toDir)){dir.create(toDir, recursive = T)}
  
  res <- file.copy(FromspecPath, TospecPath , overwrite = T )
  
  if(res){
    return(TospecPath)
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
    labCode <- availAtts$labcodes[i]
    
    val <- getASDAttributeValue(att = att, spectra = spectra) 
    recDF <- data.frame(att=att, value=val$Value, cv=val$CV, labCode=labCode)
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


insertLabResultsIntoNatsoil <- function(dbInfo, outdf){
  
  con <- DBI::dbConnect(odbc::odbc(), Driver = msqlDriver, Server = msqlServer,  Database = msqlDatabase, UID = msqlUID, PWD = msqlPWD)

  for (i in 1:nrow(outdf)) {
    rec <- outdf[i,]
    dt <- Sys.Date()
    d<-lubridate::day(dt)
    m<-lubridate::month(dt)
    y<-lubridate::year(dt)
    
    dp <- str_pad(d, 2, pad = "0")
    mp <- str_pad(m, 2, pad = "0")
    yp <- str_pad(y, 2, pad = "0")
    
    archiveSQL <- paste0("INSERT INTO LAB_RESULTS(agency_code, proj_code, s_id, o_id, h_no, samp_no, labr_no, labm_code, labr_value,labr_date)
    VALUES ('", dbInfo$agencyCode , "', '", dbInfo$projCode , "', '", dbInfo$sid, "', ", dbInfo$oid, ", ", dbInfo$hno, ", ", dbInfo$sampno, ", 1, '", rec$labCode, "', ", rec$value, ", '", paste0(dp,mp,yp), "');")
    dbExecute(con, archiveSQL) 
  }
  
  dbDisconnect(con)
}




insertSpecIntoDB <- function(specID, specDF, type, specPath, latitude, longitude, upperDepth, lowerDepth, userName, submitTime, origName) {



con <- DBI::dbConnect(odbc::odbc(), Driver = msqlDriver, Server = msqlServer,  Database = msqlDatabase, UID = msqlUID, PWD = msqlPWD)
  
newSIDsql <- paste0("select max(CAST(s_id AS int)) from sites where agency_code='", DEF_agency_code, "' and proj_code='", DEF_proj_code, "'"  )


qry <- dbSendQuery(con, newSIDsql)
newIDt <- dbFetch(qry)
newID <- as.numeric(newIDt[1,1]) +1
if(is.na(newID)){newID<-1}


sitesSQL <- paste0("INSERT INTO sites(agency_code, proj_code, s_id) VALUES ('", DEF_agency_code, "', '", DEF_proj_code, "', '", newID, "' );")
dbExecute(con, sitesSQL) 

obsSQL <- paste0("INSERT INTO OBSERVATIONS(agency_code, proj_code, s_id, o_id, o_latitude_GDA94, o_longitude_GDA94) 
VALUES ('", DEF_agency_code, "', '", DEF_proj_code, "', '", newID, "', '1', ", longitude, ",", latitude, " );")
dbExecute(con, obsSQL)


horSQL <- paste0("INSERT INTO HORIZONS(agency_code, proj_code, s_id, o_id, h_no, h_upper_depth, h_lower_depth) 
VALUES ('", DEF_agency_code, "', '", DEF_proj_code, "', '", newID, "', '1', 1, ", upperDepth, ", ", lowerDepth,");")
print(horSQL)
dbExecute(con, horSQL)

sampSQL <- paste0("INSERT INTO SAMPLES(agency_code, proj_code, s_id, o_id, h_no, samp_no, samp_upper_depth, samp_lower_depth) 
VALUES ('", DEF_agency_code, "', '", DEF_proj_code, "', '", newID, "', '1', 1, 1, ", upperDepth, ", ", lowerDepth,");")
dbExecute(con, sampSQL) 


newSIDArchsql <- paste0("select max(CAST(spec_id AS int)) from ARCHIVE_SAMPLES"  )
qry <- dbSendQuery(con, newSIDArchsql)
newSpecIDt <- dbFetch(qry)
newSpecID <- as.numeric(newSpecIDt[1,1]) + 1


archiveSQL <- paste0("INSERT INTO ARCHIVE_SAMPLES(agency_code, proj_code, s_id, o_id, h_no, samp_no, jar_no, samp_type, [location], [weight],[>2mm], spec_id, subsample_date, subsample_tray)
VALUES ('", DEF_agency_code, "', '", DEF_proj_code, "', '", newID, "', '1', 1, 1, 1, 'xx', 'here', 123, 1,", newSpecID, ", '', '');")
dbExecute(con, archiveSQL) 

metaSQL <- paste0("INSERT INTO SpectraMeta([SpectraID], [DataPath], [Type] ,[Username], [SubmitTime] ,[Lattitude], [Longitude], [UpperDepth], [LowerDepth], [OriginalName])
VALUES (",newSpecID, ",'",specPath, "','", type ,  "', '", userName ,  "', '",  submitTime ,  "', ",  latitude ,  ", ",  longitude ,  ", ", upperDepth ,  ", ", lowerDepth ,  ", '", origName, "')")
print(metaSQL)
dbExecute(con, metaSQL) 


indf <- data.frame(spectraID=newSpecID, specDF, row.names = NULL)
DBI::dbWriteTable(con, "Spectra", indf, append = TRUE, overwrite = FALSE)


dbDisconnect(con)

l <- list(agencyCode=DEF_agency_code, projCode=DEF_proj_code, sid=newID, oid=1, hno=1,sampno=1, specID=newSpecID)

return(l)

}

