library(plumber)
library(asdreader)
library(stringr)
library(DBI)
library(odbc)
library(XML)
library(xml2)
library(htmlTable)


trimSpec <- function(spectra, wavlimits) {
  datawavs <- as.numeric(names(spectra))
  limits <- which(datawavs %in% wavlimits)
  kept_index <- seq(limits[1], limits[2], 1)
  trimmed_spectra <- spectra[, kept_index]
  kept_names <- datawavs[kept_index]
  colnames(trimmed_spectra) <- kept_names
  return(trimmed_spectra)}

models <- c(SOC='SOC_Model.rds', BD='BD_Model.rds')

rootDir <- 'C:/Users/sea084/Dropbox/RossRCode/Git/Spectra'


#* @apiTitle Soil Spectra Processing API
#* @apiDescription Just exploring some ideas of how the API might work


#* Load a spectra and get back the a modelled attribute value
#* @param format (Optional) format of the response to return. Either json, csv, or xml. Default = json
#* @tag Spectra Processing
#* @post /Upload
function(req, res, format){
  # cat("---- New Upload request ----\n")
  
  upload <- list(formContents = Rook::Multipart$parse(req))
  
  fromF = upload$formContents$fileinfo$tempfile
  toF <- paste0(SpectralLibraryPath, '/',upload$formContents$fileinfo$filename)
  print(fromF)
  print(toF)
  file.copy(fromF, toF , overwrite = T )
  
  spectra <- as.data.frame(get_spectra(paste0(rootDir, '/Library/Uploads', upload$formContents$fileinfo$filename), type = "reflectance"))
  
  spName <- str_remove_all(upload$formContents$fileinfo$filename, '.asd')
  val <- getAttributeValue(upload$formContents$attribute, spectra)
  
  
  outdf <- data.frame(ASRIS_Spectra_ID='1748', Spectra_Name=spName, SoilAttribute=upload$formContents$attribute, Modelled_Value=val,  
                      Latitude=upload$formContents$latitude, Longitude=upload$formContents$longitude, 
                      Upperdepth=upload$formContents$upperdepth, Lowerdepth=upload$formContents$lowerdepth, Raw_Spectra='NULL')
  outdf$Raw_Spectra <- spectra
  
  outdf$Modelled_Value <- val
  
  label <- 'Values'
  resp <- cerealize(outdf, label, format, res)
  return(resp)
}

#* Specify a spectra ID and get info back
#* @param format (Optional) format of the response to return. Either json, csv, or xml. Default = json
#* @param spectraID The ASRIS Spectra ID
#* @param attribute Attribute to return a value for
#* @tag Spectra Processing
#* @get /querySpectra
function(req, res, spectraID, attribute, format='json'){
  # cat("---- New Upload request ----\n")
  
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver   = "SQL Server",
                        Server   = "asris-sql-stage.it.csiro.au\\sql2017",
                        Database = "NatSoil",
                        UID      = 'NEXUS\\sea084',
                        PWD      = 'Joan4066',
                        Trusted_Connection = "True"
  )
  
  att <- "Organic carbon"
  
  
  sql <- paste0("SELECT ARCHIVE_SAMPLES.spec_id, OBSERVATIONS.agency_code, OBSERVATIONS.proj_code, OBSERVATIONS.s_id, OBSERVATIONS.o_id, HORIZONS.h_no, SAMPLES.samp_no, OBSERVATIONS.o_date_desc, OBSERVATIONS.o_longitude_GDA94, OBSERVATIONS.o_latitude_GDA94, SAMPLES.samp_upper_depth, SAMPLES.samp_lower_depth, LAB_RESULTS.labm_code, LAB_METHODS.LABM_SHORT_NAME, LAB_METHODS.LABM_NAME, LAB_RESULTS.labr_date, LAB_RESULTS.labr_value, LAB_METHODS.LABM_UNITS 
              FROM (((((SITES INNER JOIN OBSERVATIONS ON (SITES.s_id = OBSERVATIONS.s_id) AND (SITES.proj_code = OBSERVATIONS.proj_code) AND (SITES.agency_code = OBSERVATIONS.agency_code)) INNER JOIN HORIZONS ON (OBSERVATIONS.o_id = HORIZONS.o_id) AND (OBSERVATIONS.s_id = HORIZONS.s_id) AND (OBSERVATIONS.proj_code = HORIZONS.proj_code) AND (OBSERVATIONS.agency_code = HORIZONS.agency_code)) INNER JOIN SAMPLES ON (HORIZONS.h_no = SAMPLES.h_no) AND (HORIZONS.o_id = SAMPLES.o_id) AND (HORIZONS.s_id = SAMPLES.s_id) AND (HORIZONS.proj_code = SAMPLES.proj_code) AND (HORIZONS.agency_code = SAMPLES.agency_code)) INNER JOIN LAB_RESULTS ON (SAMPLES.samp_no = LAB_RESULTS.samp_no) AND (SAMPLES.h_no = LAB_RESULTS.h_no) AND (SAMPLES.o_id = LAB_RESULTS.o_id) AND (SAMPLES.s_id = LAB_RESULTS.s_id) AND (SAMPLES.proj_code = LAB_RESULTS.proj_code) AND (SAMPLES.agency_code = LAB_RESULTS.agency_code)) INNER JOIN ARCHIVE_SAMPLES ON (SAMPLES.samp_no = ARCHIVE_SAMPLES.samp_no) AND (SAMPLES.h_no = ARCHIVE_SAMPLES.h_no) AND (SAMPLES.o_id = ARCHIVE_SAMPLES.o_id) AND (SAMPLES.s_id = ARCHIVE_SAMPLES.s_id) AND (SAMPLES.proj_code = ARCHIVE_SAMPLES.proj_code) AND (SAMPLES.agency_code = ARCHIVE_SAMPLES.agency_code)) INNER JOIN LAB_METHODS ON LAB_RESULTS.labm_code = LAB_METHODS.LABM_CODE  
              WHERE ARCHIVE_SAMPLES.spec_id=", spectraID, " AND LAB_METHODS.LABM_SHORT_NAME='Organic carbon' 
              ORDER BY OBSERVATIONS.agency_code, OBSERVATIONS.proj_code, OBSERVATIONS.s_id, OBSERVATIONS.o_id, HORIZONS.h_no, SAMPLES.samp_no")
  
  
  qry <- dbSendQuery(con, sql)
  df <- dbFetch(qry)
  
  dbClearResult(qry)
  dbDisconnect(con)
  
  sPath <- paste0(rootDir, '/Library/raw/Archive_', spectraID, '.asd')
  spectra <- as.data.frame(get_spectra(sPath, type = "reflectance"))
  
  val <- getAttributeValue(attribute, spectra)
  df$ModelledValue <- val
  df$RawSpectra <- spectra
  
  label <- 'SpectraInfo'
  resp <- cerealize(df, label, format, res)
  return(resp)
}

#* Get a list of the Spectra Avaliable in Natsoil
#* @param format (Optional) format of the response to return. Either json, csv, or xml. Default = json
#* @param verbose return just the IDs or all of the data in the ASRIS database
#* @tag Spectra Processing
#* @get /availableSpectra
function(req, res, verbose, format='json'){
  
  fls <- list.files(paste0(rootDir, '/Library/raw'), '.asd')
  fls <- str_remove(fls , 'Archive_')
  fls <- str_remove(fls , '.asd') 
  fls <- paste0("'", fls, "'")
  
  qry1 <- paste(fls,  collapse = ',')
  spectraIDs <- paste0(' IN (', qry1, ')')
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver   = "SQL Server",
                        Server   = "asris-sql-stage.it.csiro.au\\sql2017",
                        Database = "NatSoil",
                        UID      = 'NEXUS\\sea084',
                        PWD      = 'Joan4066',
                        Trusted_Connection = "True"
  )
  
  att <- "Organic carbon"
  
  
  sql <- paste0("SELECT ARCHIVE_SAMPLES.spec_id, OBSERVATIONS.agency_code, OBSERVATIONS.proj_code, OBSERVATIONS.s_id, OBSERVATIONS.o_id, HORIZONS.h_no, SAMPLES.samp_no, OBSERVATIONS.o_date_desc, OBSERVATIONS.o_longitude_GDA94, OBSERVATIONS.o_latitude_GDA94, SAMPLES.samp_upper_depth, SAMPLES.samp_lower_depth, LAB_RESULTS.labm_code, LAB_METHODS.LABM_SHORT_NAME, LAB_METHODS.LABM_NAME, LAB_RESULTS.labr_date, LAB_RESULTS.labr_value, LAB_METHODS.LABM_UNITS 
              FROM (((((SITES INNER JOIN OBSERVATIONS ON (SITES.s_id = OBSERVATIONS.s_id) AND (SITES.proj_code = OBSERVATIONS.proj_code) AND (SITES.agency_code = OBSERVATIONS.agency_code)) INNER JOIN HORIZONS ON (OBSERVATIONS.o_id = HORIZONS.o_id) AND (OBSERVATIONS.s_id = HORIZONS.s_id) AND (OBSERVATIONS.proj_code = HORIZONS.proj_code) AND (OBSERVATIONS.agency_code = HORIZONS.agency_code)) INNER JOIN SAMPLES ON (HORIZONS.h_no = SAMPLES.h_no) AND (HORIZONS.o_id = SAMPLES.o_id) AND (HORIZONS.s_id = SAMPLES.s_id) AND (HORIZONS.proj_code = SAMPLES.proj_code) AND (HORIZONS.agency_code = SAMPLES.agency_code)) INNER JOIN LAB_RESULTS ON (SAMPLES.samp_no = LAB_RESULTS.samp_no) AND (SAMPLES.h_no = LAB_RESULTS.h_no) AND (SAMPLES.o_id = LAB_RESULTS.o_id) AND (SAMPLES.s_id = LAB_RESULTS.s_id) AND (SAMPLES.proj_code = LAB_RESULTS.proj_code) AND (SAMPLES.agency_code = LAB_RESULTS.agency_code)) INNER JOIN ARCHIVE_SAMPLES ON (SAMPLES.samp_no = ARCHIVE_SAMPLES.samp_no) AND (SAMPLES.h_no = ARCHIVE_SAMPLES.h_no) AND (SAMPLES.o_id = ARCHIVE_SAMPLES.o_id) AND (SAMPLES.s_id = ARCHIVE_SAMPLES.s_id) AND (SAMPLES.proj_code = ARCHIVE_SAMPLES.proj_code) AND (SAMPLES.agency_code = ARCHIVE_SAMPLES.agency_code)) INNER JOIN LAB_METHODS ON LAB_RESULTS.labm_code = LAB_METHODS.LABM_CODE  
              WHERE ARCHIVE_SAMPLES.spec_id", spectraIDs, " AND LAB_METHODS.LABM_SHORT_NAME='Organic carbon' 
              ORDER BY OBSERVATIONS.agency_code, OBSERVATIONS.proj_code, OBSERVATIONS.s_id, OBSERVATIONS.o_id, HORIZONS.h_no, SAMPLES.samp_no")
  
  
  qry <- dbSendQuery(con, sql)
  df <- dbFetch(qry)
  
  if(verbose){
    outDF <- df
  }else{
    outDF <- df$spec_id
  }
  
  label <- 'Available_Spectra'
  resp <- cerealize(outDF, label, format, res)
  return(resp)
}

getAttributeValue <- function(att, spectra){
  
  
  modelName <- models[att]
  modelPath <- paste0(rootDir, '/Models/', modelName)
  model <- readRDS(modelPath)
  
  nc<-5 # no. components to use
  # to predict value based on a given spectra (calibration data)
  spectra <- trimSpec(spectra, wavlimits = c(453,2500))
  modelDat <- data.frame(TV = 0, NIR = I(as.matrix(spectra)))
  soil.pls_predict <- predict(model, ncomp = nc, newdata = modelDat)
  
  return (soil.pls_predict[1,1,1])
}


cerealize <- function(DF, label, format, res){
  


  if(format == 'xml'){
    
    res$setHeader("Content-Type", "application/xml; charset=utf-8")
    print(format)
    xmlT <- writexml(DF, label)
   
    res$body <- xmlT
    return(res)
    
  }else if(format == 'csv'){
    res$setHeader("content-disposition", paste0("attachment; filename=", label, ".csv"));
    res$setHeader("Content-Type", "application/csv; charset=utf-8")
    res$body <- writecsv(DF)
    return(res)
    
  }else if(format == 'html'){
    res$setHeader("Content-Type", "text/html ; charset=utf-8")
    res$body <- htmlTable(DF, align = "l", align.header = "l", caption = label)
    return(res)
    
  }else{
    return(DF)
  }
  
  
}



writecsv <- function(DF){
  
  tc <- textConnection("value_str2", open="w")
  write.table(DF, textConnection("value_str2", open="w"), sep=",", row.names=F, col.names=T)
  value_str2 <- paste0(get("value_str2"), collapse="\n")
  close(tc)
  return(value_str2)
  
}

writexml <- function(df, label){
  
  
  
  o <- apply(df, 1, DataFrameToXmlwriter, label)
  s <- unlist(o)
  xml <- paste( s, collapse = '')
  xml2 <- str_replace_all(paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes" ?>\n<', label, 'Records>\n', xml, '</', label, 'Records>'), '&', '')
  
  
  #cat(xml2, file='c:/temp/x.xml')
  return(xml2)
}

DataFrameToXmlwriter <- function(x, label){
  
  v <- paste0('<', label, 'Record>')
  for (i in 1:length(names(x))) {
    
    v <- paste0(v, '<', names(x)[i], '>', str_replace(x[i], '<', 'less than'), '</', names(x)[i], '> ')
  }
  v <- paste0(v,'</', label, 'Record>\n')
  
  v2 <- v
  return(v2)
}




