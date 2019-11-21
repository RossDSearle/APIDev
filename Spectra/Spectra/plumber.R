library(plumber)
library(asdreader)
library(stringr)


rootDir <- 'C:/Users/sea084/Dropbox/RossRCode/Git/Spectra'
file_path <- "c:/temp/APIdownloads"

#* @apiTitle Soil Spectra Processing
#* Load a spectra and get back the a modelled attribute value

#* @post /modelledValue
function(req, res){
 # cat("---- New Upload request ----\n")
  
  upload <- list(formContents = Rook::Multipart$parse(req))

  fromF = upload$formContents$fileinfo$tempfile
  toF <- paste0(file_path, '/',upload$formContents$fileinfo$filename)
  print(fromF)
  print(toF)
  file.copy(fromF, toF , overwrite = T )

  spectra <- as.data.frame(get_spectra(paste0(file_path, '/', upload$formContents$fileinfo$filename), type = "reflectance"))
  
  spName <- str_remove_all(upload$formContents$fileinfo$filename, '.asd')
  val <- getAttributeValue(upload$formContents$attribute, spectra)
  
  
  outdf <- data.frame(ASRIS_Spectra_ID='1748', Spectra_Name=spName, SoilAttribute=upload$formContents$attribute, Modelled_Value=val,  
                      Latitude=upload$formContents$latitude, Longitude=upload$formContents$longitude, 
                      Upperdepth=upload$formContents$upperdepth, Lowerdepth=upload$formContents$lowerdepth, Raw_Spectra='NULL')
  outdf$Raw_Spectra <- spectra
 
  outdf$Modelled_Value <- val
  return(outdf)
}

getAttributeValue <- function(att, spectra){
  
  
  modelName <- models[att]
  modelPath <- paste0(rootDir, '/Models/', modelName)
  model <- readRDS(modelPath)
  print(model)
  nc<-5 # no. components to use
  # to predict value based on a given spectra (calibration data)
  spectra <- trimSpec(spectra, wavlimits = c(453,2500))
  modelDat <- data.frame(TV = 0, NIR = I(as.matrix(spectra)))
  soil.pls_predict <- predict(model, ncomp = nc, newdata = modelDat)
  str( soil.pls_predict)
  return (soil.pls_predict[1,1,1])
}


trimSpec <- function(spectra, wavlimits) {
  datawavs <- as.numeric(names(spectra))
  limits <- which(datawavs %in% wavlimits)
  kept_index <- seq(limits[1], limits[2], 1)
  trimmed_spectra <- spectra[, kept_index]
  kept_names <- datawavs[kept_index]
  colnames(trimmed_spectra) <- kept_names
  return(trimmed_spectra)}

models <- c(SOC='SOC_Model.rds', BD='BD_Model.rds')


#* @apiTitle Plumber Example API

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
#* @post /echo
# function(req){
#  bob <- list(formContents = Rook::Multipart$parse(req))
#   print(str(bob))
#   
#   print(req$postBody$upload$tempfile)
#   somefile <- readLines(con = formContents$upload$tempfile)
#   print(somefile)
# }


