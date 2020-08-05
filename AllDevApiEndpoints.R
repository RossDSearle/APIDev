library(plumber)
library(asdreader)
library(stringr)
library(DBI)
library(odbc)
library(XML)
library(xml2)
library(htmlTable)
library(plyr);library(signal);library(pls);library(ithir);library(MASS)
library(Rook)



machineName <- as.character(Sys.info()['nodename'])
print(machineName)


if(machineName == 'WALCOT-SL'){
  apiDevRootDir <<- 'C:/Users/sea084/Dropbox/RossRCode/Git/APIDev'
  conPath <<- 'C:/Projects/SMIPS/SFS/sfs.db'
  sfsDatRoot <<- 'C:/Users/sea084/Dropbox/RossRCode/Git/APIDev'
  SpectraRootDir <<- 'C:/Users/sea084/Dropbox/RossRCode/Git/APIDev/Spectra'
  spectraStore <<- 'C:/Projects/Spectra'
}else{
  apiDevRootDir <<- '/srv/plumber/APIDev'
  conPath <<- '/mnt/data/RegionalSoilMoisture/sfs.db'
  sfsDatRoot <<- '/mnt/data'
  SpectraRootDir <<- '/srv/plumber/APIDev/Spectra'
  spectraStore <<- '/mnt/data/Spectra'
}


source(paste0(apiDevRootDir, '/apiDev_Config.R'))
source(paste0(SpectraRootDir, '/Scripts/Processing.R'))
source(paste0(SpectraRootDir, '/SpectraAPIConfig.R'))


#* @apiTitle Ross' API Development Area
#* @apiDescription Some web service endpoints to demonstrate some concepts







library(odbc)
sort(unique(odbcListDrivers()[[1]]))

#* Get a list of the Spectra Avaliable in Natsoil
#* @param format (Optional) format of the response to return. Either json, csv, or xml. Default = json
#* @param verbose return just the IDs or all of the data in the ASRIS database
#* @param bbox rectangular window in geographic coordinates (minx;miny;maxx;maxy) - NOT IMPLEMENTED YET
#* @param enddate filter on an end date (dd-mm-yyyy) - NOT IMPLEMENTED YET
#* @param startdate filter on start date (dd-mm-yyyy) - NOT IMPLEMENTED YET
#* @param username filter on a specific user
#* @tag Spectra Processing
#* @get /SoilSpectra/availableSpectra
function(req, res, username, verbose=T, format='json'){
  
  projCode='SpecDemo'
  
  df <- getAvalailableSpectra(projCode, username)
  
  print(df)
  
  if(verbose){
    outDF <- df
  }else{
    outDF <- df$SpectraID
  }
  
  label <- 'AvailableSpectra'
  resp <- cerealize(outDF, label, format, res)
  return(resp)
}



#* Specify a spectra ID and get info back
#* @param format (Optional) format of the response to return. Either json, csv, or xml. Default = json
#* @param spectraID The ASRIS Spectra ID
#* @tag Spectra Processing
#* @get /SoilSpectra/querySpectra
function(req, res, spectraID, type, format='json'){
  
  odf <- getFullSpectraInfo(spectraID)
  label <- 'Values'
  resp <- cerealize(odf, label, format, res)
  return(resp)
}

#* Specify a spectra ID for the file to return

#* @param spectraID The ASRIS Spectra ID
#* @tag Spectra Processing
#* @serializer contentType list(type="application/octet-stream")
#* @get /SoilSpectra/RawSpectraFile
function(req, res, spectraID, type, format='json'){
  
  rec <- getSpectaFilePath(spectraID)
  
  filePath <- rec$DataPath
  origName <- rec$OriginalName
  
  if(!file.exists(filePath)){stop("The raw spectra file cannot be found in the system.")}
  
  fn <- basename(filePath)
  
  res$setHeader("Content-Disposition", paste0("attachment; filename=", origName))
  bin <- readBin(paste0(filePath), "raw", n=file.info(paste0(filePath))$size)
  
  bin
}


#* Load a spectra and get back the a modelled attribute value
#* @param format (Optional) format of the response to return. Either json, csv, or xml. Default = json

#* @tag Spectra Processing
#* @post /SoilSpectra/Upload
function(req, res, type, lattitude, longitude, upperDepth, lowerDepth, userName, format='json'){
  # cat("---- New Upload request ----\n")
  
  upload <- list(formContents = Multipart$parse(req))
  
   print( upload$formContents)
  # print( upload$formContents$fileinfo$tempfile)
  # 
  path <- upload$formContents$fileinfo$tempfile
  specType <- upload$formContents$specType
  longitude <- upload$formContents$longitude
  latitude <- upload$formContents$latitude
  upperDepth <- upload$formContents$upperDepth
  lowerDepth <- upload$formContents$lowerDepth
  userName <- upload$formContents$userName
  
  print(paste0("################################ : ", basename(upload$formContents$fileinfo$filename)))
  
  outdf<-submitSpectra(specType, origName = basename(upload$formContents$fileinfo$filename), specPath = upload$formContents$fileinfo$tempfile, latitude, longitude, upperDepth, lowerDepth, userName)

  
  label <- 'Values'
  resp <- cerealize(outdf, label, upload$formContents$format, res)
  return(resp)
}



















#* Returns a regional soil mositure map as GeoTiff

#* @param Date Date for soil moisture map. (format = YYYY-MM-DD)
#* @param Region Region to generate soil moisture map for. (SFS is only option currently)
#* @param Depth Depth to generate soil moisture map for. 
#* @tag Regional Soil Moisture Maps
#' @html
#' @get /SoilMoisture/GetMap

apiGetRegionalSoilMoistureMap <- function(res, Region='SFS', Date=NULL, Depth=NULL){
  
  
  tryCatch({
    
    res$setHeader("content-disposition", paste0("attachment; filename=SM_", Region, "_", Date, "_", Depth, ".tif"));
    res$setHeader("Content-Type", "image/tiff")

    fPath <- getRegionalSMMap2(Region, Date, as.numeric(Depth))
    print(fPath)
    bin <- readBin(paste0(fPath), "raw", n=file.info(paste0(fPath))$size)
    unlink(fPath)

    return(bin)
   
  }, error = function(res)
  {
    print(geterrmessage())
    res$status <- 400
    list(error=jsonlite::unbox(geterrmessage()))
  })
  
}

#* Returns an image of all the soil moisture map

#* @param Date Date for soil moisture map. (format = YYYY-MM-DD)
#* @param Region Region to generate soil moisture map for. (SFS is only option currently)
#* @param Depth Depth to generate soil moisture map for. 
#* @tag Regional Soil Moisture Maps

#* @png (width = 500, height = 500)
#* @get /SoilMoisture/GetMapAsImage
apiGetRegionalSoilMoistureMapAsImage <- function(res, Region='SFS', Date=NULL, Depth=NULL){
  
  tryCatch({
    
    fPath <- getRegionalSMMap2(Region, Date, as.numeric(Depth))
    r <- raster(fPath)
    spp <- plot(r, main=paste0('Soil Moisture at ', Depth, 'mm on ',Date))
    
    unlink(fPath)
    print(fPath)
    return(spp)
  }, error = function(res)
  {
    res$status <- 400 # Bad request
    list(error=jsonlite::unbox(geterrmessage()))
  })
}



#* Returns a clipped vector funtional soil type map as a geoJson Stream

#* @param PaddockID (Optional) - not needed for this demo
#* @tag AgX_Demo_Download_Soil_Polygons
#' @html
#' @get /AgX/SoilMap/geojson
apiGetFunctionalSoilMapGeojson <- function(res, paddockID){
  
   tryCatch({

  polys <- getSoilMapBoundaries()
  b <- as.character(geojson_json(polys))
  b
}, error = function(res)
{
  print(geterrmessage())
  res$status <- 400
  list(error=jsonlite::unbox(geterrmessage()))
})
  
}



#* Returns a clipped vector funtional soil type map as an image - NB Demo purposes only

#* @param PaddockID (Optional) - not needed for this demo
#* @tag AgX_Demo_Download_Soil_Polygons
#* @png
#* @get /AgX/SoilMap/image
apiGetFunctionalSoilMapImage <- function(PaddockID){
  
  tryCatch({
    
    polys <- getSoilMapBoundaries()

    plot(polys["Description"], main = 'Generic Soil Groups', key.pos = 4, pal = sf.colors(5), key.width = lcm(10), reset=FALSE)
    plot(st_geometry(bdy), add=T,  type='l', border='green')
    
  }, error = function(res)
  {
    print(geterrmessage())
    res$status <- 400
    list(error=jsonlite::unbox(geterrmessage()))
  })
}


#* Returns a clipped vector funtional soil type map as a zipped up shapefile
#* @tag AgX_Demo_Download_Soil_Polygons
#* @param PaddockID (Optional) - not needed for this demo
#* @serializer contentType list(type="application/octet-stream")
#* @get /AgX/SoilMap/zipfile
apiGetFunctionalSoilMapZip <- function(res, PaddockID){
  
  tryCatch({
    
    polys <- getSoilMapBoundaries()
    
  #fname <- tempfile()
  fname <- paste0(tempdir(), '/Soil_Boundaries_For_McDonalds')
  write_sf(polys, paste0(fname, '.shp'))
  
  
  origWD <- getwd()
  setwd(dirname(fname))
  fls <- list.files(dirname(fname), pattern = basename(fname), full.names = F)
  zip(paste0(fname, '.zip'), fls)
  
  res$setHeader("Content-Disposition", "attachment; filename=soils.zip")
  bin <- readBin(paste0(fname, '.zip'), "raw", n=file.info(paste0(fname, '.zip'))$size)
  unlink(paste0(fname, '.zip'))
  unlink(fls)
  setwd(origWD)
  
  bin
  
    
  }, error = function(res)
  {
    print(geterrmessage())
    res$status <- 400
    list(error=jsonlite::unbox(geterrmessage()))
  })
}



#* Returns a set of optimised sampling locations as geojson
#* @tag AgX_Demo_Optimised_Soil_Test_Sites 
#* @param PaddockID (Optional) - not needed for this demo
#* @param NumberOfSamples 
#* @html
#* @get /AgX/SoilTestLocations/GeoJson
getSampleLocationsGeoJson <- function(res, PaddockID, NumberOfSamples = 8 ){
  
tryCatch({
    
    pts <- getSampleLocations(as.numeric(NumberOfSamples))
    b <- geojsonio::geojson_json(pts)
    b
  }, error = function(res)
  {
    print(geterrmessage())
    res$status <- 400
    list(error=jsonlite::unbox(geterrmessage()))
  })
}

#* Returns a set of optimised sampling locations as an image - NB Demo purposes only
#* @tag AgX_Demo_Optimised_Soil_Test_Sites 
#* @param PaddockID (Optional) - not needed for this demo
#* @param NumberOfSamples 
#* @png
#* @get /AgX/SoilTestLocations/image
getSampleLocationsImage <- function(res, PaddockID, NumberOfSamples = 8 ){
  
  tryCatch({
    
    pts <- getSampleLocations(as.numeric(NumberOfSamples))
   
    paths = list.files(  paste0(AgXRootDir, '/OptimisedSamples'), pattern = 'KOOKABURRA', full.names = T, recursive =T)
    r <- raster(paste0(AgXRootDir, '/OptimisedSamples/KOOKABURRA_Wheat_2012_DryYield_clip.tif'))
    plot(r)
    points(as(pts, 'Spatial'), pch=19, col='blue', cex=2)
    
    
  }, error = function(res)
  {
    print(geterrmessage())
    res$status <- 400
    list(error=jsonlite::unbox(geterrmessage()))
  })
}

#* Returns a set of optimised sampling locations as a zipped up shapefile
#* @tag AgX_Demo_Optimised_Soil_Test_Sites 
#* @param PaddockID (Optional) - not needed for this demo
#* @param NumberOfSamples 
#* @serializer contentType list(type="application/octet-stream")
#* @get /AgX/SoilTestLocations/zip
getSampleLocationsZip <- function(res, PaddockID, NumberOfSamples = 8 ){
  
  tryCatch({
    
    pts <- getSampleLocations(as.numeric(NumberOfSamples))
    
    fname <- paste0(tempdir(), '/Sample_Locations_For_Kookaburra')
    #fname <- tempfile()
    write_sf(pts, paste0(fname, '.shp'))
    
    
    
    origWD <- getwd()
    setwd(dirname(fname))
    fls <- list.files(dirname(fname), pattern = basename(fname), full.names = F)
    zip(paste0(fname, '.zip'), fls)
    
    res$setHeader("Content-Disposition", "attachment; filename=SoilSampleLocations.zip")
    bin <- readBin(paste0(fname, '.zip'), "raw", n=file.info(paste0(fname, '.zip'))$size)
    unlink(paste0(fname, '.zip'))
    unlink(fls)
    setwd(origWD)
    
    bin
    
    
  }, error = function(res)
  {
    print(geterrmessage())
    res$status <- 400
    list(error=jsonlite::unbox(geterrmessage()))
  })
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








