

machineName <- as.character(Sys.info()['nodename'])
print(machineName)


if(machineName == 'FANCY-DP'){
  apiDevRootDir <<- 'C:/Users/sea084/Dropbox/RossRCode/Git/APIDev'
  conPath <<- 'C:/Projects/SMIPS/SFS/sfs.db'
  sfsDatRoot <<- 'C:/Users/sea084/Dropbox/RossRCode/Git/APIDev'
}else{
  apiDevRootDir <<- '/srv/plumber/APIDev'
  conPath <<- '/mnt/data/RegionalSoilMoisture/sfs.db'
  sfsDatRoot <<- '/mnt/data'
}


source(paste0(apiDevRootDir, '/apiDev_Config.R'))


machineName <- as.character(Sys.info()['nodename'])

#* @apiTitle Ross' API Development Area
#* @apiDescription Some web service endpoints to demonstrate some concepts



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

#* Returns Am image of all the soil moisture map

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
    spp <- plot(r)
    
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









