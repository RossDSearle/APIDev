library(raster)
library(sf)
library(geojsonio)
library(spex)
library(dplyr)
library(tictoc)
library(clhs)


# 
# if(machineName == 'FANCY-DP'){
#   apiDevRootDir <<- 'C:/Users/sea084/Dropbox/RossRCode/Git/apiDev'
# }else{
#   apiDevRootDir <<- '/srv/plumber/apiDev'
# }

AgXRootDir <- paste0(apiDevRootDir, '/AgX/Data')


mapDesc <-  read.csv(paste0(AgXRootDir, '/GenericSoilMap/NationalGenericSoilGroupsLegend.csv'))
shapePath <- paste0(AgXRootDir, '/GenericSoilMap/paddocks.shp')
bdy <- read_sf(shapePath)
fsumPath <- paste0(AgXRootDir, '/GenericSoilMap/soil_forestclassifier_most_likely.tif' )


getSoilMapBoundaries <- function(){
  
 
  
  r <- raster(fsumPath)
  e <- raster::extent(bdy)
  rc <- raster::crop(r,extent( e@xmin, e@xmax, e@ymin, e@ymax)  )
  
   tic()
     rawPolys <- polygonize(rc)
     polys <- rawPolys %>% group_by(soil_forestclassifier_most_likely) %>% 
       summarise(m = mean(soil_forestclassifier_most_likely)) %>% 
      st_cast() 
     
    outpolys <-  merge(polys, mapDesc, by.x='soil_forestclassifier_most_likely', by.y='ID')
    outpolys2 <- outpolys[, c(1,7)]
    colnames(outpolys2) <- c('Soil_ID', 'Description', 'geometry')
    outpolys3 <-  raster::intersect(as(outpolys2, 'Spatial'), as(bdy, 'Spatial'))
    outpolys4 <-  as(outpolys3, 'sf')
    outpolys5 <- outpolys4[, c(1, 2, 4)]
    colnames(outpolys5) <-  c('Soil_ID', 'Description', 'paddockName', 'geometry')
    
    outpolys5$Description <-  droplevels( outpolys5$Description )

   toc()
   
  return(outpolys5)
}



getSampleLocations <- function(NoSamples){
  
  paths = list.files(  paste0(AgXRootDir, '/OptimisedSamples'), pattern = 'KOOKABURRA', full.names = T, recursive =T)
  #paths2 <- paths[grepl('ClippedRasters', paths, ignore.case = T)]
  #paths3 <- paths2[grepl('DryYield', paths2, ignore.case = T)]

  stk <- stack(paths)
  df <- as.data.frame(stk[])

  res <- clhs(df, size = NoSamples, iter = 500, progress = FALSE, simple = F )
  locs <- as.data.frame(xyFromCell(raster(paths[1]), res$index_samples))
  coordinates(locs) <-  ~x+y
  crs(locs) <- CRS("+proj=longlat +datum=WGS84")
   locsSf <- as(locs, 'sf')
   locsSf$SampleId <- c(1:nrow(locsSf))
   return(locsSf)
  
}








