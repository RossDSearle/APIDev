library(stringr)
library(DBI)
library(RSQLite)
library(xts)

regions <- read.csv(paste0(apiDevRootDir, '/SFS/SM_Regions.csv'), stringsAsFactors = F)


doq <- function(sql){
  qry <- dbSendQuery(con, sql)
  res <- dbFetch(qry)
  dbClearResult(qry)
  #print(head(res))
  return(res)
}


getRegionalSMMap <- function(region, dt){
  
  reg <- regions[regions$Region==region,]
  
  smipsR <- getSmips(reg, dt)
  
  probeData <- getProbeDataForaDate(dt)
  
  probePts <- drillProbeLocations(probeData, smipsR)
  
  outMapPath <- krigMap(probePts, smipsR)
  
  return(outMapPath)
}



drillProbeLocations <- function(probeData, smipsR){
  
  coordinates(probeData) <-  ~Longitude+Latitude
  crs(probeData) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  plot(probeData)
  pts <- extract(smipsR, probeData)
  smipsDrill <- data.frame(probeData@coords[,1], probeData@coords[,2], probeData@data$SM, pts)
  probeData@data$smips <- pts
  return(probeData)
}
  

krigMap <- function(probeData, smipsR){
  
  mod1 = lm(probeData$SM ~ probeData$smips, data = probeData)
  summary(mod1)
  
  v <- automap::autofitVariogram(SM ~ smips,probeData)

  mMod <- gstat(NULL, "moist", SM ~ smips, probeData , model = v$var_model)
  
  names(smipsR) <- 'smips'
  
  outFilePath <- paste0(apiDevRootDir, '/SFS/tmp/',  basename(tempfile()), '.tif')
  sfsMap <- interpolate(smipsR, mMod, xyOnly = FALSE, index = 1,format="GTiff",overwrite=T)
  wrMsk <- raster(paste0(apiDevRootDir, '/SFS/Masks/SFS.tif'))
  
  om <- mask(sfsMap,wrMsk, filename=outFilePath)
  return(outFilePath)
}
  

getSmips <- function(reg, dt){
  
  yr<-str_sub(dt, 1,4)
  xsub <- paste0('&SUBSET=x(', reg$minx, ',', reg$maxx, ')')
  ysub <- paste0('&SUBSET=y(', reg$miny, ',', reg$maxy, ')')
  outfile = paste0( 'C:/Projects/SMIPS/SFS/regionalSM/wetnessIndex/wi_', dt, '.tif' )
  wcsUrl <- paste0('http://ternsoils.nexus.csiro.au/cgi-bin/mapserv.exe?map=e:/MapServer/SMIPS/moisture.map&SERVICE=WCS&VERSION=2.0.0&REQUEST=GetCoverage&COVERAGEID=CSIRO_Wetness-Index&FORMAT=image/tiff&mYear=', yr, '&mDate=', dt, '&mFName=CSIRO_Wetness-Index', xsub, ysub, '&FORMAT=image/tiff')
  download.file(wcsUrl, destfile = outfile, quiet = T, mode = 'wb')
  
  wi <- raster(outfile)
  crs(wi)
  plot(wi)
  return(wi)
}





getProbeDataForaDate <- function(dt){
  
  sql <-  paste0('SELECT Sites.SiteID, Sensors.device, Sites.SiteName, Sites.Longitude, Sites.Latitude, DataStore.dt, Max(DataStore.value) AS SM 
                  FROM (Sites INNER JOIN Sensors ON Sites.ID = Sensors.device) INNER JOIN DataStore ON (Sensors.id = DataStore.id) AND (Sensors.device = DataStore.device)
                  WHERE  Sensors.Depth=100 and dt Between "', dt, '" And "', as.Date(dt) + 1 ,'"
                  GROUP BY Sites.SiteID, Sensors.device, Sites.SiteName, Sites.Longitude, Sites.Latitude;')
  df  <- doq(sql)
  return(df)
}