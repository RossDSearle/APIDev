library(stringr)
library(DBI)
library(RSQLite)
library(xts)
library(gstat)
library(RCurl)
library(raster)



regions <- read.csv(paste0(apiDevRootDir, '/SFS/SM_Regions.csv'), stringsAsFactors = F)



doq <- function(sql){
  con <- dbConnect(RSQLite::SQLite(), conPath, flags = SQLITE_RO)
  qry <- dbSendQuery(con, sql)
  res <- dbFetch(qry)
  dbClearResult(qry)
  #print(head(res))
  dbDisconnect(con)
  return(res)
}

# region <- 'SFS'
# dt <- '2016-10-01'
# r <-raster(smipsPath)
# smR <- raster(outMapPath)
# plot(smR)

getRegionalSMMap <- function(region, dt){
  
  reg <- regions[regions$Region==region,]

  smipsPath <- getSmips(reg, dt)
  smipsR <- raster(smipsPath)
  
   probeData <- getProbeDataForaDate(dt)
   
   probePts <- drillProbeLocations(probeData, smipsR)

  outMapPath <- krigMap(probePts, smipsR)

  unlink(smipsPath)

  return(outMapPath)

}



drillProbeLocations <- function(probeData, smipsR){
  
  #print(smipsR)
  coordinates(probeData) <-  ~Longitude+Latitude
  crs(probeData) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  probeData <- probeData[-zerodist(probeData)[,1],]
  #plot(probeData)
  pts <- extract(smipsR, probeData)
  smipsDrill <- data.frame(probeData@coords[,1], probeData@coords[,2], probeData@data$SM, pts)
  probeData@data$smips <- pts
  #print(head(probeData))
  return(probeData)
}
  

krigMap <- function(probePts, smipsR){
  
  #mod1 = lm(probePts$SM ~ probePts$smips, data = probePts)
  #summary(mod1)
  
  #v <- suppressWarnings(automap::autofitVariogram(SM ~ smips,probePts))
  v <- automap::autofitVariogram(SM ~ smips,probePts)

  #mMod <-  suppressWarnings(gstat(NULL, "moist", SM ~ smips, probePts , model = v$var_model))
  mMod <-  gstat(NULL, "moist", SM ~ smips, probePts , model = v$var_model)
  
  names(smipsR) <- 'smips'
  
  outFilePath <- paste0(apiDevRootDir, '/SFS/tmp/',  basename(tempfile()), '.tif')
  #sfsMap <- suppressWarnings( interpolate(smipsR, mMod, xyOnly = FALSE, index = 1,format="GTiff",overwrite=T))
  sfsMap <- raster::interpolate(smipsR, mMod, xyOnly = FALSE, index = 1,format="GTiff",overwrite=T)
  #plot(sfsMap)
  wrMsk <- raster(paste0(apiDevRootDir, '/SFS/Masks/SFS.tif'))
  
  om1 <- mask(sfsMap,wrMsk)
  om2 <- clamp(om1, lower=0, upper=100)
  writeRaster(om2, outFilePath)
  return(outFilePath)
}
  

getSmips <- function(reg, dt){
  
  yr<-str_sub(dt, 1,4)
  xsub <- paste0('&SUBSET=x(', reg$minx, ',', reg$maxx, ')')
  ysub <- paste0('&SUBSET=y(', reg$miny, ',', reg$maxy, ')')
  outfile = paste0(apiDevRootDir, '/SFS/tmp/',  basename(tempfile()), '.tif')
  wcsUrl <- paste0('http://ternsoils.nexus.csiro.au/cgi-bin/mapserv.exe?map=e:/MapServer/SMIPS/moisture.map&SERVICE=WCS&VERSION=2.0.0&REQUEST=GetCoverage&COVERAGEID=CSIRO_Wetness-Index&FORMAT=image/tiff&mYear=', yr, '&mDate=', dt, '&mFName=CSIRO_Wetness-Index', xsub, ysub, '&FORMAT=image/tiff')
  download.file(wcsUrl, destfile = outfile, quiet = T, mode = 'wb')
  
  #wi <- raster(outfile)
  #crs(wi)
  #plot(wi)
  return(outfile)
}





getProbeDataForaDate <- function(dt){
  
  sql <-  paste0('SELECT Sites.SiteID, Sensors.device, Sites.SiteName, Sites.Longitude, Sites.Latitude, DataStore.dt, Max(DataStore.value) AS SM 
                  FROM (Sites INNER JOIN Sensors ON Sites.ID = Sensors.device) INNER JOIN DataStore ON (Sensors.id = DataStore.id) AND (Sensors.device = DataStore.device)
                  WHERE  Sensors.Depth=100 and dt Between "', dt, '" And "', as.Date(dt) + 1 ,'"
                  GROUP BY Sites.SiteID, Sensors.device, Sites.SiteName, Sites.Longitude, Sites.Latitude;')
  df  <- doq(sql)
  return(df)
}





getSeason <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Summer",
          ifelse (d >= SE & d < SS, "Fal",
                  ifelse (d >= SS & d < FE, "Winter", "Springl")))
}

getSeasonAsNumeric <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, 4,
          ifelse (d >= SE & d < SS, 1,
                  ifelse (d >= SS & d < FE, 2, 3)))
}

