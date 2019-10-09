library(stringr)
library(DBI)
library(RSQLite)
library(xts)
library(gstat)
library(RCurl)
library(raster)
library(lubridate)
library(ranger)

rasterOptions(datatype="FLT4S", timer=TRUE, format='GTiff',progress="text",chunksize=1e+08,maxmemory=1e+09, overwrite=TRUE) # maxmemory = max no of cells to read into memory



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









######  CSIRO Thredds serve info  ########################
CSIRO_OpenDAP_Server <- 'http://esoil.io/thredds/dodsC/SMIPS/SMIPSv0.5.nc'
originDay = 42326
originDate <- '2015-11-20'
Ausminx <- 112.905
Ausminy <-  -43.735
Ausmaxx <- 154.005
Ausmaxy <- -9.005
AusRes <- 0.01
Ausnumrows <- 3474
Ausnumcols <- 4110


###### constants for model evaluation data
covsPath <-  paste0(sfsDatRoot, '/SFS/CovariatesNoNa')
covfls <- list.files(covsPath, full.names = T, recursive = T, pattern = '.tif')
predStk <- stack(c(covfls))
pdf1 <- as.data.frame(as.matrix(predStk))
Rmodel <- readRDS(paste0(sfsDatRoot, '/SFS/ML/RFmodel.rds'))
templateRSMIPS <- raster(paste0(sfsDatRoot, '/SFS/Masks/SFS.tif'))

templateR <- raster(nrows=Ausnumrows, ncols=Ausnumcols, xmn=Ausminx, xmx=Ausmaxx, ymn=Ausminy, ymx=Ausmaxy, crs=CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'))

#dt <- '2018-09-25'


getcellsForALatLon <- function(lon, lat){
  cell <- cellFromXY(templateR, cbind(c(lon), c(lat)))
  colNum <- colFromCell(templateR, cell)
  rowNum <- rowFromCell(templateR, cell)
  return(data.frame(colNum=colNum, rowNum=rowNum))
}






getSMIPSrasterCSIRO_OpenDAP <- function(reg, product, dt){
  
  minx =  reg$minx
  miny =  reg$miny
  maxx =  reg$maxx
  maxy =  reg$maxy

  
  xext = maxx - minx
  yext = maxy - miny
  
  #stridex <- ceiling(xext / ( AusRes * wmsnumcols))
  # stridey <- ceiling(yext / ( AusRes * wmsnumrows))
  
  stridey = 1
  
  ll <- getcellsForALatLon(minx, miny)
  ur <- getcellsForALatLon(maxx, maxy)
  
  subcols <- ceiling( c((ur$colNum-1) - ll$colNum) / stridey )
  subrows <- ceiling( c((ll$rowNum-1) - ur$rowNum) / stridey )
  
  dayNum = as.numeric(as.Date(paste(dt), "%Y-%m-%d") - as.Date(paste(originDate), "%Y-%m-%d"))
  
  url <- paste0('http://esoil.io/thredds/dodsC/SMIPSall/SMIPSv0.5.nc.ascii?',product, '%5B',dayNum ,'%5D%5B', ur$rowNum-1, ':', stridey, ':', ll$rowNum-1, '%5D%5B', ll$colNum-1, ':', stridey, ':', ur$colNum-1, '%5D')
  
  #url <- paste0('http://esoil.io/thredds/dodsC/SMIPSall/SMIPSv0.5.nc.ascii?',product,%5B1%5D%5B0:1:10%5D%5B0:1:10%5D
  #  url <-  "http://esoil.io/thredds/dodsC/SMIPSall/SMIPSv0.5.nc.ascii?Openloop_Wetness_Index%5B0:1:0%5D%5B0:1:0%5D%5B0:1:0%5D" 
  #  http://esoil.io/thredds/dodsC/SMIPSall/SMIPSv0.5.nc
  print(url)
  
  d1 <- getURI(url)
  
  odData1 <- read.table(text=d1, skip=12, nrows = subrows , sep = ',')
  odData2 <- odData1[,-1]
  m1 <- as.matrix(odData2)
  
  r <- raster(nrows=nrow(odData2), ncols=ncol(odData2), xmn=minx, xmx=maxx, ymn=miny, ymx=maxy, crs=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),  vals=m1)
  
  #outfile = paste0(apiDevRootDir, '/SFS/tmp/',  basename(tempfile()), '.tif')
  
  return(r)
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









# region='SFS'
# dt='2019-07-14'
# theDepth=600
##########################    ML  Approaches   ########################

getRegionalSMMap2 <- function(region, dt, depth){
  
  reg <- regions[regions$Region==region,]
  product='Openloop_Wetness_Index'
  
  smipsR <- getSMIPSrasterCSIRO_OpenDAP(reg, product, dt)
  
  # smipsPath <- getSmips(reg, dt)
  #  smipsR <- raster(smipsPath)
  
  #probeData <- getProbeDataForaDate2(dt, reg$Region)
  #probePts <- drillProbeLocations(probeData, smipsR)
  #outMapPath <- krigMap(probePts, smipsR)
  
  outMapPath <- MLMap(dt, depth, smipsR)
  #unlink(smipsPath)
  
  return(outMapPath)
  
}

MLMap <- function( dt, theDepthVal, smipsR){
  
  dt <- as.Date(dt)
  doyVal <- yday(dt)
  mthVal <- month(dt)
  seasonVal <- getSeasonAsNumeric(dt)
  
  smipsR[is.na(smipsR[])] <- 0
  names(smipsR) <- 'SMIPSVal'
  smipsR <- resample(smipsR, templateRSMIPS)
  
  depthR <- templateRSMIPS
  depthR[] <- theDepthVal
  names(depthR) <- 'Depth'
  doy <- templateRSMIPS
  doy[] <- doyVal
  names(doy) <- 'doy'
  mth <- templateRSMIPS
  mth[] <- mthVal
  names(mth) <- 'mth'
  season <- templateRSMIPS
  season[] <- seasonVal
  names(season) <- 'season'
  
  predStk2 <- stack(c(smipsR, doy, mth, season, depthR))
  pdf2 <- as.data.frame(as.matrix(predStk2))
  pdf <- cbind(pdf1,pdf2)
  
  map <- predict( Rmodel, pdf)
  
  outR <- templateRSMIPS
  outR[] <- map$predictions
  outName <- paste0(apiDevRootDir, '/SFS/tmp/',  basename(tempfile()), '.tif')
  outRc <- mask(outR, templateRSMIPS, filename = outName, overwrite=T)
  
  plot(outRc)
  
  return(outName)
  
}


getProbeDataForaDate2 <- function(dt, region, depth){
  
  aoi <- '-38.5;141;-37;144.5'
  startDate <- '2018-07-14T00:00:00'
  endDate <-  '2018-07-14T23:59:59'
  dataType <- 'Soil-Moisture'
  
  
  # make a map of all sensor locations
  #url <- paste0(server,'/SensorAPI/getSensorLocations?usr=', sfUsr, '&pwd=', sfPwd)
  url <- paste0(server,'/SensorAPI/getSensorLocations?usr=', sfUsr, '&pwd=', sfPwd, '&bbox=', aoi)
  sensorsJson <- getURL(url)
  sensDF <- fromJSON(sensorsJson)
  sensDF <- sensDF[sensDF$SensorGroup == region,]
  
  outDF <- data.frame()
  
  for (i in 1:nrow(sensDF)) {
    print(i)
    
    id <- sensDF$SiteID[i]
    url <- paste0(server, '/SensorAPI/getSensorDataStreams?usr=', sfUsr, '&pwd=', sfPwd, '&siteid=', id,  '&sensortype=', dataType, '&startdate=', startDate, '&enddate=', endDate)
    DSJson <- getURL(url)
    sensDS <- fromJSON(DSJson)
    
    if(is.null(sensDS$error)){
      sdf <- data.frame() 
      
      
      
      for (j in 1:nrow(sensDS)) {
        rec <- sensDS[j,]
        rdf <- data.frame(SiteID=id, Longitude=rec$Longitude, Latitude=rec$Latitude, Depth=rec$UpperDepthCm, dt=rec$DataStream[[1]]$t, val=rec$DataStream[[1]]$v )
        sdf <- rbind(sdf, rdf)
        Sys.sleep(1)
        
      }
      
      write.csv(sdf, paste0(rootDir, '/ProbeDumps/SM_', id, '.csv'))
      outDF <- rbind(outDF, sdf)
    }else{
      print(paste0('ERROR : ', id))
    }
    
  }
  
}


