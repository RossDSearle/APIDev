library(raster)
library(DBI)
library(xts)
library(lubridate)
library(Cubist)
library(ranger)
library(hexbin)


apiDevRootDir <- 'C:/Users/sea084/Dropbox/RossRCode/Git/apiDev'
dataRoot <- 'C:/Projects/SMIPS/SFS/regionalSM/data'


source(paste0(apiDevRootDir, '/SFS/SFSMethods.R'))
source('C:/Users/sea084/Dropbox/RossRCode/myFunctions/GeneralUtils.R')
source('C:/Users/sea084/Dropbox/RossRCode/myFunctions/ModelUtils.R')
source('C:/Users/sea084/Dropbox/RossRCode/NAWRA/Production/RandomForestUtils_V2.R')

conPath <<- 'C:/Projects/SMIPS/SFS/sfs.db'
region <- 'SFS'

regions <- read.csv(paste0(apiDevRootDir, '/SFS/SM_Regions.csv'), stringsAsFactors = F)
reg <- regions[regions$Region==region,]

sql <- 'SELECT Sites.SiteID, Max(DataStore.dt) AS MaxOfdt, Min(DataStore.dt) AS MinOfdt
FROM (Sites INNER JOIN Sensors ON Sites.ID = Sensors.device) INNER JOIN DataStore ON (Sensors.device = DataStore.device) AND (Sensors.id = DataStore.id)
GROUP BY Sites.SiteID;'

#dateRanges  <- doq(sql)

DBStartDate <- as.Date('2013-01-02')
DBEndDate <- as.Date('2017-03-01')
DBEndDate - DBStartDate


sDate <- as.Date('2015-11-19')
eDate <- as.Date('2017-03-01')
dts <- seq.Date(sDate, eDate, 1)
nDays <- as.numeric(eDate - sDate)

### Download the SMIPS rasters clipped to the region of interest from the WCS end point
for (i in 1:length(dts)) {
  print(i)
  dt <- dts[i]
  yr<-str_sub(dt, 1,4)
  xsub <- paste0('&SUBSET=x(', reg$minx, ',', reg$maxx, ')')
  ysub <- paste0('&SUBSET=y(', reg$miny, ',', reg$maxy, ')')
  outfile = paste0(dataRoot, '/SmipsClips/CSIRO_Wetness-Index_', dt , '.tif')
  wcsUrl <- paste0('http://ternsoils.nexus.csiro.au/cgi-bin/mapserv.exe?map=e:/MapServer/SMIPS/moisture.map&SERVICE=WCS&VERSION=2.0.0&REQUEST=GetCoverage&COVERAGEID=CSIRO_Wetness-Index&FORMAT=image/tiff&mYear=', yr, '&mDate=', dt, '&mFName=CSIRO_Wetness-Index', xsub, ysub, '&FORMAT=image/tiff')
  download.file(wcsUrl, destfile = outfile, quiet = T, mode = 'wb')
}


### generate a moving average for the SMIPS Timeseries rasters
smipsClipIn <- paste0(dataRoot, '/SmipsClips')
smipsClips <- list.files(smipsClipIn, full.names = T, recursive = T, pattern = '.tif')

for(i in 1:length(smipsClips)){
  
  print(i)
 
  fname <- smipsClips[i+2]
  dt <-  dt <- str_remove( str_split(fname, '_')[[1]][3], '.tif')
  outname <- paste0(dataRoot, '/SmipsMovingAverage/', basename(fname))
  if(!file.exists(outname)){
  
      stk <- stack(smipsClips[i:(i+5)])
      r <- mean(stk)
      writeRaster(r, outname)
  }
}



#### Generate the ML training data
covsPath <- paste0(dataRoot, '/Covariates')
fls <- list.files(covsPath, full.names = T, recursive = T, pattern = '.tif')



sfsreg <- raster(paste0(dataRoot, '/Masks/SFS.tif'))
outdf <- data.frame(longitude=numeric(), latitude=numeric(), ProbeVal=numeric(), SmipsVal=numeric())

smipsAvgRoot <- paste0(dataRoot, '/SmipsMovingAverage')

startDate <- sDate + 2
endDate <- eDate - 2
dts <- seq.Date(startDate, endDate, 1)
nDays <- as.numeric(endDate - startDate)

for (i in 1:length(dts)) {
  
  print(i)
  dt <- dts[i]
  doy <- yday(dt)
  mth <- month(dt)
  season <- getSeasonAsNumeric(dt)
  fpath <- paste0(smipsAvgRoot, '/CSIRO_Wetness-Index_', dt ,'.tif' )
 
  r <- raster(fpath)
  cr <- crop(r, sfsreg)
  stk <- stack(c(r, fls))
  
  sql <-  paste0('SELECT Sites.SiteID, Sensors.device, Sites.SiteName, Sites.Longitude, Sites.Latitude, Sensors.Depth, DataStore.dt, Avg(DataStore.value) AS SM 
                  FROM (Sites INNER JOIN Sensors ON Sites.ID = Sensors.device) INNER JOIN DataStore ON (Sensors.id = DataStore.id) AND (Sensors.device = DataStore.device)
                  WHERE dt Between "', dt, '" And "', as.Date(dt) + 1 ,'"
                  GROUP BY Sites.SiteID, Sensors.device, Sensors.Depth;')
  probeData  <- doq(sql)

  coordinates(probeData) <-  ~Longitude+Latitude
  crs(probeData) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  pts <- extract(stk, probeData, df=T)
  names(pts)[2] <- 'SMIPS'
  
  smipsDrill <- data.frame(sid=probeData$SiteID, dt = dt, longitude=probeData@coords[,1], latitude=probeData@coords[,2], ProbeVal=probeData@data$SM, Depth=probeData$Depth, pts[,-1], doy, mth, season )
  
  outdf <- rbind(outdf, smipsDrill)
  
}


workDir <- paste0(dataRoot, '/ML')

write.csv(outdf, paste0(workDir, '/SmipsSmoothDrill.csv'), row.names=F)
modDf <- read.csv(paste0(workDir, '/SmipsSmoothDrill.csv'))

modelling.samplePercentage = 70


splitSamples <-createTrainingSample(modDf, 1, modelling.samplePercentage)
trainSet <- as.data.frame(splitSamples[1])
validSet <- as.data.frame(splitSamples[2])
colnames(trainSet) <- colnames(modDf)
colnames(validSet) <- colnames(modDf)
#createDirectory(paste0(rootDir,'/Cubist'))
write.table(trainSet, paste0(workDir, '/trainingData.csv'), sep=",", row.names=F)
write.table(validSet, paste0(workDir, '/validationData.csv'), sep=",", row.names=F)

trainSet <- read.csv(paste0(workDir, '/trainingData.csv'), stringsAsFactors = F)
validSet <- read.csv(paste0(workDir, '/validationData.csv'), stringsAsFactors = F)
###   Generate a cubist model

y <- trainSet$ProbeVal
x <- trainSet[6:ncol(trainSet)]

Cmodel <- cubist(x = x , y = y, committees=10, cubistControl( rules = 500, extrapolation = 100))
summary(Cmodel)

CmodelFilename <- paste0(workDir, '/model_Cubist')
saveRDS(Cmodel, paste0(CmodelFilename, '.rds'))

Cmodel <- readRDS(paste0(CmodelFilename, '.rds'))

Coutfilename <-paste0(CmodelFilename, '.rules')
file.create(Coutfilename)
modelText <- summary(Cmodel)
writeLines(modelText$output, Coutfilename)
xp <- validSet[2:ncol(validSet)]
yp <- validSet$ProbeVal
Cmv <- predict(Cmodel, xp)
Ctdf <- data.frame(yp, Cmv)
fitStats(Ctdf[1],Ctdf[2], paste0("Soil Moisture"),  paste0(Coutfilename, '_ModelStats.txt'), 'topleft', verbose = T)
write.table(Ctdf, paste0(Coutfilename, '_Obs_vs_Mod.csv'), sep=",", row.names=F)
hexbinplot( Ctdf$Cmv ~ Ctdf$yp, colramp= function(n){LinOCS(n,beg=15,end=225)})
hexbinplot( Ctdf$Cmv ~ Ctdf$yp, colramp= function(n){BTC(n,beg=15,end=225)})
hexbinplot( Ctdf$Cmv ~ Ctdf$yp, colramp= function(n){magent  (n,beg=15,end=225)})
hexbinplot( Ctdf$Cmv ~ Ctdf$yp, colramp= function(n){plinrain(n,beg=15,end=225)})
colfunc <- colorRampPalette(c("brown", 'lightyellow', "darkblue"))
col=colfunc(20)
hexbinplot( Ctdf$Cmv ~ Ctdf$yp, colramp=function(n){colfunc(20)})

#### Generate a RF model
Rmodel <- ranger(y ~ ., data = x, write.forest = TRUE, importance = 'impurity', num.trees = 100)
RmodelPath = paste0(workDir, '/RFmodel.rds')
saveRDS(Rmodel,RmodelPath )
summariseRFModel( RmodelPath, "SMIPS")
#imp <- model$variable.importance[order(model$variable.importance, decreasing = T)]
#imp
xp <- validSet[2:ncol(validSet)]
Rmv = predict(Rmodel, data=xp,  predict.all = F)
yp <- validSet$ProbeVal



Rtdf <- data.frame(yp, Rmv$predictions)
hexbinplot( Rtdf$Rmv.predictions ~ Rtdf$yp)
           

RmodelFilename <- paste0(workDir, '/', 'SMIPS', '_model_Ranger')
Coutfilename <-paste0(RmodelFilename, '.rules')
fitStats(Rtdf[1],Rtdf[2], paste0('SMIPS', ' Ranger'),  paste0(RmodelFilename, '_ModelStats.txt'), 'topleft', verbose = T)
write.table(Rtdf, paste0(RmodelFilename, '_Obs_vs_Mod.csv'), sep=",", row.names=F)



#### Validations 

sql <- 'select * from Sites'
siteData  <- doq(sql)
depth = 300
#outDir <-  paste0(tsOutDir, '/',depth)
obsDir <- paste0(dataRoot, '/TS/', depth)
if(!dir.exists( obsDir)) {dir.create(obsDir, recursive = T)}
for (i in 1:nrow(siteData)) {
  print(i)
  rec <- siteData[i,]
  sid <- rec$SiteID
  
  sql <-  paste0('SELECT DataStore.dt, Avg(DataStore.value) AS SM
                  FROM (Sites INNER JOIN Sensors ON Sites.ID = Sensors.device) INNER JOIN DataStore ON (Sensors.id = DataStore.id) AND (Sensors.device = DataStore.device)
                  WHERE SiteID = "',sid , '" and Depth = ', depth, ' and ( dt Between "', sDate , '" and "', eDate + 1 ,'")
                  GROUP BY Sites.SiteID, Sensors.device, Sites.SiteName, date(DataStore.dt)
                  ORDER By date(DataStore.dt)')
  
  tsdf  <- doq(sql)
  tail(tsdf)
  
  if(nrow(tsdf) > 0){
    ts <- xts(tsdf[,-1], order.by=as.Date(tsdf[,1]))
    write.csv(tsdf, paste0(obsDir, '/', sid, '_', depth, '.csv'))
    saveRDS(ts, paste0(obsDir, '/', sid, '_', depth, '.rds'))
    plot(ts)
  }
  
}



depth = 300
outDir <- paste0(dataRoot, '/ObsVmod')

for (i in 1:nrow(siteData)) {
  
  print(i)
  rec <- siteData[i,]
  sid <- rec$SiteID
  
  obsfile <- paste0(obsDir, '/', sid, '_', depth, '.rds') 

  if(file.exists(obsfile)){
      x <- modDf[validSet$Depth == depth & modDf$sid == sid, 6:ncol(modDf) ]
      y <- predict(Cmodel, x)
      obsVals <- modDf[modDf$Depth == depth & modDf$sid == sid, 5 ]
      df <- data.frame(obs=obsVals, modVals=y)
      plot(df)
      fitStats(df[1],df[2], paste0(outDir, '/', sid, '_', depth, '_ModelStats.txt'), 'topleft', verbose = T)
      write.csv(df, paste0(outDir, '/', sid, '_', depth, '.csv'))
  }
}



##   Make lots of maps  ######
depth <- 300
smipsAvgRoot <- paste0(dataRoot, '/SmipsMovingAverage')
templateR <- raster(paste0(dataRoot, '/Masks/SFS.tif'))
rasterOut <- paste0(dataRoot, '/Rasters/', depth)
if(!dir.exists( rasterOut)) {dir.create(rasterOut, recursive = T)}

for (i in 1:length(dts)) {
  
  print(i)
  dt <- dts[i]
  doyVal <- yday(dt)
  mthVal <- month(dt)
  seasonVal <- getSeasonAsNumeric(dt)
  
  fpath <- paste0(smipsAvgRoot, '/CSIRO_Wetness-Index_', dt ,'.tif' )
  smipsR <- raster(fpath)
  names(smipsR) <- 'SMIPS'
  
  doy <- templateR
  doy[] <- doyVal
  names(doy) <- 'doy'
  mth <- templateR
  mth[] <- mthVal
  names(mth) <- 'mth'
  season <- templateR
  season[] <- seasonVal
  names(season) <- 'season'
  depthR<- templateR
  depthR[] <- depth
  names(depthR) <- 'Depth'
  
  predStk <- stack(c(depthR, smipsR, fls, doy, mth, season))
  predStk[is.na(predStk[])] <- -1
  #map <- predict( Rmodel, as.data.frame(as.matrix(predStk)))
  
  pdf <- as.data.frame(as.matrix(predStk))
  map <- predict( Cmodel, pdf)
  
  outR <- templateR
  #outR[] <- map$predictions
  outR[] <- map
  outName <- paste0(rasterOut, '/regSM_', dt , '.tif')
  outRc <- mask(outR, templateR, filename = outName, overwrite=T)
  
  plot(outRc)
  
  
}



