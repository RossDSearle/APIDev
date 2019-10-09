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


region <- 'SFS'
version <- '3'

regions <- read.csv(paste0(apiDevRootDir, '/SFS/SM_Regions.csv'), stringsAsFactors = F)
reg <- regions[regions$Region==region,]

df <- read.csv(paste0(dataRoot, version,'/ProbeDumps/SM_All.csv'), stringsAsFactors = F)
summary(df)
nrow(df)
df2 <- df[df$val >= 1, ] 
nrow(df2)
df3 <- df2[!is.na(df2$val), ] 
nrow(df3)
write.csv(df3, paste0(dataRoot, version,'/ProbeDumps/SM_All_tidy.csv') )
plot(df3$dt, df3$val)
hist(df3$val)
summary(df3)
probeSM <- read.csv(paste0(dataRoot, version,'/ProbeDumps/SM_All_tidy.csv'))



sDate <- as.Date('2015-11-20')
#sDate <- as.Date('2019-06-08')
eDate <- as.Date('2019-10-08')
dts <- seq.Date(sDate, eDate, 1)
nDays <- as.numeric(eDate - sDate)

### Download the SMIPS rasters clipped to the region of interest from the WCS end point
# for (i in 1:length(dts)) {
#   print(i)
#   dt <- dts[i]
#   yr<-str_sub(dt, 1,4)
#   xsub <- paste0('&SUBSET=x(', reg$minx, ',', reg$maxx, ')')
#   ysub <- paste0('&SUBSET=y(', reg$miny, ',', reg$maxy, ')')
#   outfile = paste0(dataRoot, '/SmipsClips/CSIRO_Wetness-Index_', dt , '.tif')
#   wcsUrl <- paste0('http://ternsoils.nexus.csiro.au/cgi-bin/mapserv.exe?map=e:/MapServer/SMIPS/moisture.map&SERVICE=WCS&VERSION=2.0.0&REQUEST=GetCoverage&COVERAGEID=CSIRO_Wetness-Index&FORMAT=image/tiff&mYear=', yr, '&mDate=', dt, '&mFName=CSIRO_Wetness-Index', xsub, ysub, '&FORMAT=image/tiff')
#   download.file(wcsUrl, destfile = outfile, quiet = T, mode = 'wb')
# }

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

#dt <- '2018-09-25'

templateR <- raster(nrows=Ausnumrows, ncols=Ausnumcols, xmn=Ausminx, xmx=Ausmaxx, ymn=Ausminy, ymx=Ausmaxy, crs=CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'))

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
  #print(url)
  
  d1 <- getURI(url)
  
  odData1 <- read.table(text=d1, skip=12, nrows = subrows , sep = ',')
  odData2 <- odData1[,-1]
  m1 <- as.matrix(odData2)
  
  r <- raster(nrows=nrow(odData2), ncols=ncol(odData2), xmn=minx, xmx=maxx, ymn=miny, ymx=maxy, crs=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),  vals=m1)
  
  #outfile = paste0(apiDevRootDir, '/SFS/tmp/',  basename(tempfile()), '.tif')
  
  return(r)
}

clipSMIPS <- function(reg, product, dt ){
  r <- getSMIPSrasterCSIRO_OpenDAP(reg, product, dt )
  smipsR <- resample(r, templateRSFS)
  outfile = paste0(dataRoot, version, '/SmipsClips/CSIRO_Wetness-Index_', dt , '.tif')
  plot(smipsR)
  writeRaster(smipsR,outfile, overwrite=T)
}

#source('C:/Users/sea084/Dropbox/RossRCode/Git/APIDev/SFS/SFSMethods.R')
product='Openloop_Wetness_Index'
templateRSFS <- raster(paste0(apiDevRootDir, '/SFS/Masks/SFS.tif'))

outDir = paste0(dataRoot, version, '/SmipsClips/')
if(!dir.exists(outDir)){dir.create(outDir)}

for (i in 1:length(dts)) {
  print(i)
  dt <- dts[i]
  outfile = paste0(dataRoot, '3/SmipsClips/CSIRO_Wetness-Index_', dt , '.tif')
  
  if(!file.exists(outfile)){
    try(clipSMIPS(reg, product, dt ), silent = TRUE)
   
  }
}

### generate a moving average for the SMIPS Timeseries rasters
smipsClipIn <- paste0(dataRoot, version, '/SmipsClips')
smipsClips <- list.files(smipsClipIn, full.names = T, recursive = T, pattern = '.tif')
outDir <- paste0(dataRoot, version, '/SmipsMovingAverage')
if(!dir.exists(outDir)){dir.create(outDir)}
for(i in 1:length(smipsClips)){
  
  print(i)
 
  fname <- smipsClips[i+2]
  dt <-  dt <- str_remove( str_split(fname, '_')[[1]][3], '.tif')
  outname <- paste0(outDir, '/', basename(fname))
  if(!file.exists(outname)){
  
      stk <- stack(smipsClips[i:(i+5)])
      r <- mean(stk)
      writeRaster(r, outname)
  }
}



#### Generate the ML training data
covsPath <- paste0(dataRoot, '/Covariates')
covfls <- list.files(covsPath, full.names = T, recursive = T, pattern = '.tif')



sfsreg <- raster(paste0(dataRoot, '/Masks/SFS.tif'))
#outdf <- data.frame(longitude=numeric(), latitude=numeric(), ProbeVal=numeric(), SmipsVal=numeric())



startDate <- sDate + 2
endDate <- eDate - 2
dts <- seq.Date(startDate, endDate, 1)
nDays <- as.numeric(endDate - startDate)

##### Drill the covariates stack at the site locations 

df <- data.frame(probeSM$SiteID, probeSM$Longitude, probeSM$Latitude, row.names = NULL)
sites <- unique(df)
colnames(sites) <- c('sid', 'lon', 'lat')
coordinates(sites) <-  ~lon+lat
crs(sites) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
plot(sites)

covstk <- stack(covfls)
pts <- extract(covstk, sites, df=T)
covsDrill <- data.frame(sites$sid, pts[,-1])
names(covsDrill)[1] <- 'sid'
head(covsDrill)

outDir <- paste0(dataRoot, version, '/Drills')
if(!dir.exists(outDir)){dir.create(outDir)}
write.csv(covsDrill, paste0(outDir, '/covsDrill.csv'), row.names = F)

# Drill the SMIPS Layers

smipsAvgRoot <- paste0(dataRoot, '/SmipsMovingAverage')
smipsfls <- list.files(smipsAvgRoot, full.names = T, recursive = T, pattern = '.tif')
#smipsstk <- stack(smipsfls[1:10])
#smipspts <- extract(smipsstk, sites, df=T)

simpsDrillDF <- data.frame()
for (i in 1:length(smipsfls)) {
  print(i)
  smipR <- raster(smipsfls[i])
  smipspts <- extract(smipR, sites, df=T)
  n1 <- str_remove( names(smipR), 'CSIRO_Wetness.Index_')
  dt <- str_replace_all(n1, '[.]', '-')
  outsmpDF <- data.frame(sites$sid, dt, smipspts[,2])
  colnames(outsmpDF) <- c('sid', 'dt', 'SMIPSVal')
  simpsDrillDF <- rbind(simpsDrillDF, outsmpDF)
}

colnames(simpsDrillDF) <- c('sid', 'dt', 'SMIPSVal')
write.csv(simpsDrillDF, paste0(dataRoot, version, '/Drills/smipsDrill.csv'), row.names = F)

covsnpDF <- merge(simpsDrillDF, covsDrill, 'sid')
head(covsnpDF)
sdf <- covsnpDF[order(covsnpDF$sid, covsnpDF$dt),] 
head(sdf)
write.csv(sdf, paste0(dataRoot, version, '/Drills/smipsAndCovsMerge.csv'), row.names = F)

str(probeSM)
probeData <- data.frame(sid=probeSM$SiteID, Depth=probeSM$Depth, dt=as.Date(probeSM$dt), probeVal=probeSM$val, stringsAsFactors = F)

csDF <- read.csv(paste0(dataRoot, version, '/Drills/smipsAndCovsMerge.csv'), stringsAsFactors = F)
csDF$dt <- as.Date(csDF$dt)
str(csDF)
tdf <- merge(probeData, csDF, by=c('sid', 'dt'))
otdf <- tdf[order(tdf$sid, tdf$dt, tdf$Depth),] 

otdf$doy <- yday(otdf$dt)
otdf$mth <- month(otdf$dt)
otdf$season <- getSeasonAsNumeric(otdf$dt)
otdf <- na.omit(otdf)
otdf2 <- otdf[c(1,2,4,3, 5:34)]  # rearrange the cols to make it easier to use in ML
write.csv(otdf2, paste0(dataRoot, version,  '/Drills/rawTraining.csv'), row.names = F)

# for (i in 1:length(dts)) {
#   
#   print(i)
#   dt <- dts[i]
#   doy <- yday(dt)
#   mth <- month(dt)
#   season <- getSeasonAsNumeric(dt)
#   fpath <- paste0(smipsAvgRoot, '/CSIRO_Wetness-Index_', dt ,'.tif' )
#  
#   r <- raster(fpath)
#   cr <- crop(r, sfsreg)
#   stk <- stack(c(r, fls))
#   
#   sql <-  paste0('SELECT Sites.SiteID, Sensors.device, Sites.SiteName, Sites.Longitude, Sites.Latitude, Sensors.Depth, DataStore.dt, Avg(DataStore.value) AS SM 
#                   FROM (Sites INNER JOIN Sensors ON Sites.ID = Sensors.device) INNER JOIN DataStore ON (Sensors.id = DataStore.id) AND (Sensors.device = DataStore.device)
#                   WHERE dt Between "', dt, '" And "', as.Date(dt) + 1 ,'"
#                   GROUP BY Sites.SiteID, Sensors.device, Sensors.Depth;')
#   probeData  <- doq(sql)
# 
#   coordinates(probeData) <-  ~Longitude+Latitude
#   crs(probeData) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#   pts <- extract(stk, probeData, df=T)
#   names(pts)[2] <- 'SMIPS'
#   
#   smipsDrill <- data.frame(sid=probeData$SiteID, dt = dt, longitude=probeData@coords[,1], latitude=probeData@coords[,2], ProbeVal=probeData@data$SM, Depth=probeData$Depth, pts[,-1], doy, mth, season )
#   
#   outdf <- rbind(outdf, smipsDrill)
#   
# }



workDir <- paste0(dataRoot, version, '/ML')
if(!dir.exists(workDir)){dir.create(workDir)}

modDf <- read.csv(paste0(dataRoot, version, '/Drills/rawTraining.csv'), stringsAsFactors = F)

modelling.samplePercentage = 70


splitSamples <-createTrainingSample(modDf, 1, modelling.samplePercentage)
trainSet <- as.data.frame(splitSamples[1])
validSet <- as.data.frame(splitSamples[2])
colnames(trainSet) <- colnames(modDf)
colnames(validSet) <- colnames(modDf)
#createDirectory(paste0(rootDir,'/Cubist'))
write.table(trainSet, paste0(workDir, '/trainingData2.csv'), sep=",", row.names=F)
write.table(validSet, paste0(workDir, '/validationData2.csv'), sep=",", row.names=F)

trainSet <- read.csv(paste0(workDir, '/trainingData2.csv'), stringsAsFactors = F)
validSet <- read.csv(paste0(workDir, '/validationData2.csv'), stringsAsFactors = F)
###   Generate a cubist model

y <- trainSet$probeVal
x <- trainSet[4:ncol(trainSet)]

Cmodel <- cubist(x = x , y = y, committees=3, cubistControl( rules = 100, extrapolation = 100))
summary(Cmodel)

CmodelFilename <- paste0(workDir, '/model_Cubist')
saveRDS(Cmodel, paste0(CmodelFilename, '.rds'))

Cmodel <- readRDS(paste0(CmodelFilename, '.rds'))

Coutfilename <-paste0(CmodelFilename, '.rules')
file.create(Coutfilename)
modelText <- summary(Cmodel)
writeLines(modelText$output, Coutfilename)
xp <- validSet[2:ncol(validSet)]
yp <- validSet$probeVal
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
Rmodel <- ranger(y ~ ., data = x, write.forest = TRUE, importance = 'impurity', num.trees = 10)
RmodelPath = paste0(workDir, '/RFmodel.rds')
saveRDS(Rmodel,RmodelPath )
summariseRFModel( RmodelPath, "SMIPS")
#imp <- model$variable.importance[order(model$variable.importance, decreasing = T)]
#imp
xp <- validSet[3:ncol(validSet)]
Rmv = predict(Rmodel, data=xp,  predict.all = F)
yp <- validSet$probeVal



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

outDir <- paste0(dataRoot, version, '/ObsVmod')
if(!dir.exists(outDir)){dir.create(outDir)}

pDir <- paste0(dataRoot, version, '/ProbeDumps')
probefls <- list.files(pDir, full.names = T, recursive = T, pattern = 'SM_SFS')

for (i in 1:nrow(siteData)) {
  
  print(i)
  obsfile <- probefls[i]
  #rec <- siteData[i,]
  #sid <- rec$SiteID
  
  #obsfile <- paste0(obsDir, '/', sid, '_', depth, '.rds') 

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
smipsAvgRoot <- paste0(dataRoot, version, '/SmipsMovingAverage')
templateR <- raster(paste0(dataRoot, '/Masks/SFS.tif'))
rasterOut <- paste0(dataRoot, version, '/Rasters/', depth)
if(!dir.exists( rasterOut)) {dir.create(rasterOut, recursive = T)}

covsPath <-  paste0(dataRoot, '/CovariatesNoNa')
covfls <- list.files(covsPath, full.names = T, recursive = T, pattern = '.tif')

startDate <- sDate + 2
endDate <- eDate - 2
dts <- seq.Date(startDate, endDate, 1)

predStk <- stack(c(covfls))
pdf1 <- as.data.frame(as.matrix(predStk))

for (i in 1:length(dts)) {
  
  print(i)
  dt <- dts[i]
  doyVal <- yday(dt)
  mthVal <- month(dt)
  seasonVal <- getSeasonAsNumeric(dt)
  
  fpath <- paste0(smipsAvgRoot, '/CSIRO_Wetness-Index_', dt ,'.tif' )
  smipsR <- raster(fpath)
  names(smipsR) <- 'SMIPSVal'
  smipsR[is.na(smipsR)] <- 0
 
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
  
  predStk2 <- stack(c(smipsR, doy, mth, season, depthR))
  #predStk[is.na(predStk[])] <- -1
  #map <- predict( Rmodel, as.data.frame(as.matrix(predStk)))
  
  pdf2 <- as.data.frame(as.matrix(predStk2))
  
  pdf <- cbind(pdf1,pdf2)
  
  #map <- predict( Cmodel, pdf)
  map <- predict( Rmodel, pdf)
  #Rmodel
  
  outR <- templateR
  #outR[] <- map$predictions
  outR[] <- map$predictions
  outName <- paste0(rasterOut, '/regSM_', dt , '.tif')
  outRc <- mask(outR, templateR, filename = outName, overwrite=T)
  
  plot(outRc, main=dt)
  
  
}

r1 <- raster('C:/Projects/SMIPS/SFS/regionalSM/data3/Rasters/300/regSM_2015-11-22.tif')
r2 <- raster('C:/Projects/SMIPS/SFS/regionalSM/data3/Rasters/300/regSM_2016-10-07.tif')
r1-r2


names(pdf)[2] <- 'SMIPSVal'

