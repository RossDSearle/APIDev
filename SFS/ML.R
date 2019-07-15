
library(lubridate)
library(Cubist)
library(ranger)
source('C:/Users/sea084/Dropbox/RossRCode/myFunctions/GeneralUtils.R')
source('C:/Users/sea084/Dropbox/RossRCode/myFunctions/ModelUtils.R')
source('C:/Users/sea084/Dropbox/RossRCode/NAWRA/Production/RandomForestUtils_V2.R')

source('C:/Users/sea084/Dropbox/RossRCode/Git/apiDev/SFS/SFSMethods.R')





apiDevRootDir <<- 'C:/Users/sea084/Dropbox/RossRCode/Git/apiDev'

startDate <- as.Date('2016-01-03')
endDate <- as.Date('2017-02-27')
dts <- seq.Date(startDate, endDate, 1)
nDays <- as.numeric(endDate - startDate)

smipsPath1 <- 'E:/SMIPS/Wetness-Index/Final/2016'
smipsfls1 <- list.files(smipsPath1, full.names = T, recursive = T, pattern = '.tif')
smipsPath2 <- 'E:/SMIPS/Wetness-Index/Final/2017'
smipsfls2 <- list.files(smipsPath2, full.names = T, recursive = T, pattern = '.tif')
allsmips <- c(smipsfls1, smipsfls2)

covsPath <- 'C:/Users/sea084/Dropbox/RossRCode/Git/APIDev/SFS/Covariates'
fls <- list.files(covsPath, full.names = T, recursive = T, pattern = '.tif')



sfsreg <- raster(paste0(apiDevRootDir, '/SFS/Masks/SFS.tif'))
outdf <- data.frame(longitude=numeric(), latitude=numeric(), ProbeVal=numeric(), SmipsVal=numeric())

#smipsClipOut <- 'C:/Projects/SMIPS/SFS/regionalSM/data/SmipsClips'

smipsAvgRoot <- 'C:/Projects/SMIPS/SFS/regionalSM/data/SmipsMovingAverage'

for (i in 1:length(dts)) {
  
  print(i)
  dt <- dts[i]
  doy <- yday(dt)
  mth <- month(dt)
  season <- getSeasonAsNumeric(dt)
  fpath <- paste0(smipsAvgRoot, '/CSIRO_Wetness-Index_', dt ,'.tif' )
  #fpath <- avgsmips[i]
  #fname <- basename(fpath)
  #dt <- str_remove( str_split(fname, '_')[[1]][3], '.tif')
  
  r <- raster(fpath)
  cr <- crop(r, sfsreg)
  plot(cr)
  #rcr <- resample(cr, sfsreg, filename = paste0(smipsClipOut, '/', basename(fpath)), overwrite=T)
  
  stk <- stack(c(r, fls))
  #stk <- addLayer(stk, r)
  names(stk)
  
  #probeData <- getProbeDataForaDate(dt)
  
  #  Max probe value for a day at 100 mm
  # sql <-  paste0('SELECT Sites.SiteID, Sensors.device, Sites.SiteName, Sites.Longitude, Sites.Latitude, DataStore.dt, Max(DataStore.value) AS SM 
  #                 FROM (Sites INNER JOIN Sensors ON Sites.ID = Sensors.device) INNER JOIN DataStore ON (Sensors.id = DataStore.id) AND (Sensors.device = DataStore.device)
  #                 WHERE  Sensors.Depth=100 and dt Between "', dt, '" And "', as.Date(dt) + 1 ,'"
  #                 GROUP BY Sites.SiteID, Sensors.device, Sites.SiteName, Sites.Longitude, Sites.Latitude;')
  # 
  #  Avg probe value for a day acrros all depths
  
  sql <-  paste0('SELECT Sites.SiteID, Sensors.device, Sites.SiteName, Sites.Longitude, Sites.Latitude, DataStore.dt, Avg(DataStore.value) AS SM 
                  FROM (Sites INNER JOIN Sensors ON Sites.ID = Sensors.device) INNER JOIN DataStore ON (Sensors.id = DataStore.id) AND (Sensors.device = DataStore.device)
                  WHERE dt Between "', dt, '" And "', as.Date(dt) + 1 ,'"
                  GROUP BY Sites.SiteID, Sensors.device, Sites.SiteName, Sites.Longitude, Sites.Latitude;')
  probeData  <- doq(sql)
  
  coordinates(probeData) <-  ~Longitude+Latitude
  crs(probeData) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  pts <- extract(stk, probeData, df=T)
  names(pts)[2] <- 'SMIPS'
  
  smipsDrill <- data.frame(longitude=probeData@coords[,1], latitude=probeData@coords[,2], ProbeVal=probeData@data$SM, pts[,-1], doy, mth, season )
  
  outdf <- rbind(outdf, smipsDrill)

}


workDir <- paste0(apiDevRootDir, '/SFS/ML')

write.csv(outdf, paste0(workDir, '/SmipsSmoothDrill.csv'))
modDf <- read.csv(paste0(workDir, '/SmipsSmoothDrill.csv'))

plot(outdf$ProbeVal, outdf$SmipsVal)
cor(outdf$ProbeVal, outdf$SmipsVal) 



modelling.samplePercentage = 30


splitSamples <-createTrainingSample(modDf, 1, modelling.samplePercentage)
trainSet <- as.data.frame(splitSamples[1])
validSet <- as.data.frame(splitSamples[2])
colnames(trainSet) <- colnames(modDf)
colnames(validSet) <- colnames(modDf)
#createDirectory(paste0(rootDir,'/Cubist'))
write.table(trainSet, paste0(workDir, '/trainingData.csv'), sep=",", row.names=F)
write.table(validSet, paste0(workDir, '/validationData.csv'), sep=",", row.names=F)


y <- trainSet$ProbeVal
x <- trainSet[5:ncol(trainSet)]
x <- trainSet[,5:8]
Cmodel <- cubist(x = x[1:100,] , y = y[1:100], committees=1,  cubistControl( label = "SMIPS", rules = 2))
summary(Cmodel)

CmodelFilename <- paste0(rootDir, '/', crp, '_model_Cubist')
saveRDS(Cmodel, paste0(CmodelFilename, '.rds'))
Coutfilename <-paste0(CmodelFilename, '.rules')
file.create(Coutfilename)
modelText <- summary(Cmodel)
writeLines(modelText$output, Coutfilename)

xp <- validSet[2:ncol(validSet)]
yp <- validSet$Yield
Cmv <- predict(Cmodel, xp)
Ctdf <- data.frame(yp, Cmv)

fitStats(Ctdf[1],Ctdf[2], paste0(crp, ' Cubist'),  paste0(Coutfilename, '_ModelStats.txt'), 'topleft', verbose = T)
write.table(Ctdf, paste0(Coutfilename, '_Obs_vs_Mod.csv'), sep=",", row.names=F)



Rmodel <- ranger(y ~ ., data = x, write.forest = TRUE, importance = 'impurity', num.trees = 1000)
RmodelPath = paste0(workDir, '/RFmodel.rds')
saveRDS(Rmodel,RmodelPath )
summariseRFModel( RmodelPath, "SMIPS")
#imp <- model$variable.importance[order(model$variable.importance, decreasing = T)]
#imp
xp <- validSet[2:ncol(validSet)]
Rmv = predict(Rmodel, data=xp,  predict.all = F)
yp <- validSet$ProbeVal
Rtdf <- data.frame(yp, Rmv$predictions)
RmodelFilename <- paste0(workDir, '/', 'SMIPS', '_model_Ranger')
Coutfilename <-paste0(RmodelFilename, '.rules')
fitStats(Rtdf[1],Rtdf[2], paste0('SMIPS', ' Ranger'),  paste0(RmodelFilename, '_ModelStats.txt'), 'topleft', verbose = T)
write.table(Rtdf, paste0(RmodelFilename, '_Obs_vs_Mod.csv'), sep=",", row.names=F)




##   Make a map  ######

workDir <- paste0( 'C:/Projects/SMIPS/SFS/regionalSM/data/ML')
Rmodel <- readRDS(paste0(workDir, '/RFmodel.rds'))
templateR <- raster(paste0(apiDevRootDir, '/SFS/Masks/SFS.tif'))
covsPath <- 'C:/Users/sea084/Dropbox/RossRCode/Git/APIDev/SFS/Covariates'
fls <- list.files(covsPath, full.names = T, recursive = T, pattern = '.tif')

dt <- as.Date('2016-11-19')
doyVal <- yday(dt)
mthVal <- month(dt)
seasonVal <- getSeasonAsNumeric(dt)

fpath <- paste0(smipsAvgRoot, '/CSIRO_Wetness-Index_', dt ,'.tif' )
smipsR <- raster(fpath)
names(smipsR) <- 'SMIPS'

depth <- templateR
depth[] <- 300
names(depth) <- 'Depth'
doy <- templateR
doy[] <- doyVal
names(doy) <- 'doy'
mth <- templateR
mth[] <- mthVal
names(mth) <- 'mth'
season <- templateR
season[] <- seasonVal
names(season) <- 'season'

predStk <- stack(c(depth, smipsR, fls, doy, mth, season))
names(predStk)
Rmodel$forest$independent.variable.names
predStk[is.na(predStk[])] <- 0
map <- predict( Rmodel, as.data.frame(as.matrix(predStk)))

outR <- templateR
outR[] <- map$predictions
plot(outR)
names(predStk)
Rmodel$forest$independent.variable.names


plot(predStk)


##   Make lots of maps  ######
smipsAvgRoot <- 'C:/Projects/SMIPS/SFS/regionalSM/data/SmipsMovingAverage'
templateR <- raster(paste0(apiDevRootDir, '/SFS/Masks/SFS.tif'))

for (i in 1:length(dts)) {
  
  print(i)
  dt <- dts[i]
  doy <- yday(dt)
  mth <- month(dt)
  season <- getSeasonAsNumeric(dt)

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
  
  predStk <- stack(c(smipsR, fls, doy, mth, season))
  predStk[is.na(predStk[])] <- -1
  map <- predict( Rmodel, as.data.frame(as.matrix(predStk)))
  
  outR <- templateR
  outR[] <- map$predictions
  outName <- paste0(apiDevRootDir, '/SFS/ML/RegionalMaps/regSM_', dt , '.tif')
  outRc <- mask(outR, templateR, filename = outName, overwrite=T)
  
  plot(outRc)
  
  
}

