library(httr)
library(jsonlite)
library(RCurl)
library(rgdal)
library(xts)

source('C:/Users/sea084/Dropbox/RossRCode/myFunctions/GeneralUtils.R')
source('C:/Users/sea084/Dropbox/RossRCode/myFunctions/ModelUtils.R')


dataRoot <- 'C:/Projects/SMIPS/SFS/regionalSM/data'

url <- 'http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorLocations'
sd <- fromJSON(URLencode(url))


coordinates(sd) <-  ~Longitude+Latitude
wgs84 <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
crs(sd) <- wgs84
plot(sd)

writeOGR(sd, paste0(dataRoot, '/Validation'), 'SensorLocs', driver="ESRI Shapefile", overwrite_layer=TRUE)

         

url <- 'http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=VicAg_LakeBolac&sensortype=Rainfall&startdate=2016-01-01T00%3A00%3A00&enddate=2019-10-01T00%3A00%3A00&aggperiod=days'
rainfall <- fromJSON(URLencode(url))

rts <- xts(rainfall$DataStream[[1]]$v, order.by = as.Date(rainfall$DataStream[[1]]$t))
bolacRain <- rts['2016/2019']
plot(bolacRain)


saveRDS(bolacRain,  paste0(dataRoot, version, '/Validation/bolacRain.rds'))

url <- 'http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=VicAg_LakeBolac&sensortype=Soil-Moisture&startdate=2016-01-01T00%3A00%3A00&enddate=2019-10-01T00%3A00%3A00&aggperiod=days'
sm <- fromJSON(URLencode(url))

head(sm$DataStream[1])
str(sm$DataStream)

smts <- xts(sm$DataStream[[1]]$v, order.by = as.Date(sm$DataStream[[1]]$t))
bolacSM30 <- smts['2016-01-01/2019-10-01']
bolacSM30[which(bolacSM30 < 0)] <- NA
plot(bolacSM30)

bsm <-na.approx(bolacSM30)
plot(bsm)
saveRDS(bsm,  paste0(dataRoot,version, '/Validation/bolacSM30.rds'))


SMIn <- paste0(dataRoot,version, '/Rasters/300')

sdate <- as.Date('2016-01-01')
edate <- as.Date('2019-10-01')
depth <- 300

dts <- seq.Date(sdate, edate,1)
fls <- paste0(SMIn, '/regSM_', dts, '.tif')
stk <- stack(fls)

pt <- as.matrix(data.frame(x=as.numeric(sm$Longitude[1]), y=as.numeric(sm$Latitude[1]), stringsAsFactors =  F ))
xy <- SpatialPoints(pt)
pts <- raster::extract(stk, xy, df=T)

bts <- t(pts)[-1,]

bolacDF <- data.frame(dt=dts, modelled=bts, observed=bsm)
plot(bolacDF$modelled, bolacDF$observed)

bolXts <- xts(x=bolacDF[,2:3], order.by = bolacDF$dt)
plot(bolXts, legend.loc='topleft', main="Modelled Vs Observed Soil Moisture at 30cm at Bolac")
fitStats(bolXts$modelled, bolXts$observed, paste0('Bolac Modelled Vs Observed'),  paste0('c:/temp/ModelStats.txt'), 'topleft', verbose = T)

saveRDS(bolXts,  paste0(dataRoot, '/Validation/bolacObsVmod30.rds'))


