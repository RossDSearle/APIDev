library(raster)
library(DBI)
library(xts)

DBStartDate <- as.Date('2014-08-08')
DBEndDate <- as.Date('2016-10-11')

DBEndDate - DBStartDate


apiDevRootDir <<- 'C:/Users/sea084/Dropbox/RossRCode/Git/apiDev'

source('C:/Users/sea084/Dropbox/RossRCode/Git/apiDev/SFS/SFSMethods.R')

conPath <<- 'C:/Projects/SMIPS/SFS/sfs.db'
inDir <- 'C:/Users/sea084/Dropbox/RossRCode/Git/APIDev/SFS/ML/RegionalMaps'

smm <- list.files(inDir, full.names = T, recursive = T, pattern = '.tif')


stkm <- stack(smm)
plot(stkm)


library(colorRamps)
col5 <- colorRampPalette(c('blue', 'gray96', 'red'))  #create color ramp starting from blue to red
color_levels=20 #the number of colors to use
max_absolute_value=40 #what is the maximum absolute value of raster?
plot(stkm, col=col5(n=color_levels), breaks=seq(25,max_absolute_value,length.out=color_levels+1) , axes=FALSE)


sdt <- as.Date('2016-01-03')
edt <- as.Date('2017-02-27')
dts <- seq.Date(sdt, edt, 1)


####   Extract The average soil mositure across the whole of the predicted raster each day
avgs <- numeric(length(smm))

for (i in 1:length(smm)) {
  
  r <- raster(smm[i])
  a <- mean(r[], na.rm=T)
  print(a)  
  
  avgs[i] <- a
}
plot(avgs)
avgModeledSM <- xts(avgs, order.by=dts)
plot(avgModeledSM)


tso <- readRDS('C:/Users/sea084/Dropbox/RossRCode/Git/APIDev/SFS/TS/cerdi.sfs.5096.platform.rds')

tsm <- xts::merge.xts(avgModeledSM, tso)
plot(tsm)


######   Extract the modelled values for each SM Probe for each day
avgs <- numeric(length(smm))

modDir <- paste0(apiDevRootDir, '/SFS/ML/RegionalMaps')
fls <- list.files(modDir, full.names = T, recursive = T, pattern = '.tif')
stk <- stack(fls)


sql <- 'select * from Sites'
siteData  <- doq(sql)

coordinates(siteData) <-  ~Longitude+Latitude
crs(siteData) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
pts <- extract(stk, siteData, df=T)
tpts <- t(pts)
modVals <- tpts[-1,]
msm <- xts(modVals, order.by=dts)
colnames(msm) <- siteData$SiteID
plot(ModeledSMatProbes)


for (i in 1:nrow(siteData)) {
  print(i)
  rec <- siteData[i,]
  oname <- paste0(apiDevRootDir, '/SFS/TS/', rec$SiteID, '.rds')
  tso <- readRDS(oname)
  tsm <- msm[,rec$SiteID]
  plot(tsm, ylim=c(30, 50))
  lines(tso, col='red')
}


tso <- readRDS('C:/Users/sea084/Dropbox/RossRCode/Git/APIDev/SFS/TS/cerdi.sfs.5096.platform.rds')
tsm <- msm[,'cerdi.sfs.5096.platform']

plot(tsm)
lines(tso, col='red')
for (i in 1:length(dts)) {
  
  dt <- dts[i]
 dailySM <- getProbeDataForaDate(dt)
 t(dailySM)
  
  r <- raster(smm[i])
  a <- mean(r[], na.rm=T)
  print(a)  
  
  avgs[i] <- a
}








sql <- 'select * from Sites'
siteData  <- doq(sql)


tsOutDir <- paste0(apiDevRootDir, '/SFS/TS' )
sdt <- '2016-01-01'

for (i in 1:nrow(siteData)) {
  
  print(i)
  
  rec <- siteData[i,]
  sid <- rec$SiteID
  
  sql <-  paste0('SELECT DataStore.dt, Avg(DataStore.value) AS SM 
                  FROM (Sites INNER JOIN Sensors ON Sites.ID = Sensors.device) INNER JOIN DataStore ON (Sensors.id = DataStore.id) AND (Sensors.device = DataStore.device)
                  WHERE SiteID = "',sid , '" and ( dt Between "', sdt, '" and "', as.Date(sdt) + 800 ,'")
                  GROUP BY Sites.SiteID, Sensors.device, Sites.SiteName, date(DataStore.dt) 
                  ORDER By  date(DataStore.dt)')
  
  tsdf  <- doq(sql)
  tail(tsdf)
  ts <- xts(tsdf[,-1], order.by=as.Date(tsdf[,1]))
  
  write.csv(tsdf, paste0(tsOutDir, '/', sid, '.csv'))
  saveRDS(ts, paste0(tsOutDir, '/', sid, '.rds'))
  
  plot(qxts)
}

sql <-  paste0('SELECT Sites.SiteID, Sensors.device, Sites.SiteName, Sites.Longitude, Sites.Latitude, DataStore.dt, Avg(DataStore.value) AS SM 
                  FROM (Sites INNER JOIN Sensors ON Sites.ID = Sensors.device) INNER JOIN DataStore ON (Sensors.id = DataStore.id) AND (Sensors.device = DataStore.device)
                  WHERE SiteID = "cerdi.sfs.4860.platform" and ( dt Between "', sdt, '" and "', as.Date(sdt) + 400 ,'")
                  GROUP BY Sites.SiteID, Sensors.device, Sites.SiteName, date(DataStore.dt) ')
ts  <- doq(sql)
ts
head(ts)
tail(ts)


sql <-  paste0('SELECT DataStore.dt, Avg(DataStore.value) AS SM 
                  FROM (Sites INNER JOIN Sensors ON Sites.ID = Sensors.device) INNER JOIN DataStore ON (Sensors.id = DataStore.id) AND (Sensors.device = DataStore.device)
                  WHERE SiteID = "cerdi.sfs.4860.platform" and ( dt Between "', sdt, '" and "', as.Date(sdt) + 8000 ,'")
                  GROUP BY Sites.SiteID, Sensors.device, Sites.SiteName, date(DataStore.dt) 
                  ORDER By  date(DataStore.dt)')
ts  <- doq(sql)
ts

qxts <- xts(ts[,-1], order.by=as.Date(ts[,1]))

plot(qxts)
tail(ts)




