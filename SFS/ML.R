library(ranger)

source('C:/Users/sea084/Dropbox/RossRCode/Git/apiDev/SFS/SFSMethods.R')

apiDevRootDir <<- 'C:/Users/sea084/Dropbox/RossRCode/Git/apiDev'

smipsPath1 <- 'E:/SMIPS/Wetness-Index/Final/2016'
smipsfls1 <- list.files(smipsPath1, full.names = T, recursive = T, pattern = '.tif')
smipsPath2 <- 'E:/SMIPS/Wetness-Index/Final/2017'
smipsfls2 <- list.files(smipsPath2, full.names = T, recursive = T, pattern = '.tif')
allsmips <- c(smipsfls1, smipsfls2)

covsPath <- 'C:/Users/sea084/Dropbox/RossRCode/Git/APIDev/SFS/Covariates'
fls <- list.files(covsPath, full.names = T, recursive = T, pattern = '.tif')



sfsreg <- raster(paste0(apiDevRootDir, '/SFS/Masks/SFS.tif'))
outdf <- data.frame(longitude=numeric(), latitude=numeric(), ProbeVal=numeric(), SmipsVal=numeric())

smipsClipOut <- 'C:/Projects/SMIPS/SFS/regionalSM/data/SmipsClips'

for (i in 1:length(allsmips)) {
  
  print(i)
  
  fpath <- allsmips[i]
  fname <- basename(fpath)
  dt <- str_remove( str_split(fname, '_')[[1]][3], '.tif')
  
  r <- raster(fpath)
  cr <- crop(r, sfsreg)
  plot(cr)
  rcr <- resample(cr, sfsreg, filename = paste0(smipsClipOut, '/', basename(fpath)), overwrite=T)
  
  stk <- stack(c(rcr, fls))
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

  pts <- extract(stk, probeData)
  smipsDrill <- data.frame(longitude=probeData@coords[,1], latitude=probeData@coords[,2], ProbeVal=probeData@data$SM, pts )
  
  outdf <- rbind(outdf, smipsDrill)

}




plot(outdf$ProbeVal, outdf$SmipsVal)
cor(outdf$ProbeVal, outdf$SmipsVal) 



