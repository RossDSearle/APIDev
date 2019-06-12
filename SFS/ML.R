library(ranger)

source('C:/Users/sea084/Dropbox/RossRCode/Git/apiDev/SFS/SFSMethods.R')

apiDevRootDir <<- 'C:/Users/sea084/Dropbox/RossRCode/Git/apiDev'

smipsPath <- 'C:/Projects/SMIPS/Rasters/CSIRO/Wetness-Index/Final'

fls <- list.files(smipsPath, full.names = T, recursive = T, pattern = '.tif')


stk <- stack(fls[1:5])

sql <- 'Select * from Sites'
sites <- doq(sql)




outdf <- data.frame(longitude=numeric(), latitude=numeric(), ProbeVal=numeric(), SmipsVal=numeric())

for (i in 1:length(fls)) {
  
  print(i)
  
  fpath <- fls[i]
  fname <- basename(fpath)
  dt <- str_remove( str_split(fname, '_')[[1]][3], '.tif')
  
  r <- raster(fpath)
  
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
  #plot(sites)
  pts <- extract(r, probeData)
  smipsDrill <- data.frame(longitude=probeData@coords[,1], latitude=probeData@coords[,2], ProbeVal=probeData@data$SM, SmipsVal=pts)
  
  outdf <- rbind(outdf, smipsDrill)

}




plot(outdf$ProbeVal, outdf$SmipsVal)
cor(outdf$ProbeVal, outdf$SmipsVal) 



