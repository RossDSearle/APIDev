library(geojson)
library(raster)


machineName <- as.character(Sys.info()['nodename'])
print(machineName)


if(machineName == 'FANCY-DP'){
  apiDevRootDir <<- 'C:/Users/sea084/Dropbox/RossRCode/Git/APIDev'
  conPath <<- 'C:/Projects/SMIPS/SFS/sfs.db'
}else{
  apiDevRootDir <<- '/srv/plumber/APIDev'
  conPath <<- '/mnt/data/RegionalSoilMoisture/sfs.db'
}

#server <<- '127.0.0.1'
#portNum <<- 8029
#portNum <<- 8031

source(paste0(apiDevRootDir, '/AgX/AgXMethods.R'))
source(paste0(apiDevRootDir, '/SFS/SFSMethods.R'))


cat(server)