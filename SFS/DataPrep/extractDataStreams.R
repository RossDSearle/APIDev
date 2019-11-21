library(RCurl)
library(jsonlite)
library(raster)
library(GSIF)
library(rgdal)
library(gstat)
library(automap)

source('C:/Users/sea084/Dropbox/RossRCode/myFunctions/GeneralUtils.R')

rootDir <- 'C:/Projects/SMIPS/SFS/regionalSM/data3'

server <- 'http://ternsoils.nexus.csiro.au'
portNum <- 8070

server <- 'http://esoil.io/SensorFederationWebAPI'
portNum <- ''

sfUsr <- 'ross.searle@csiro.au'
sfPwd <- 'S4QQBMk74zhnBnEpTcd6iLwlUredn6kekLkjFL'

minX <- 141
maxX <- 144.5
minY <- -38.5
maxY <- -37
  
aoi <- '-38.5;141;-37;144.5'
startDate <- '2015-09-09T00:00:00'
endDate <-  '2019-10-06T23:59:59'
dataType <- 'Soil-Moisture'


# make a map of all sensor locations
#url <- paste0(server,'/SensorAPI/getSensorLocations?usr=', sfUsr, '&pwd=', sfPwd)
url <- paste0(server,'/SensorAPI/getSensorLocations?usr=', sfUsr, '&pwd=', sfPwd, '&bbox=', aoi)
sensorsJson <- getURL(url)
sensDF <- fromJSON(sensorsJson)
head(sensDF)
vcd(sensDF)
str(sensDF)

pts <- sensDF
coordinates(pts) <- ~Longitude+Latitude
plot(pts)
writeOGR(pts, paste0(rootDir), 'SFSsensorLocs', driver="ESRI Shapefile", overwrite_layer=TRUE)


myPolygon = Polygon(cbind(c(minX,minX,maxX,maxX,minX),c(minY,maxY,maxY,minY,minY)))
myPolygons = Polygons(list(myPolygon), ID = "A")
SpPolygon = SpatialPolygons(list(myPolygons))
lines(SpPolygon, col='red')
z <- lapply(list(SpPolygon), function(i) SpatialPolygonsDataFrame(i, data.frame(id=1:length(i)), match.ID = FALSE))
writeOGR(z[[1]], paste0(rootDir), 'sfsBox', driver="ESRI Shapefile", overwrite_layer=TRUE)


sensDF <- sensDF[sensDF$Backend == 'IOT_CERDI', ]


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
      
        outDir <-  paste0(rootDir, '/ProbeDumps')
        if(!dir.exists(outDir)){dir.create(outDir)}
        
      write.csv(sdf, paste0(rootDir, '/ProbeDumps/SM_', id, '.csv'))
      outDF <- rbind(outDF, sdf)
      }else{
        print(paste0('ERROR : ', id))
      }

}

write.csv(outDF, paste0(rootDir, '/ProbeDumps/SM_All.csv'))

