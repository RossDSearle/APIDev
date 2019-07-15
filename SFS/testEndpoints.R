library(RCurl)
library(raster)


date <- '2016-06-01'

date <- '2016-05-01'

url <- paste0('http://esoil.io/APIDev/SoilMoisture/GetMap?Region=SFS&Date=', date)


outfile = paste0('c:/temp/', date, '.tif')
download.file(url, destfile = outfile, quiet = T, mode = 'wb')
r <- raster(outfile)
plot(r)


