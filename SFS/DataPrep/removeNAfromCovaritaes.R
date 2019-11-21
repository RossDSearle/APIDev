library(raster)

covsPath <- 'C:/Users/sea084/Dropbox/RossRCode/Git/APIDev/SFS/Covariates'
fls <- list.files(covsPath, full.names = T, recursive = T, pattern = '.tif')

for (i in 1:length(fls)) {
  r <- raster(fls[i])
  r[is.na(r[])] <- 0
  outname <- paste0('C:/Users/sea084/Dropbox/RossRCode/Git/APIDev/SFS/CovariatesNoNa/', basename(fls[i]) )
  writeRaster(r, outname)
}