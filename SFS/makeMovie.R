library(raster)
library(imager)
library(png)
library(stringr)
library(rasterVis)

version = '3'

dataRoot <- 'C:/Projects/SMIPS/SFS/regionalSM/data'
SMIn <- paste0(dataRoot, version, '/Rasters/300')
fls <- list.files(SMIn, full.names = T, recursive = T, pattern = '.tif')
#theStack <- stack(SMinPaths)

sdate <- as.Date('2016-01-01')
edate <- as.Date('2019-10-01')
depth <- 300

dts <- seq.Date(sdate, edate,1)
fls <- paste0(SMIn, '/regSM_', dts, '.tif')

theStack <- stack(fls)
upperBound <- max(maxValue(theStack))
lowerBound <- min(minValue(theStack))


probVals <- read.csv(paste0(dataRoot, version, '/ML/SmipsSmoothDrill.csv'), stringsAsFactors = F)

colfunc <- colorRampPalette(c("brown", 'lightyellow', "darkblue"))

tmpDir <- 'c:/temp/vids'
if(!dir.exists(tmpDir)){dir.create(tmpDir)}
invisible(file.remove(list.files(tmpDir, full.names=TRUE)))


bR <- readRDS(paste0(dataRoot, version, '/Validation/bolacRain.rds'))

print('Generating images from rasters')
pb <- txtProgressBar(min = 0, max = nlayers(theStack), style = 3)

for(i in 1:nlayers(theStack)){
  
  
  
  png(paste0(tmpDir, "/image-", i, ".png"), 1000, 800)
  
  
  r <- theStack[[i]]
  dt <- as.Date(str_replace_all(str_replace(names(r), 'regSM_', ''), '[.]', '-' ))
  
  ### the raster plot seems to mess up the layout so plot it last
  nf <- layout(matrix(c(3,1,
                        2,0
                        #2,0
                        ),nrow=2,byrow = TRUE), widths = c(0.9,0.1),heights = c(0.8,0.2))
 # layout.show(n=3)
  
  #layout.show(nf)
  par(mar = c(0.1, 0.1, 0.1, 0.1)) 
  legend_image <- as.raster(matrix(rev(colfunc(20)), ncol=1))
  plot(c(0,2),c(0,8),type = 'n', axes = F,xlab = '', ylab = '')
  text(x=1, y = seq(0,7,l=6), labels = seq(0,70,l=6))
  rasterImage(legend_image, xleft=0.2, ybottom=0, xright=0.5,ytop=8)
  #plot(c(0,8),c(0,2),type = 'n', axes = F,xlab = '', ylab = '')
  
  #lines(c(0.4,7.7), c( 1, 1), lwd=4)
  #text(x=seq(0, 8, length.out=6), y = 0.7, labels = c('2001', 'Mar', 'May', 'Jul', 'Sep', 'Nov'))
  #text(x=seq(0, 8, length.out=365), y = 0.7, labels = seq(2001, 2016, 1))
  #text(x=seq(0.4, 7.7, length.out=12), y = 0.7, labels = c('Jan', 'Feb', 'Mar', 'April', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
  
  #xpt <- ((7.3/nlayers(theStack)) * i) + .4
  #points(xpt,1, pch=18, cex=3, col='red')
  
  #plot.zoo(bR, yaxt="n", ylab=NULL)
  par(mar = c(0.1, 0.1, 0.1, 0.1)) 
  xp <-  plot(bR, col='blue', type='h', main='Rainfall at Bolac',  xaxt="n", xlab='')
  big.red.dot <- xts(2, dt)
  xp <- points(  big.red.dot, col="red", pch=18, cex=3  )
  plot(xp)
  
  par(mar = c(1, 1, 1, 1)) 
  #plot(theStack[[i]], col=colfunc(20),zlim=c(0,60), legend=FALSE, axes=FALSE, box=FALSE)
  image(theStack[[i]], col=colfunc(20),zlim=c(0,60), legend=FALSE, axes=FALSE, box=FALSE, xlab='', ylab='')
  
  pVals <- probVals[probVals$Depth == depth & probVals$dt == dt, 3:5 ]
   coordinates(pVals) <-  ~longitude+latitude
  # wgs84 <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
  # crs(pVals) <- wgs84
   pVals$Col <- colfunc(20)[as.numeric(cut(pVals$ProbeVal,breaks = 20))]
   points(pVals, pch=16, cex = 5, col='White')
   points(pVals, pch=16, cex = 3, lwd = 5, col=pVals$Col)

  
  #pVals$longitude, pVals$latitude, pch=16, cex = 2, col='White'
  #levelplot(r, margin=FALSE, colorkey=FALSE) + layer(sp.points(data.frame(x=pVals$longitude, y=pVals$latitude), pch=16, cex = 2, col='White'))
  
  dev.off()
  plotRGB(stack(paste0(tmpDir, "/image-", i, ".png")))
  
  setTxtProgressBar(pb, i)
}
close(pb)


#make.video(tmpDir,'c:/temp/a.avi', fps = 25, verbose = F,  extra.args='scale=600:600')
#load.video('c:/temp/a.avi') %>% play



args <- ' -framerate 0025  -i c:/temp/vids/image-%d.png -vf scale=2000:1600 -y c:/temp/a.avi'
system2("ffmpeg", args, stdout = F, stderr = F)
system('open "c:/temp/a.avi"')
