## Soil spectral functions

##FUNCTIONS

# Removing the instrumnet detector errors (Mario method)
rm.detector.error<- function(spectra,wavelength, cuts) {
  section1<-spectra[which(names(spectra)=="350"):which(names(spectra)== cuts[1])]
  section2<-spectra[which(names(spectra)== cuts[1]):which(names(spectra)==cuts[2])]
  section3<-spectra[which(names(spectra)== cuts[2]):which(names(spectra)=="2500")]
  ##Final and initial points of sections (corrected by the slope of the previous points)
  d1<- cuts[1] -1
  d1<- as.character(d1)
  d2<- cuts[1] -2
  d2<- as.character(d2)
  d1a<- cuts[1] + 1
  d1a<- as.character(d1a)
  d2a<- cuts[1] + 2
  d2a<- as.character(d2a)
  e1<- cuts[2] - 1
  e1<- as.character(e1)
  e2<- cuts[2] - 2
  e2<- as.character(e2)
  e1a<- cuts[2] + 1
  e1a<- as.character(e1a)
  e2a<- cuts[2] + 2
  e2a<- as.character(e2a)
  f1<-section1[d1]+section1[d1]-section1[d2]
  i2<-section2[d1a]+section2[d1a]-section2[d2a]
  f2<-section2[e1]+section2[e1]-section2[e2]
  i3<-section3[e1a]+section3[e1a]-section3[e2a]
  
  section1[length(section1)]<-f1
  section2[1]<-i2
  section2[length(section2)]<-f2
  section3[1]<-i3
  
  ##steps
  dif1<-(i2-f1)
  dif2<-(i3-f2)
  ############            EQUATIONS by Padarian et al. AND MATH IS FUN  ################# 
  ##a is the movement of section1, b of section2 and so on 
  equations<-matrix(c(1,0,1,-1,1,1,0,-1,1),3)
  colnames(equations)<-c("a","b","c")
  ##the conditions of the different equations
  solMat<- matrix(NA,ncol=3,nrow=nrow(spectra))
  cSpectra<- matrix(NA,ncol=ncol(spectra),nrow=nrow(spectra))
  
  # for every spectrum
  for (i in 1:nrow(spectra)){
    conditions<-matrix(c(dif1[i,],dif2[i,],0))
    ##solve the equations (dot product of the inverse of the first matrix by the second one)
    solMat[i,]<-t(solve(equations)%*%conditions)
    ##smoothing
    cSpectra[i,]<-as.matrix(cbind((section1[i,-length(section1)]+solMat[i,1]),(section2[i,]+solMat[i,2]),(section3[i,-1]+solMat[i,3])))}
  cSpectra<- as.data.frame(cSpectra)
  colnames(cSpectra)<- wavelength # label the columns accordingly
  return(cSpectra)}



#Function for trimming spectra or targeting a specific spectral region of interest
trimSpec <- function(spectra, wavlimits) {
  datawavs <- as.numeric(names(spectra))
  limits <- which(datawavs %in% wavlimits)
  kept_index <- seq(limits[1], limits[2], 1)
  trimmed_spectra <- spectra[, kept_index]
  kept_names <- datawavs[kept_index]
  colnames(trimmed_spectra) <- kept_names
  return(trimmed_spectra)}


#Function for applying Savitsky-Golay smoothing filter
filter_sg <- function(spectra, n, p, m) {
  spectra <- as.matrix(spectra)
  ## run filter
  sg <- aaply(spectra, 1, sgolayfilt, n = n, p = p, m = m)
  ## arrange appropriately if a single sample
  if (nrow(spectra) == 1) {
    sg <- matrix(sg, dim(spectra))}
  ## return data frame
  sg<- as.data.frame(sg)
  colnames(sg) <- colnames(spectra)
  return(sg)
}


#Function for applying Standard Normal Variate Transformation
snvBLC <- function(spectra) {
  spectra <- as.matrix(spectra)
  snvMat <- matrix(NA, ncol = ncol(spectra), nrow = nrow(spectra))
  for (i in 1:nrow(spectra)) {
    snvMat[i, ] <- (spectra[i, ] - mean(spectra[i, ]))/sd(spectra[i,])}
  snvMat<- as.data.frame(snvMat)
  colnames(snvMat) <- colnames(spectra)
  return(snvMat)}

#Function for applying Multiplicative Scatter Correction
mscBLC <- function(spectra) {
  #first calculate a mean spectrum.
  meanSpec <- as.matrix(colMeans(spectra))
  mscMat <- matrix(NA, ncol = ncol(spectra), nrow = nrow(spectra))
  spectra <- as.matrix(spectra)
  for (i in 1:nrow(spectra)) {
    # determine the slope and intercept co-efficents
    specLM <- lm(spectra[i, ] ~ meanSpec)
    specCE <- t(as.matrix(specLM$coefficients))
    # Adjust the spectra
    mscMat[i, ] <- t(as.matrix((spectra[i, ] - specCE[1, 1])/specCE[1,2]))}
  mscMat<- as.data.frame(mscMat)
  colnames(mscMat) <- colnames(spectra)
  return(mscMat)}

#Spectral or dimension reduction: Resampling and averaging
compSpec<- function(spectra, window){
  compMat<- matrix(NA,ncol=(ncol(spectra))/window,nrow=nrow(spectra))
  cc<-1
  for (i in 1:ncol(compMat)) {
    compMat[,i]<-rowMeans(spectra[,cc:(cc+(window-1))])
    cc<-cc+window}
  colnames(compMat)<- colnames(spectra)
  return(compMat)}

#Spectral or dimension reduction: Wavelets
wavelet_smooth<- function(spectra, res){
  wave_spectra<- matrix(NA,ncol=2^res,nrow=nrow(spectra))
  for (i in 1:nrow(spectra)){
    wds<-wd(as.matrix(spectra[i,]),bc="symmetric",filter.number = 10, family = 'DaubExPhase', min.scale = 2)
    wave_spectra[i,]<- accessC.wd(wds, level=res)}
  colnames(wave_spectra) <- seq((404 + 0.5*(2048/(2^res))),2451, by=2048/(2^res))
  return(wave_spectra)}

## Convex hull method
chBLC<- function(spectra){
  interval<- c(1:ncol(spectra))
  hull_spectra<- matrix(NA,ncol=ncol(spectra),nrow=nrow(spectra))
  for (i in 1:nrow(spectra)){
    tempSpect= as.matrix(spectra[i,])
    data1 <- sortedXyData(interval, tempSpect)
    ## calculate convex hull
    c_hull <- chull(data1)
    ## get the appropriate region
    c_hull <- c_hull[which(c_hull == 1):length(c_hull)]
    ## calculate linear approximation between hull points
    linear_approx <- approx(data1[c_hull,], xout = interval, method = 'linear', ties = 'mean')
    ## calculate the deviation from the convex hull
    hull_spectra[i,] <- ( linear_approx[[2]] - tempSpect )/linear_approx[[2]]}
  colnames(hull_spectra) <- colnames(spectra)
  return(hull_spectra)}
