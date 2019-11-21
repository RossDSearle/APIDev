## Contrived example of spectral prep-processing and modelling using Australian Archive data

## 1. National vis-NIR library join with soil data 
## 2. Spectra data pre-processing
## 3. Spectra and data outlier detection and removal  
## 4. Partial least squares modelling of target variable
## Target variable is: Soil Organic Carbon
## spectra are preprocessed:
## splice correction, trimming, conversion to absorbance, baseline with SNV
## 

setwd('C:/Users/sea084/Dropbox/RossRCode/Git/Spectra')
library(plyr);library(signal);library(pls);library(ithir);library(MASS)

# functions
source("spectralProcess_functions.R")


# soil data
soil.dat<- readRDS("soil_data_archive_SOC.rds")


# spectra data
spectra.dat<- readRDS("soil_spectra_archive.rds")
write.csv(spectra.dat, 'spectra.csv')



## JOINING OF DATASETS
# soil information where there is a spectra
soil.dat<- soil.dat[soil.dat$spec_id %in% spectra.dat$specID,]


# soil information with spectra and soil organic carbon 
# lab codes: 6A1, 6_DC, 6z, 6A1_UC, 6H1_TOC
soil.dat <- soil.dat[soil.dat$labm_code %in% c("6A1", "6_DC", "6z", "6A1_UC", "6H1_TOC"),]
hist(soil.dat$labr_value)

# remove pretty ridicules SOC values
soil.dat<- soil.dat[soil.dat$labr_value <= 15,]


# pull out the spectra data for the relevant SOC data
spectra.dat<- spectra.dat[spectra.dat$specID %in% soil.dat$spec_id,]


#combine soil and spectra data
combined.dat<- cbind(soil.dat[match(spectra.dat$specID, soil.dat$spec_id),],spectra.dat)
combined.dat[1:10,1:20]

write.csv(combined.dat, 'combined.csv')

## SPECTRAL PRE-PROCESSING

# simple plotting
plot(seq(350,2500, 1), combined.dat[1,which(names(combined.dat)=="350"):which(names(combined.dat)=="2500")], type="l", ylim=c(0,1),xlab = "nm", ylab="ref")
for (j in 2:nrow(combined.dat)){
  lines(seq(350,2500, 1),combined.dat[j,which(names(combined.dat)=="350"):which(names(combined.dat)=="2500")] )}

## just get the spectra
spectra<- combined.dat[,which(names(combined.dat)=="350"):which(names(combined.dat)=="2500")]
colnames(spectra)<- c(350:2500)

## splice correction
spectra<- rm.detector.error(spectra = spectra, wavelength = seq(350,2500,1),cuts = c(1000,1830))

# smooth
spectra<- filter_sg(spectra = spectra,n = 11,p = 2,m = 0)

# trim spectra 
spectra<- trimSpec(spectra, wavlimits = c(453,2500))

# convert to absorbance
spectra<- log(1/spectra)

# standard normal variate transformation
spectra<- snvBLC(spectra)

#combine back with soil data
mod.dat.all<- cbind(combined.dat$labr_value, spectra)
names(mod.dat.all)[1]<- "target"

#may need to do a tranformation of target variable
hist(mod.dat.all$target)
hist(log(mod.dat.all$target)) # seems better
mod.dat.all$target<- log(mod.dat.all$target+0.01)
mod.dat.all$target



# OUTLIER DETECTION AND REMOVAL
#PRINCIPAL COMPONENTS of the processed spectra
pr_spectra<-prcomp(mod.dat.all,center = T, scale. = T)

#amount of variance explained by each component
# Components 1 to 10
summary(pr_spectra)[[6]][,1:10]

# principal component scores
pr_scores <- pr_spectra$x 

##Mahalobinas distance 
#Mean PCA scores (first 6 components)
mean_pcaA <- (colMeans(pr_scores[, 1:6])) 

#Covariance matrix of PCA scores
cov_pcaA <- cov(pr_scores[, 1:6]) 

#Empty matrix to put outputs
chiMat <- matrix(NA, ncol = 3, nrow = nrow(mod.dat.all)) 

#Calculate the Mahalanobis distance 
chiMat[, 1] <- mahalanobis(pr_scores[, 1:6], mean_pcaA, cov_pcaA)

# Chi square distribution of the distances
chiMat[, 2] <- pchisq(c(chiMat[, 1]), df = 6)

plot(chiMat[, 1],chiMat[, 2], xlab= "distance", ylab="cumulative probability")

# which spectra are outliers
pcrit<- 0.99 # remove the furtherest 1% from the data
for (i in 1:nrow(chiMat)){
  if (chiMat[i,2] >= pcrit)
  {chiMat[i,3]<-0}else{chiMat[i,3]<-1}}

par(mfrow=c(1,2))
plot(chiMat[,1],chiMat[,2], xlab= "distance", ylab="cumlative prob")
points(chiMat[which(chiMat[,3]==0),1:2],pch='X',col='red')
abline(v=qchisq(pcrit,5),col="green")
plot(pr_scores[,1], pr_scores[,2], xlab="PCA 1", ylab="PCA 2", xlim=c(min(pr_scores[,1:2]), max(pr_scores[,1:2])),ylim=c(min(pr_scores[,1:2]), max(pr_scores[,1:2])))
points(pr_scores[which(chiMat[,3]==0),1:2],pch='X',col='red')

##Also remove the spectra outliers from the original data
new.mod.dat.all<-mod.dat.all[chiMat[,3] == 1,]



### PLSR MODELLING
## plsr model (no external validation)
###########################################################
#Partial least Squares model
modelDat <- data.frame(TV = new.mod.dat.all$target, NIR = I(as.matrix(new.mod.dat.all[,2:ncol(new.mod.dat.all)])))

maxc<-25# number of max components
soil.pls_model <- plsr(TV ~ NIR, maxc,data=modelDat, validation = "CV")
plot(soil.pls_model, "val") # RMSEP curves

nc<-5 # no. components to use
class(modelDat)
write.csv(modelDat, 'c:/temp/modeldata.csv')
# to predict value based on a given spectra (calibration data)
soil.pls_predict <- predict(soil.pls_model, ncomp = nc, newdata = modelDat)
# goodness of fit
gfc.pls_predict<- goof(new.mod.dat.all$target,soil.pls_predict, plot.it = T)
gfc.pls_predict


##### plsr with internal and external validation
#Partial least Squares model
# select 20% for external validation
## Random holdback
set.seed(123)
training <- sample(nrow(new.mod.dat.all), 0.80 * nrow(new.mod.dat.all))

# calibration
cal.modelDat <- data.frame(TV = new.mod.dat.all$target[training], NIR = I(as.matrix(new.mod.dat.all[training,2:ncol(new.mod.dat.all)])))
#validation
val.modelDat <- data.frame(TV = new.mod.dat.all$target[-training], NIR = I(as.matrix(new.mod.dat.all[-training,2:ncol(new.mod.dat.all)])))


maxc<-25# number of max components
soil.pls_model <- plsr(TV ~ NIR, maxc,data=cal.modelDat, validation = "CV")
plot(soil.pls_model, "val") # RMSEP curves

nc<-5 # no. components to use

# to predict value based on a given spectra (calibration data)
cal.soil.pls_predict <- predict(soil.pls_model, ncomp = nc, newdata = cal.modelDat)
# goodness of fit
gfc.pls_predict<- goof(new.mod.dat.all$target[training],cal.soil.pls_predict)
gfc.pls_predict

# to predict value based on a given spectra (validation data)
val.soil.pls_predict <- predict(soil.pls_model, ncomp = nc, newdata = val.modelDat)
# goodness of fit
v.gfc.pls_predict <- goof(new.mod.dat.all$target[-training],val.soil.pls_predict)
v.gfc.pls_predict


## END




saveRDS(soil.pls_model, 'MOdels/SOC_Model.rds') 

cal.soil.pls_predict <- predict(soil.pls_model, ncomp = nc, newdata = cal.modelDat[,1])






