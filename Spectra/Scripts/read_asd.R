library(asdreader)

files<- list.files(path = "c:/temp/scans/raw/", full.names = T, recursive = F)
files


#Generate an empty table to put spectra into
s1<- as.data.frame(get_spectra(files[1],type = "reflectance"))


for (i in 2:length(files)){
  tmp.spec<- as.data.frame(get_spectra(files[i],type = "reflectance"))
  s1<- rbind(s1,tmp.spec)}

str(s1)


md <- get_metadata(files[1])
sp <- get_spectra(files[1],type = "reflectance")
str(sp)
head(sp)



