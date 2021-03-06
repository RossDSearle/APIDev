##  curl -v -F filename=atestFile.dat -F upload=@C:\Temp\LG.csv http://localhost:9278/upload
## curl -v -F filename=atestFile.dat -F upload=@C:\Temp\AusClay.png http://localhost:9278/upload

library(httr)
library(RCurl)
library(htmltidy)

server <- 'http://esoil.io/APIDev/'



# Return a list of the available spectra IDs in the library. Just those that Brendan supplied and are in the NatSoil DB at this stage
r <-  GET(paste0(server, '/SoilSpectra/availableSpectra?format=json&verbose=F'))
js <- content(r, "text")
js
jsonview::json_tree_view(js)


# Same as above but also returns the sample info held in NatSoil
r <-  GET(paste0(server, '/SoilSpectra/availableSpectra?format=json&verbose=T'))
js <- content(r, "text")
jsonview::json_tree_view(js)


# as above but some R code to deal with various return formats
r <-  GET(paste0(server, '/SoilSpectra/availableSpectra?verbose=T&format=csv'))
js <- content(r, "text")
if(r$headers$`content-type` == 'application/xml; charset=utf-8'){
  xml_view(js)
}else if(r$headers$`content-type` == 'application/csv; charset=utf-8'){
  df <- read.csv(text=js)
  View(df)
}else{
  jsonview::json_tree_view(js)
}


# Query the Spectral library to return stored data for a given soil attribute - queris ARIS and runs the spectra through the model to return a modelled spectra value
r <-  GET(paste0(server, '/SoilSpectra/querySpectra?spectraID=21403&attribute=SOC&format=json'))
js <- content(r, "text")
jsonview::json_tree_view(js)


# as above but return the data in XML format
r <-  GET(paste0(server, '/SoilSpectra/querySpectra?spectraID=21403&attribute=SOC&format=xml'))
js <- content(r, "text")
xml_view(js)


# Upload a local spectra file (.asd) and run it through a model to estimate a value - this is a post request so a bit different to the above
result <-  POST(paste0(server, '/SoilSpectra/Upload'), body = list(fileinfo = upload_file("C:/Temp/scans/raw/Archive_22670.asd"), 
                                                                    attribute='SOC',  
                                                                    longitude='151.2345', 
                                                                    latitude='-25.7777', 
                                                                    upperdepth='0.0', 
                                                                    lowerdepth='.25',
                                                                    format='json'
                                                                    ))
js <- content(result, "text")
jsonview::json_tree_view(js)






