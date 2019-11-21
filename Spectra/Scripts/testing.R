##  curl -v -F filename=atestFile.dat -F upload=@C:\Temp\LG.csv http://localhost:9278/upload
## curl -v -F filename=atestFile.dat -F upload=@C:\Temp\AusClay.png http://localhost:9278/upload

library(httr)
library(RCurl)
library(htmltidy)

result2 <-  POST('http://localhost:8813/SoilSpectra/Upload', body = list(fileinfo = upload_file("C:/Temp/scans/raw/Archive_22670.asd"), 
                                                                    attribute='SOC',  
                                                                    longitude='151.2345', 
                                                                    latitude='-25.7777', 
                                                                    upperdepth='0.0', 
                                                                    lowerdepth='.25',
                                                                    format='json'
                                                                    ))
js <- content(result2, "text")
jsonview::json_tree_view(js)


# uri <- "http://localhost:7940/upload"
# file.name <- "C:/Temp/LG.csv"
# result1 <- postForm(uri, file = fileUpload(filename = file.name, contentType="text/csv"), .opts = list(ssl.verifypeer = FALSE))


r <-  GET('http://127.0.0.1:8813/SoilSpectra/querySpectra?spectraID=21403&attribute=SOC&format=json')
r <-  GET('http://127.0.0.1:8813/SoilSpectra/querySpectra?spectraID=21403&attribute=SOC&format=xml')
js <- content(r, "text")
js
jsonview::json_tree_view(js)

r <-  GET('http://127.0.0.1:8813/SoilSpectra/availableSpectra?verbose=T&format=csv')
js <- content(r, "text")
if(r$headers$`content-type` == 'application/xml; charset=utf-8'){
  xml_view(js)
}else if(r$headers$`content-type` == 'application/csv; charset=utf-8'){
  data <- read.csv(text=js)
  View(data)
}else{
  jsonview::json_tree_view(js)
}
r$headers
js
jsonview::json_tree_view(js)

