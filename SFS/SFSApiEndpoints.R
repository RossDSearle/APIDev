
#library(geojson)

#source('C:/Users/sea084/Dropbox/RossRCode/Git/apiDev/AgX/AgXMethods.R')

#* @apiTitle ASIS API Development
#* @apiDescription Some web service endpoints to demonstrate some concepts


#* Returns a test

#* @param test (Optional) - not needed for this demo
#* @tag AgX_Demo_Download_Soil_Polygons
#' @html
#' @get /AgX/SoilMap/geojson
apiTest <- function(res, test){
  
   tryCatch({

 "Hi
     "
}, error = function(res)
{
  print(geterrmessage())
  res$status <- 400
  list(error=jsonlite::unbox(geterrmessage()))
})
  
}










