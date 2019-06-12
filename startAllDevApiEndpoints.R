library(plumber)
library(htmltidy)
library(swagger)

#setwd('C:/Users/sea084/Dropbox/RossRCode/Git/apiDev/AgX')
#getwd()
source('apiDev_Config.R')



# cat("\n")
# cat("\n")
# cat("You are now running the AgX Web Services API\n")
# cat("\n")
# cat("API uri root is '/AgX'\n")
# cat("\n")
# cat("Currently supported API endpoints are \n")
# cat("    - SoilMap\n")
# cat("\n")
# cat("Some usage examples...\n")
# cat("-----------------------------\n")
# cat(server, ":", portNum, "/AgX/SoilMap\n", sep = '')
# 
# cat("Running the swagger UI at http://",server,":", portNum, "/__swagger__/\n", sep = '')
# cat("\n", sep = '')


r <- plumb(paste0(apiDevRootDir, "/AllDevApiEndpoints.R")) 
print(r)

 
 
 # root <- plumber$new()
 # 
 # 
 # users <- plumber$new(paste0(apiDevRootDir, "/AgX/AgXApiEndpoints2.R"))
#  r$mount("/users", users)
  
  options("plumber.host" = server)
  options("plumber.apiHost" = server)
  options("plumber.port" = portNum)
  options("plumber.apiPort" = portNum)
  
  if(machineName == 'FANCY-DP'){
    if("package:htmltidy" %in% search() ){
      viewer <- getOption("viewer")
      viewer(paste0('http://127.0.0.1:', portNum, '/__swagger__/'))
    }
  }
  r$run(host=server, port=portNum, swagger=TRUE)
  
 




