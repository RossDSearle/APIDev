library(plumber)
library(htmltidy)
library(swagger)

machineName <- as.character(Sys.info()['nodename'])

if(machineName == 'FANCY-DP'){
  deployRootDir <- 'C:/Users/sea084/Dropbox/RossRCode/Git/APIDev'
  server <- '127.0.0.0'
}else{
  deployRootDir <- '/srv/plumber/APIDev'
  server <- 'http://esoil.io'
}



r <- plumb(paste0(deployRootDir, "/AllDevApiEndpoints.R")) 
print(r)



# deploy
portNum <- 8029
options("plumber.host" = "0.0.0.0")
options("plumber.apiHost" = "0.0.0.0")

# debug
# portNum <- 8028
#server <<- '127.0.0.1'



r$run(host=server, port=portNum, swagger=TRUE)
