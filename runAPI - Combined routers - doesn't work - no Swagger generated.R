library(plumber)
f1_pr <- plumber$new("C:/Users/sea084/Dropbox/RossRCode/Git/apiDev/apis/AgXApiEndpoints.R")
f2_pr <- plumber$new("C:/Users/sea084/Dropbox/RossRCode/Git/apiDev/apis/SFSApiEndpoints.R")

master_pr <- plumber$new()
master_pr$mount("/f1", f1_pr)
master_pr$mount("/f2", f2_pr)


machineName <- as.character(Sys.info()['nodename'])
print(machineName)

apiDevRootDir <<- 'C:/Users/sea084/Dropbox/RossRCode/Git/apiDev'

server <<- '127.0.0.1'
portNum <<- 8029

cat(server)

#r$run(host=server, port=portNum, swagger=TRUE)

master_pr$swaggerFile()
master_pr$run(host=server, port=portNum, swagger=T)



p <- plumb("C:/Users/sea084/Dropbox/RossRCode/Git/apiDev/apis/AgXApiEndpoints.R")
p$run(host=server, port=portNum, swagger=T)

plumber$new()