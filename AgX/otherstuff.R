

#' #' @get /ftypes
#' ftypes <- function(res){
#'   
#'   tmp <- 'C:/Temp/outpost1.zip'
#'   include_file('C:/Temp/outpost1.zip', res)
#' 
#' }


#* Download a binary file.
#* @serializer contentType list(type="application/octet-stream")
#* @get /download-binary
# ftypes2 <- function(res){
# 
#   x <- list(a=123, b="hi!")
#   tmp <- tempfile(fileext=".rds")
#   saveRDS(x, tmp)
#   res$setHeader("Content-Disposition", "attachment; filename=mydata.Rds")
#   bin <- readBin(tmp, "raw", n=file.info(tmp)$size)
#   file.remove(tmp)
#   bin
# }


#* Download a binary file.
#* @serializer contentType list(type="application/octet-stream")
#* @get /download-zip
# ftypes3 <- function(res){
# 
#   x <- list(a=123, b="hi!")
#   tmp <- tempfile(fileext=".rds")
#   saveRDS(x, tmp)
# 
#   res$setHeader("Content-Disposition", "attachment; filename=mydata.tif")
# 
#   bin <- readBin(tmp, "raw", n=file.info(tmp)$size)
#   tmp <- 'C:/Temp/outpost1.zip'
#   tmp <- 'C:/Projects/AgDataShop/SLGA/Covariates/Soil/Soil_Kaolinite.tif'
#   bin <- readBin(tmp, "raw", n=file.info(tmp)$size)
# 
#   bin
# }

