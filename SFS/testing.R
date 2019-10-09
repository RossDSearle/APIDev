library(httr)
library(lubridate)
library(stringr)
library(raster)


dts <-  seq(as.Date("2019/1/1"), as.Date("2019/10/1"), 3)

for (i in 1: length(dts))
{
 print(i)
    dt <- dts[i]
    yr <- year(dt)
    mn <- str_pad(month(dt), 2, pad = "0")
    dy <- str_pad(day(dt), 2, pad = "0")
    fdt <- paste0(yr, '-', mn, '-', dy)
    bob <- GET(paste0('http://127.0.0.1:8767/SoilMoisture/GetMap?Depth=100&Region=SFS&Date=', fdt), write_disk(paste0("c:/temp/sfs/SM_100_", fdt, ".tif"), overwrite=TRUE))
    r <- raster(paste0("c:/temp/sfs/SM_100_", fdt, ".tif"))
    plot(r)

}
