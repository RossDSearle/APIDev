#' @title gdal_polygonizeR
#' @description R Wrapper for the gdal_polygonize.py python script (http://www.gdal.org/gdal_polygonize.html)
#' This utility creates vector polygons for all connected regions of pixels in the raster sharing
#' a common pixel value. Each polygon is created with an attribute indicating the pixel value of
#' that polygon. Can be userful for example to create fishnet polygons (see "create_fishnet")
#'
#' @param x          `character`filename of a raster file, or "R" raster object
#' @param outshape   `character`filename of the desired output polygon shapefile
#' @param gdalformat  defaults to ESRI - don't change
#' @param pypath     `character` path of python  - if `NULL` (the default) the script tries to 
#' automatically retrieve it
#' @param readpoly   `logical` If TRUE sends back the shapefile as a SpataialPolygonsDataFrame to R
#'     (defaults to FALSE)
#' @param quiet      `logical` if TRUE, limit the messages (defaults to TRUE)
#' @param overwrite  `logical` If TRUE overwrite shapefile if already existing (defaults to FALSE)
#'
#' @return NULL, or a SpataialPolygonsDataFrame (if readpoly = TRUE)
#' @export
#' @importFrom raster writeRaster
#' @importFrom methods is
#' @author Original code by Jonh Baumgartner
#' [https://johnbaumgartner.wordpress.com/2012/07/26/getting-rasters-into-shape-from-r/](), with
#' slight modifications by L.Busetto


gdal_polygonizeR <- function(x, 
                             outshape   = NULL, 
                             gdalformat = 'ESRI Shapefile',
                             pypath     = NULL, 
                             readpoly   = TRUE, 
                             quiet      = TRUE, 
                             overwrite  = FALSE) {
  
  if (is.null(pypath)) {
    pypath <- Sys.which('gdal_polygonize.py')
  }
  
  if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.")
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dirname(pypath))
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep = '.'))
    if (any(f.exists)) {
      if (overwrite == FALSE) {
        stop(sprintf('File already exists: %s',
                     toString(paste(outshape, c('shp', 'shx', 'dbf'),
                                    sep = '.')[f.exists])), call.=FALSE)
      } else (
        unlink(paste(outshape, c('shp', 'shx', 'dbf')))
      )
    }
    if (methods::is(x, 'Raster')) {
      
      raster::writeRaster(x, {f <- tempfile(fileext = '.tif')})
      rastpath <- normalizePath(f)
    } else if (is.character(x)) {
      rastpath <- normalizePath(x)
    } else stop('x must be a file path (character string), or a Raster object.')
    
    system2('python', args = (paste0(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"',
                                             pypath, rastpath, gdalformat, outshape), " -fieldname id")))
    if (isTRUE(readpoly)) {
      shp <- readshape(outshape)
      return(shp)
    }
    return(NULL)
  }
}

library(raster)
library(sf)


shapePath <- 'C:/Projects/AgDataShop/TimsPlace/pdksWM.shp'
fstm <- 'C:/Projects/GenericSoilGroups/soil_forestclassifier_most_likely.tif'

bdy <- read_sf(shapePath)
s3 <- bdy %>% st_transform(4326)

r <- raster(fstm)
e <- raster::extent(s3)
rc <- raster::crop(r,extent( e@xmin, e@xmax, e@ymin, e@ymax)  )

polys <- rasterToPolygons(rc, dissolve =T)

plot(polys)


Sys.setenv(PYTHONHOME="C:/Python36")  # `A+C` could also be used
Sys.setenv(PYTHONPATH="c:/Python36/Lib")

Sys.unsetenv("R_TEST")  # may warn and not succeed
Sys.getenv("R_TEST", unset = NA)

set PYTHONHOME="C:/Python36"
set PYTHONPATH=c:/Python36/Lib
set PATH=%PYTHONHOME%;%PATH%
  
  
  
gdal_polygonizeR( rc,
  outshape   = 'c:/temp/rp.shp', 
  gdalformat = 'ESRI Shapefile',
  pypath     = 'C:/Program Files/GDAL/gdal_polygonize.py', 
  #pypath     = 'C:/Python36/',
  readpoly   = F, 
  quiet      = TRUE, 
  overwrite  = T) 






library(raster)
library(sf)
library(spex)
rt <- raster('C:/Projects/AgDataShop/CropMaps/vicCrps.tif')
plot(rt)

polys <- polygonize(rt)

write_sf(polys, 'c:/temp/vic.shp')

p2 <- polys
p2 %>%  
  split(.$vicCrps) %>% 
  lapply(st_union) %>% 
  do.call(c, .) %>% # bind the list element to a single sfc
  st_cast()  # mapview doesn't like GEOMETRY -> cast to MULTIPOLYGON





polygonizer(rt, 'c:/temp/aaaaaatest.shp', readpoly=F)

polygonizer <- function(x, outshape=NULL, gdalformat = 'ESRI Shapefile', 
                        pypath=NULL, readpoly=TRUE, quietish=TRUE) {
  # x: an R Raster layer, or the file path to a raster file recognised by GDAL
  # outshape: the path to the output shapefile (if NULL, a temporary file will be created)
  # gdalformat: the desired OGR vector format
  # pypath: the path to gdal_polygonize.py (if NULL, an attempt will be made to determine the location
  # readpoly: should the polygon shapefile be read back into R, and returned by this function? (logical)
  # quietish: should (some) messages be suppressed? (logical)
  if (isTRUE(readpoly)) require(rgdal)
  if (is.null(pypath)) {
    pypath <- Sys.which('gdal_polygonize.py')
  }
  ## The line below has been commented:
  # if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.") 
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dirname(pypath))
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
    if (any(f.exists)) 
      stop(sprintf('File already exists: %s', 
                   toString(paste(outshape, c('shp', 'shx', 'dbf'), 
                                  sep='.')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  if (is(x, 'Raster')) {
    require(raster)
    writeRaster(x, {f <- tempfile(fileext='.tif')})
    rastpath <- normalizePath(f)
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop('x must be a file path (character string), or a Raster object.')
  
  ## Now 'python' has to be substituted by OSGeo4W
  #system2('python',
  system2('C:\\OSGeo4W64\\OSGeo4W.bat',
          args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"', 
                        pypath, rastpath, gdalformat, outshape)))
  if (isTRUE(readpoly)) {
    shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose=!quietish)
    return(shp) 
  }
  return(NULL)
}




