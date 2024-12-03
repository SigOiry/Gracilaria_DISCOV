library(tidyverse)
library(terra)
library(sf)

msk <- "Data/shp/align_dem/Saja_align.shp" %>% read_sf()

Belon_DEM <- "Data/DEM/SajaSouth_DEM_LIDAR.tif" %>% 
  rast() %>% 
  crop(msk, mask = T)
names(Belon_DEM) <- "layer"

Belon_3D <- "Data/MDS05_Spain.tif" %>% 
  rast() %>% 
  crop(msk, mask = T)
names(Belon_3D) <- "layer"




Diff <- (Belon_3D %>% as.data.frame() %>% 
                             pull(layer) %>% 
                             mean()
                          )-
                            (Belon_DEM %>% 
                               resample(Belon_3D, method = "average") %>%
                               as.data.frame() %>% 
                               pull(layer)  %>% 
                               mean())
a <-  "Data/DEM/SajaSouth_DEM_LIDAR.tif" %>% 
  rast() + Diff

b <-  "Data/DEM/SajaNorth_DEM_LIDAR.tif" %>% 
  rast() + Diff

writeRaster(a,"Data/DEM/SajaSouth_DEM_LiDAR1.tif",overwrite = T)
writeRaster(b,"Data/DEM/SajaNorth_DEM_LiDAR1.tif",overwrite = T)
