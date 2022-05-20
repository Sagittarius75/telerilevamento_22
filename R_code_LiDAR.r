# R Code for  visualizing and analysing LiDAR data

# install.packages("lidR")
library(raster) #"Geographic Data Analysis and Modeling"
library(rgdal) #"Geospatial Data Abstraction Library"
library(viridis)
library(ggplot2)
library(RStoolbox)
library(lidR)

# setwd("~/lab/") # Linux
setwd("C:/lab/") # Windows
# setwd("/Users/name/Desktop/lab/") # Mac 

# load dsm 2013
dsm_2013 <- raster("2013Elevation_DigitalElevationModel-0.5m.tif")
dtm_2013 <- raster("2013Elevation_DigitalTerrainModel-0.5m.tif")

# view info about the raster.
dsm_2013

plot(dsm_2013)

chm_2013 <- dsm_2013 - dtm_2013

ggplot() + 
  geom_raster(chm_2013, mapping =aes(x=x, y=y, fill=layer)) + 
  scale_fill_viridis() +
  ggtitle("CHM 2013 San Genesio/Jenesien")
  
# load dsm 2004
dsm_2004 <- raster("2004Elevation_DigitalElevationModel-2.5m.tif")
dtm_2004 <- raster("2004Elevation_DigitalTerrainModel-2.5m.tif")

# view info about the raster.
dsm_2004

plot(dsm_2004)

chm_2004 <- dsm_2004 - dtm_2004
  
ggplot() + 
  geom_raster(chm_2004, mapping =aes(x=x, y=y, fill=layer)) + 
  scale_fill_viridis() +
  ggtitle("CHM 2004 San Genesio/Jenesien")  
  
diff_chm <- chm_2013 - chm_2004

chm_2013_resample <- resample(chm_2013, chm_2004)

diff_chm <- chm_2013_resample - chm_2004

ggplot() + 
  geom_raster(diff_chm, mapping =aes(x=x, y=y, fill=layer)) + 
  scale_fill_viridis() +
  ggtitle("diff chm 2013 - 2004 San Genesio/Jenesien")  
  
point_cloud <- readLAS("point_cloud.laz")
plot(point_cloud)




