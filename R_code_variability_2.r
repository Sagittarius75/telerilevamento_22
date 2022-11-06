# R code for calculating spatial variability based on multivariate maps

library(raster)
library(RStoolbox)
library(ggplot2)
library(patchwork)
library(viridis)

#setwd("~/lab/") # Linux
setwd("C:/lab/") # Windows
# setwd("/Users/name/Desktop/lab/") # Mac 

siml <- brick("sentinel.png")

# NIR 1
# red
# green

ggRGB(siml, 1, 2, 3)
ggRGB(siml, 3, 1, 2)

# Exercise: calculate a PCA on the image

siml_pca <- rasterPCA(siml)

# $call
# $model
# $map

# Exercise: view how much variance is explained by each component

summary(siml_pca$model)

ggplot

g1 <- ggplot() +
geom_raster(siml_pca$map, mapping=aes(x=x, y=y, fill=PC1)) +
scale_fill_viridis(option = "inferno") +
ggtitle("PC1")

g1

g3 <- ggplot() +
geom_raster(siml_pca$map, mapping=aes(x=x, y=y, fill=PC3)) +
scale_fill_viridis(option = "inferno") +
ggtitle("PC3")

g3

g1+g3

# Exercise: insert the secod component in the graph

g2 <- ggplot() +
geom_raster(siml_pca$map, mapping=aes(x=x, y=y, fill=PC2)) +
scale_fill_viridis(option = "inferno") +
ggtitle("PC2")

g1+g2+g3

# let's calcualte variability

pc1 <- siml_pca$map[[1]]

sd3 <- focal(pc1, matrix(1/9, 3, 3), fun=sd)

ggplot() +
geom_raster(sd3, mapping=aes(x=x, y=y, fill=layer)) +
scale_fill_viridis(option = "inferno") +
ggtitle("Standard deviation of PC1")









