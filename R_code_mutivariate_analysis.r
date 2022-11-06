# R code for multivariate analysis

library(raster)
library(RStoolbox)
library(ggplot2)
library(patchwork)
library(viridis)


# setwd("~/lab/") # Linux
setwd("C:/lab/") # Windows
# setwd("/Users/name/Desktop/lab/") # Mac 

p224r63_2011 <- brick("p224r63_2011_masked.grd")

plot(p224r63_2011)

p224r63_2011

# aggregate cells: resampling (ricampionamento)
p224r63_2011res <- aggregate(p224r63_2011, fact=10)

g1 <- ggRGB(p224r63_2011, 4, 3, 2)
g2 <- ggRGB(p224r63_2011res, 4, 3, 2)

g1+g2

# aggressive resampling
p224r63_2011res100 <- aggregate(p224r63_2011, fact=100)

g3 <- ggRGB(p224r63_2011res100, 4, 3, 2)

g1+g2+g3

# PCA analisys
p224r63_2011respca <- rasterPCA(p224r63_2011res)
p224r63_2011respca

summary(p224r63_2011respca$model)
plot(p224r63_2011respca$map)

g1 <- ggplot() +
geom_raster(p224r63_2011respca$map, mapping =aes(x=x, y=y, fill=PC1)) +
scale_fill_viridis(option = "inferno") +
ggtitle("PC1")

g2 <- ggplot() +
geom_raster(p224r63_2011respca$map, mapping =aes(x=x, y=y, fill=PC7)) +
scale_fill_viridis(option = "inferno") +
ggtitle("PC7")

g1+g2

g3 <- ggplot() +
geom_raster(p224r63_2011res, mapping =aes(x=x, y=y, fill=B4_sre)) +
scale_fill_viridis(option = "inferno") +
ggtitle("B4_sre")

g1+g3

g4 <- ggRGB(p224r63_2011res, 4, 3, 2)

g1+g4

g4 <- ggRGB(p224r63_2011, 4, 3, 2)

g1+g4

plotRGB(p224r63_2011, 2, 3, 4, stretch="hist")

g4 <- ggRGB(p224r63_2011, 2, 3, 4, stretch="hist")

g1+g4


plotRGB(p224r63_2011respca$map, 1, 2, 3, stretch="lin")
plotRGB(p224r63_2011respca$map, 5, 6, 7, stretch="lin")









