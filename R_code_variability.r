# R code variability

library(raster)
library(RStoolbox) for image viewing and variability calculation
# install.packages("RStoolbox")
library(ggplot2) # for ggplot plotting
# install.packages("gridExtra")
library(gridExtra) # for plotting ggplots together
# install.packages("viridis")
library(viridis) # for ggplot colouring
library(patchwork)


# setwd("~/lab/") # Linux
setwd("C:/lab/") # Windows
# setwd("/Users/name/Desktop/lab/") # Mac

# Exercise: import the Similaun image

sen <- brick("sentinel.png")

# Exercise: plot the image by the ggRGB function
ggRGB(sen, 1, 2, 3, stretch="lin") # stretch is done automatically
# or ggRGB(sen, 1, 2, 3)

ggRGB(sen, 2, 1, 3)

# Exercise: plot the two graphs one beside the other
g1 <- ggRGB(sen, 1, 2, 3)
g2 <- ggRGB(sen, 2, 1, 3)
g1+g2 # thanks to patchwork!

# calculation of variability over NIR
nir <- sen[[1]]

sd3 <- focal(nir, matrix(1/9, 3, 3), fun=sd)
# sd3 <- focal(nir, w=matrix(1/9, nrow=3, ncol=3), fun=sd)

clsd <- colorRampPalette(c('blue','green','pink','magenta','orange','brown','red','yellow'))(100) # 
plot(sd3, col=clsd)

# plotting with ggplot
ggplot() +
geom_raster(sd3, mapping = aes(x=x, y=y, fill=layer))

# with viridis
ggplot() +
geom_raster(sd3, mapping = aes(x=x, y=y, fill=layer)) +
scale_fill_viridis() +
ggtitle("Standard deviation by viridis")

# cividis
ggplot() +
geom_raster(sd3, mapping = aes(x=x, y=y, fill=layer)) +
scale_fill_viridis(option = "cividis") +
ggtitle("Standard deviation by viridis")

# magma
ggplot() +
geom_raster(sd3, mapping = aes(x=x, y=y, fill=layer)) +
scale_fill_viridis(option = "magma") +
ggtitle("Standard deviation by viridis")

# exercise: make the same calculation with a 7x7 window
sd7 <- focal(nir, matrix(1/49, 7, 7), fun=sd)


















