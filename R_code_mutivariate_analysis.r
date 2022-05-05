# R code for multivariate analysis

library(raster)
library(RStoolbox)
library(ggplot2)
library(patchwork)



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
