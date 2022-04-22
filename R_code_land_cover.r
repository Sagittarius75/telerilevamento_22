# Code for generating land cover maps from satellite imagery

library(raster)
library(RStoolbox) # for classification
# install.packages("ggplot2")
# for info https://patchwork.data-imaginist.com/
libray(ggplot2)
# install.packages("patchwork")
library(patchwork)

# setwd("~/lab/") # Linux
setwd("C:/lab/") # Windows
# setwd("/Users/name/Desktop/lab/") # Mac

l92 <- brick("defor1_.jpg")
plotRGB(l92, 1, 2, 3, stretch="lin")

# Exercise: import defor2 and plot in a single window
l06 <- brick("defor2_.jpg")
par(mfrow=c(1,2))
plotRGB(l92, 1, 2, 3, stretch="lin")
plotRGB(l06, 1, 2, 3, stretch="lin")

# making a simple multiframe with ggplot2

ggRGB(l92, 1, 2, 3, stretch="lin")
ggRGB(l06, 1, 2, 3, stretch="lin")

p1 <- ggRGB(l92, 1, 2, 3, stretch="lin")
p2 <- ggRGB(l06, 1, 2, 3, stretch="lin")
p1+p2 # thanks to patchwork!
p1/p2

# classification
l92c <- unsuperClass(l92, nClasses=2)
plot(l92c$map)
# class 1: agriculture
# class 2: forest

# Exercise: classify the Landsat image from 2006
l06c <- unsuperClass(l06, nClasses=2)
plot(l06c$map)
# class 1: agriculture
# class 2: forest

# set.seed() would allow you to attain the same results ...

# frequencies
freq(l92c$map)
#     value  count
# [1,]  class 1  34134 pixels agricultural areas
# [2,]  class 2 307158 pixels forest

freq(l06c$map)
#     value  count
# [1,]  class1 178503 pixels agricultural areas
# [2,]  class2 164223 pixels forest


