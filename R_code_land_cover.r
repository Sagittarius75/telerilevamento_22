# Code for generating land cover maps from satellite imagery

library(raster)
library(RStoolbox) # for classification
# install.packages("ggplot2")
# for info https://patchwork.data-imaginist.com/
library(ggplot2)
# install.packages("patchwork")
library(patchwork)
# install.packages("gridExtra")
library(gridExtra) # for grid.arrange plotting

# setwd("~/lab/") # Linux
setwd("C:/lab/") # Windows
# setwd("/Users/name/Desktop/lab/") # Mac

# NIR 1
# r 2
# g 3

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

l92
tot92 <- 341292

# proportion of classes
prop_forest_92 <- 305213 / tot92

# percent of classes
perc_forest_92 <- 305213 * 100 / tot92

# Exercise: calculate the percentage of agricultural areas in 1992

# method 1
perc_agr_92 <- 100 - perc_forest_92

# method 2
perc_agr_92 <- 36079 * 100 / tot92

# percent_forest_92: 89.4287
# percent_agr_92: 10.5713

freq(l06c$map)
#     value  count
# [1,]  class1 164223 pixels agricultural areas
# [2,]  class2 178503 pixels forest

# percentage 2006

l06
tot06 <- 342726

# percent of classes
perc_forest_06 <- 178503 * 100 / tot06

# Exercise: calculate the percentage of agricultural areas in 1992

# method 1
perc_agr_06 <- 100 - perc_forest_06

# method 2
perc_agr_92 <- 164223 * 100 / tot06

# percent_forest_06: 52.0833  
# percent_agr_06: 47.9167

# Final Data:
# percent_forest_92: 89.4287
# percent_agr_92: 10.5713
# percent_forest_06: 52.0833
# percent_agr_06: 47.9167 

# build a dataframe

class <- c("Forest","Agriculture")
percent_1992 <- c(89.43, 10.57)
percent_2006 <- c(52.08, 47.92)

multitemporal <- data.frame(class, percent_1992, percent_2006)
multitemporal

# table
View(multitemporal)

# let's plot them!

# aes = aestetics = columns

# 1992
# pdf
# pdf("percentages_1992.pdf")
ggplot(multitemporal, aes(x=class, y=percent_1992, color=class)) + geom_bar(stat="identity", fill="white")
# dev.off()

# Exercise: make the same graph for 2006
# pdf
# pdf("percentages_2006.pdf")
ggplot(multitemporal, aes(x=class, y=percent_2006, color=class)) + geom_bar(stat="identity", fill="white")
# dev.off()

# no at lesson
# p1 <- ggplot(multitemporal, aes(x=class, y=percent_1992, color=class)) + geom_bar(stat="identity", fill="white")
# p2 <- ggplot(multitemporal, aes(x=class, y=percent_2006, color=class)) + geom_bar(stat="identity", fill="white")

# grid.arrange(p1, p2, nrow=1)




