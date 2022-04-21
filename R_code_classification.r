library(raster)
# install.packages("RStoolbox")
library(RStoolbox)

# setwd("~/lab/") # Linux
setwd("C:/lab/") # Windows
# setwd("/Users/name/Desktop/lab/") # Mac 

# data import

so <- brick("Solar_Orbiter_s_first_views_of_the_Sun_pillars.jpg")
so

# plotRGB(so, r=1, g=2 ,b=3, stretch="lin")
# or
plotRGB(so, 1, 2, 3, stretch="lin")
# plotRGB(so, 1, 2, 3, stretch="hist")

# Classifying the solar data

soc <- unsuperClass(so, nClasses=3)
soc
cl <- colorRampPalette(c('yellow','black','red'))(100)
plot(soc$map, col=cl)

# set.seed R function Example: set.seed(5) to uniform random numbers in different cases
# set.seed(5)

######

# day 2 Grand Canyon

gc <- brick("dolansprings_oli_2013088_canyon_lrg.jpg")
gc

# rosso = 1
# verde = 2
# blu = 3

plotRGB(gc, r=1, g=2, b=3, stretch="lin")

# change the stretch to histogram stretching
plotRGB(gc, r=1, g=2, b=3, stretch="hist")

# classification

gcclass2 <- unsuperClass(gc, nClasses=2)
gcclass2
plot(gcclass2$map)

# exercise: classify the map with 4 classes

gcclass4 <- unsuperClass(gc, nClasses=4)
plot(gcclass4$map)

# compare the classified map with the original set

clc <- colorRampPalette(c('yellow','red','blue','black'))(100)
par(mfrow=c(2,1))
plot(gcclass4$map, col=clc)
plotRGB(gc, r=1, g=2, b=3, stretch="hist")

# windows problem with image size, possible solution
st <- stack(gc, gcclass4$map)
par(mfrow=c(2,1))
plot(st)



