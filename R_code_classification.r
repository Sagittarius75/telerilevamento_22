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



