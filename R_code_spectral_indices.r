library(raster)
# install.packages(rgdal")
# library(rgdal)

# setwd("~/lab/") # Linux
setwd("C:/lab/") # Windows
# setwd("/Users/name/Desktop/lab/") # Mac

# import the first file -> defor1_.jpg -> give it the name l1992
l1992 <- brick("defor1_.jpg")

# l1992 <- brick("p224r63_1988.grd")

plotRGB(l1992, r=1, g=2, b=3, stretch="lin")
# plotRGB(l1992, r=4, g=3, b=2, stretch="lin")

# layer 1 = NIR
# layer 2 = red
# layer 3 = green

# Exercise: import the second file -> defor2_.jpg -> give it the name l2006
l2006 <- brick("defor2_.jpg")

# l2006 <- brick("p224r63_2011.grd")

plotRGB(l2006, r=1, g=2, b=3, stretch="lin")
# plotRGB(l2006, r=4, g=3, b=2, stretch="lin")



# Excercise: plot in a multiframe the two images with one on the top of the other
par(mfrow=c(2, 1))
plotRGB(l1992, r=1, g=2, b=3, stretch="lin")
plotRGB(l2006, r=1, g=2, b=3, stretch="lin")

# DVI Difference Vegetation Index
dvi1992 = l1992[[1]] - l1992[[2]]
# or
# substitute index numbers with name


# dvi1992 = l1992[[4]] - l1992[[3]]
dvi1992


cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100) # specifying a color scheme
plot(dvi1992, col=cl)

dvi2006 = l2006[[1]] - l2006[[2]]
dvi2006
cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100) # specifying a color scheme
plot(dvi2006, col=cl)

par(mfrow=c(2, 1))
plot(dvi1992, col=cl)
plot(dvi2006, col=cl)

# DVI difference in time
dvi_dif = dvi1992 - dvi2006
cld <- colorRampPalette(c('blue','white','red'))(100) 
dev.off()
plot(dvi_dif, col=cld)

