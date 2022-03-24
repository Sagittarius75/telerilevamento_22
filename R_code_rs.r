# questo e' il primo script che useremo a lezione

# install.packages("raster")
library(raster)

# Settaggio cartella di lavoro
# setwd("~/lab/") # Linux
# setwd("/Users/emma/desktop/lab") #mac
setwd("C:/lab/") # windows

# import
l2011 <- brick("p224r63_2011.grd")
l2011

# plot
plot(l2011)

# https://www.r-graph-gallery.com/42-colors-names.html
cl <- colorRampPalette(c("black", "grey", "light grey")) (100)

plot(l2011, col=cl)
# dev.off()

# Landsat ETM+
# b1 = blu
# b2 = verde
# b3 = rosso
# b4 = infrarosso vicino NIR

# plot della banda del blu - B1_sre
plot(l2011$B1_sre) # trinity
# or
plot(l2011[[1]]) # neo

plot(l2011$B1_sre)
cl <- colorRampPalette(c("black", "grey", "light grey")) (100)
plot(l2011$B1_sre, col=cl)

# plot b1 from dark blue to blue to light blue
clb <- colorRampPalette(c("dark blue", "blue", "light blue")) (100)
plot(l2011$B1_sre, col=clb)

# let's export the image and let it appear in the lab folder: kind of magic
pdf("banda1.pdf")
plot(l2011$B1_sre, col=clb)
dev.off()

png("banda1.png")
plot(l2011$B1_sre, col=clb)
dev.off()

# plot b2 from dark green to green to light green
clg <- colorRampPalette(c("dark green", "green", "light green")) (100)
plot(l2011$B2_sre, col=clg)

# multiframe
par(mfrow=c(1,2))
plot(l2011$B1_sre, col=clb)
plot(l2011$B2_sre, col=clg)
dev.off()

# export multiframe
pdf("multiframe.pdf")
par(mfrow=c(1,2))
plot(l2011$B1_sre, col=clb)
plot(l2011$B2_sre, col=clg)
dev.off()

# exercise: revert the multiframe
par(mfrow=c(2,1))
plot(l2011$B1_sre, col=clb)
plot(l2011$B2_sre, col=clg)

# let's plot the first four bands
par(mfrow=c(2,2))
# blue
plot(l2011$B1_sre, col=clb)
# green
plot(l2011$B2_sre, col=clg)
# red
clr <- colorRampPalette(c("violet", "red", "pink")) (100)
plot(l2011$B3_sre, col=clr)
# NIR
clnir <- colorRampPalette(c("red", "orange", "yellow")) (100)
plot(l2011$B4_sre, col=clnir)

# Day #3

# Plot of l2011 in the NIR channel (NIR band)
clnir <- colorRampPalette(c("red", "orange", "yellow")) (100)
plot(l2011$B4_sre, col=clnir)

# Landsat ETM+
# b1 = blu
# b2 = verde
# b3 = rosso
# b4 = infrarosso vicino NIR

# plot RGB layers
plotRGB(l2011, r=3, g=2, b=1, stretch="lin")
plotRGB(l2011, r=4, g=3, b=2, stretch="lin")
plotRGB(l2011, r=3, g=4, b=2, stretch="lin")
plotRGB(l2011, r=3, g=2, b=4, stretch="lin")
plotRGB(l2011, r=3, g=4, b=2, stretch="hist")

# Excercise: build a multiframe with visible RGB 
# (linear stretch) on top of false colours 
# (histogram stretch)

par(mfrow=c(2,1))
plotRGB(l2011, r=3, g=2, b=1, stretch="lin")
plotRGB(l2011, r=3, g=4, b=2, stretch="hist")


# Excercise: upload the image from 1988
l1988 <- brick("p224r63_1988.grd")
l1988

par(mfrow=c(2,1))
plotRGB(l1988, r=4, g=3, b=2, stretch="lin")
plotRGB(l2011, r=4, g=3, b=2, stretch="lin")

