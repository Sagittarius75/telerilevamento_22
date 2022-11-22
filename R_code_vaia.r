
library(raster)
library(RStoolbox)
library(ggplot2)
library(patchwork)
library(viridis)


# setwd("~/telerilevamento_exam/vaia") # Linux
setwd("C:/telerilevamento_exam/vaia") # Windows
# setwd("/Users/name/Desktop/telerilevamento_exam/vaia") # Mac 

vlist2018 <- list.files(pattern="T32TPR_2018")
vimport2018 <- lapply(vlist2018, raster)
prevaia2018 <- brick(vimport2018)

vlist2022 <- list.files(pattern="T32TPR_2022")
vimport2022 <- lapply(vlist2022, raster)
postvaia2022 <- brick(vimport2022)

# layer 1 = blue
# layer 2 = green
# layer 3 = red
# layer 4 = NIR

# prevaia2018st <- stack(vimport2018)
# postvaia2022st <- stack(vimport2022)

prevaia2018res <- aggregate(prevaia2018, fact=10)
postvaia2022res <- aggregate(postvaia2022, fact=10)


# nc = natural color
# cir = color infrared

ggpnc2018 <- ggRGB(prevaia2018res, 3, 2, 1, stretch="lin")
ggpnc2022 <- ggRGB(postvaia2022res, 3, 2, 1, stretch="lin")

ggpcir2018 <- ggRGB(prevaia2018res, 4, 3, 2, stretch="lin")
ggpcir2022 <- ggRGB(postvaia2022res, 4, 3, 2, stretch="lin")

(ggpnc2018 + ggpcir2018) / (ggpnc2022 + ggpcir2022)

dvi2018 = prevaia2018res[[4]] - prevaia2018res[[3]]
dvi2022 = postvaia2022res[[4]] - postvaia2022res[[3]]

# cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100) # specifying a color scheme

# ggpdvi2018 <- ggplot() +
# geom_raster(dvi2018, mapping =aes(x=x, y=y, fill=layer)) +
# scale_fill_viridis(option = "viridis") +
# ggtitle("DVI_2018")

# ggpdvi2022 <- ggplot() +
# geom_raster(dvi2022, mapping =aes(x=x, y=y, fill=layer)) +
# scale_fill_viridis(option = "viridis") +
# ggtitle("DVI_2022")

# pdf("VAIA_comparison.pdf")
# (ggpnc2018 + ggpcir2018 + ggpdvi2018) / (ggpnc2022 + ggpcir2022 + ggpdvi2022)
# dev.off()

# cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100) # specifying a color scheme

# par(mfrow=c(1, 2))
# plot(dvi2018, col=cl)
# plot(dvi2022, col=cl)

# dvi_dif = dvi2018 - dvi2022
# cld <- colorRampPalette(c('blue','white','red'))(100) 

# pdf("VAIA_dvi_diff_2018_22.pdf")
# plot(dvi_dif, col=cld) # extreme values are clouds
# dev.off()

ndvi2018 = dvi2018 / (prevaia2018res[[4]] + prevaia2018res[[3]])
ndvi2022 = dvi2022 / (postvaia2022res[[4]] + postvaia2022res[[3]])

ggpndvi2018 <- ggplot() +
geom_raster(ndvi2018, mapping =aes(x=x, y=y, fill=layer)) +
scale_fill_viridis(option = "viridis") +
ggtitle("NDVI_2018")

ggpndvi2022 <- ggplot() +
geom_raster(ndvi2022, mapping =aes(x=x, y=y, fill=layer)) +
scale_fill_viridis(option = "viridis") +
ggtitle("NDVI_2022")

ggpndvi2018 + ggpndvi2022

ndvi_dif = ndvi2018 - ndvi2022
cld <- colorRampPalette(c('blue','white','red'))(100) 

# pdf("VAIA_dvi_diff_2018_22.pdf")
plot(ndvi_dif, col=cld) # extreme values are clouds
# dev.off()

# par(mfrow=c(1, 2))
# plotRGB(prevaia2018res, r=4, g=3, b=2, stretch="lin")
# plotRGB(postvaia2022res, r=4, g=3, b=2, stretch="lin")

prevaia2018c <- unsuperClass(ndvi2018, nClasses=3)
postvaia2022c <- unsuperClass(ndvi2022, nClasses=3)

freq(prevaia2018c$map)
# class 1 790455 vegetation
# class 2 123785 water, clouds, urban
# class 3 291363 ground, urban
   
# total pixel = 1205604

freq(postvaia2022c$map)

# class 1   257416 water, clouds, urban
# class 2   387494 ground, urban
# class 3   560694 vegetation
    
# total pixel = 1205604

par(mfrow=c(1, 2))
# plotRGB(prevaia2018res, r=3, g=2, b=1, stretch="lin")
plot(prevaia2018c$map)

# par(mfrow=c(1, 2))
# plotRGB(postvaia2022res, r=3, g=2, b=1, stretch="lin")
plot(postvaia2022c$map)

# pdf("VAIA_class_2018_22.pdf")
 par(mfrow=c(2, 2))
 plotRGB(prevaia2018res, r=4, g=3, b=2, stretch="lin")
 plot(prevaia2018c$map)
 plotRGB(postvaia2022res, r=4, g=3, b=2, stretch="lin")
 plot(postvaia2022c$map)
# dev.off()

perc_veg_2018 <- 790455 * 100 / 1205604
perc_urb_2018 <- (123785 + 291363) * 100 / 1205604

perc_veg_2022 <- 560694 * 100 / 1205604
perc_urb_2022 <- (257416 + 387494) * 100 / 1205604

# perc_veg_2018 = 65.56506
# perc_urb_2018 = 34.43486

# perc_veg_2022 = 46.50731
# perc_urb_2022 = 53.49269

class <- c("Vegetation","Urban")
percent_2018 <- c(65.56, 34.44)
percent_2022 <- c(46.51, 53.49)

multitemporal <- data.frame(class, percent_2018, percent_2022)
multitemporal

View(multitemporal)


# par(mfrow=c(1, 2))
# plot(prevaia2018c$map)
# legend(x = "topright", legend = c("1: clouds", "2: urban", "3: perfect vegetation", "4: good vegetation", "5: water"), title = ("classes 2018"))

# plot(postvaia2022c$map)
# legend(x = "topright", legend = c("1: water", "2: clouds", "3: good vegetation", "4: urban", "5: perfect vegetation"), title = ("classes 2022"))
  
pca2018 <- rasterPCA(prevaia2018res)
pca2022 <- rasterPCA(postvaia2022res)

pca2018
summary(pca2018$model)

ggppc1_2018 <- ggplot() +
geom_raster(pca2018$map, mapping=aes(x=x, y=y, fill=PC1)) +
scale_fill_viridis(option = "viridis") +
ggtitle("PC1 prevaia 2018")

ggppc1_2022 <- ggplot() +
geom_raster(pca2022$map, mapping=aes(x=x, y=y, fill=PC1)) +
scale_fill_viridis(option = "viridis") +
ggtitle("PC1 postvaia 2022")

ggppc1_2018 + ggppc1_2022

pc1_2018 <- pca2018$map[[1]]
pc1_2022 <- pca2022$map[[1]]

sdpc1_2018 <- focal(pc1_2018, matrix(1/9, 3, 3), fun=sd)
sdpc1_2022 <- focal(pc1_2022, matrix(1/9, 3, 3), fun=sd)

ggpsdpc1_2018 <- ggplot() +
geom_raster(sdpc1_2018, mapping=aes(x=x, y=y, fill=layer)) +
scale_fill_viridis(option = "viridis") +
ggtitle("SD prevaia 2018")

ggpsdpc1_2022 <- ggplot() +
geom_raster(sdpc1_2022, mapping=aes(x=x, y=y, fill=layer)) +
scale_fill_viridis(option = "viridis") +
ggtitle("SD postvaia 2022")

ggpsdpc1_2018 + ggpsdpc1_2022

nir2018 <- prevaia2018res[[4]]
nir2022 <- postvaia2022res[[4]]

sdnir2018 <- focal(nir2018, matrix(1/9, 3, 3), fun=sd)
# sdnir2018 <- focal(nir2018, w=matrix(1/9, nrow=3, ncol=3), fun=sd)
sdnir2022 <- focal(nir2022, matrix(1/9, 3, 3), fun=sd)

clsd <- colorRampPalette(c('blue','green','pink','magenta','orange','brown','red','yellow'))(100) # 

par(mfrow=c(1,2))
plot(sdnir2018, col=clsd)
plot(sdnir2022, col=clsd)

# plotting with ggplot and viridis

ggpnir2018 <- ggplot() +
geom_raster(nir2018, mapping = aes(x=x, y=y, fill=T32TPR_20180718T101031_B08_10m)) +
scale_fill_viridis(option = "viridis") +
ggtitle("NIR 2018 viridis")

ggpnir2022 <- ggplot() +
geom_raster(nir2022, mapping = aes(x=x, y=y, fill=T32TPR_20220722T100559_B08_10m)) +
scale_fill_viridis(option = "viridis") +
ggtitle("NIR 2022 viridis")

ggpsdnir2018 <- ggplot() +
geom_raster(sdnir2018, mapping = aes(x=x, y=y, fill=layer)) +
scale_fill_viridis(option = "viridis") +
ggtitle("Standard Deviation NIR 2018 viridis")

ggpsdnir2022 <- ggplot() +
geom_raster(sdnir2022, mapping = aes(x=x, y=y, fill=layer)) +
scale_fill_viridis(option = "viridis") +
ggtitle("Standard Deviation NIR 2022 viridis")


