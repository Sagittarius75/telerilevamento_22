
library(raster)
library(RStoolbox)
library(ggplot2)
library(patchwork)
library(viridis)


# setwd("~/telerilevamento_exam/vaia") # Linux
setwd("C:/telerilevamento_exam/vaia_crop_02") # Windows
# setwd("/Users/name/Desktop/telerilevamento_exam/vaia") # Mac 

vlist2018 <- list.files(pattern="T32TPS_2018")
vimport2018 <- lapply(vlist2018, raster)
prevaia2018 <- brick(vimport2018[[4]], vimport2018[[3]], vimport2018[[2]])

vlist2019 <- list.files(pattern="T32TPS_2019")
vimport2019 <- lapply(vlist2019, raster)
postvaia2019 <- brick(vimport2019[[4]], vimport2019[[3]], vimport2019[[2]])

# layer 1 = NIR
# layer 2 = red
# layer 3 = green

# prevaia2018res <- aggregate(prevaia2018, fact=10)
# postvaia2019res <- aggregate(postvaia2019, fact=10)

# cir = color infrared

ggpcir2018 <- ggRGB(prevaia2018, 1, 2, 3, stretch="lin")
ggpcir2019 <- ggRGB(postvaia2019, 1, 2, 3, stretch="lin")

ggpcir2018 + ggpcir2019

dvi2018 = prevaia2018[[1]] - prevaia2018[[2]]
dvi2019 = postvaia2019[[1]] - postvaia2019[[2]]

# cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100) # specifying a color scheme

# ggpdvi2018 <- ggplot() +
# geom_raster(dvi2018, mapping =aes(x=x, y=y, fill=layer)) +
# scale_fill_viridis(option = "viridis") +
# ggtitle("DVI_2018")

# ggpdvi2019 <- ggplot() +
# geom_raster(dvi2019, mapping =aes(x=x, y=y, fill=layer)) +
# scale_fill_viridis(option = "viridis") +
# ggtitle("DVI_2019")

# pdf("VAIA_comparison.pdf")
# (ggpcir2018 + ggpdvi2018) / (ggpcir2019 + ggpdvi2019)
# dev.off()

# cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100) # specifying a color scheme

# par(mfrow=c(1, 2))
# plot(dvi2018, col=cl)
# plot(dvi2019, col=cl)

# dvi_dif = dvi2018 - dvi2019
# cld <- colorRampPalette(c('blue','white','red'))(100) 

# pdf("VAIA_dvi_diff_2018_19.pdf")
# plot(dvi_dif, col=cld) # extreme values are clouds
# dev.off()

# let's calculate NDVI

ndvi2018 = dvi2018 / (prevaia2018[[1]] + prevaia2018[[2]])
ndvi2019 = dvi2019 / (postvaia2019[[1]] + postvaia2019[[2]])

ggpndvi2018 <- ggplot() +
geom_raster(ndvi2018, mapping =aes(x=x, y=y, fill=layer)) +
scale_fill_viridis(option = "viridis") +
ggtitle("NDVI_2018")

ggpndvi2019 <- ggplot() +
geom_raster(ndvi2019, mapping =aes(x=x, y=y, fill=layer)) +
scale_fill_viridis(option = "viridis") +
ggtitle("NDVI_2019")

ggpndvi2018 + ggpndvi2019

ndvi_dif = ndvi2018 - ndvi2019
cld <- colorRampPalette(c('blue','white','red'))(100) 

# pdf("VAIA_dvi_diff_2018_19.pdf")
plot(ndvi_dif, col=cld) # extreme values are clouds
# dev.off()

ndvi_difc <- unsuperClass(ndvi_dif, nClasses=5)

par(mfrow=c(1, 2))
plot(ndvi_dif, col=cld)
plot(ndvi_difc$map)

prevaia2018c <- unsuperClass(ndvi2018, nClasses=3)
postvaia2019c <- unsuperClass(ndvi2019, nClasses=3)

freq(prevaia2018c$map)
# class 1  1090655 water, ice, hard soil
# class 2  9894220 vegetation
# class 3  2698647 soft soil

# total pixel = 13691664

freq(postvaia2019c$map)

# class 1   1387056 water, ice, hard soil
# class 2   2939317 soft soil
# class 3   9362251 vegetation

# total pixel = 13691664

par(mfrow=c(1, 2))
plot(prevaia2018c$map)
plot(postvaia2019c$map)

# pdf("VAIA_class_2018_19.pdf")
# par(mfrow=c(2, 2))
# plotRGB(prevaia2018res, r=1, g=2, b=3, stretch="lin")
# plot(prevaia2018c$map)
# plotRGB(postvaia2019res, r=1, g=2, b=3, stretch="lin")
# plot(postvaia2019c$map)
# dev.off()

perc_veg_2018 <- 9894220 * 100 / 13691664
perc_softsoil_2018 <- 2698647 * 100 / 13691664

perc_veg_2019 <- 9362251 * 100 / 13691664
perc_softsoil_2019 <- 2939317 * 100 / 13691664

# perc_veg_2018 = 72.26455
# perc_softsoil_2018 = 19.71015

# perc_veg_2019 = 68.37921
# perc_softsoil_2019 = 21.46793

class <- c("Vegetation","soft soil")
percent_2018 <- c(72.26, 19.71)
percent_2019 <- c(68.38, 21.47)

multitemporal <- data.frame(class, percent_2018, percent_2019)
multitemporal

View(multitemporal)


# par(mfrow=c(1, 2))
# plot(prevaia2018c$map)
# legend(x = "topright", legend = c("1: clouds", "2: urban", "3: perfect vegetation", "4: good vegetation", "5: water"), title = ("classes 2018"))

# plot(postvaia2019c$map)
# legend(x = "topright", legend = c("1: water", "2: clouds", "3: good vegetation", "4: urban", "5: perfect vegetation"), title = ("classes 2022"))
  
pca2018 <- rasterPCA(prevaia2018)
pca2019 <- rasterPCA(postvaia2019)

pca2018
summary(pca2018$model)

ggppc1_2018 <- ggplot() +
geom_raster(pca2018$map, mapping=aes(x=x, y=y, fill=PC1)) +
scale_fill_viridis(option = "viridis") +
ggtitle("PC1 prevaia 2018")

ggppc1_2019 <- ggplot() +
geom_raster(pca2019$map, mapping=aes(x=x, y=y, fill=PC1)) +
scale_fill_viridis(option = "viridis") +
ggtitle("PC1 postvaia 2019")

ggppc1_2018 + ggppc1_2019

pc1_2018 <- pca2018$map[[1]]
pc1_2019 <- pca2019$map[[1]]

sdpc1_2018 <- focal(pc1_2018, matrix(1/9, 3, 3), fun=sd)
sdpc1_2019 <- focal(pc1_2019, matrix(1/9, 3, 3), fun=sd)

ggpsdpc1_2018 <- ggplot() +
geom_raster(sdpc1_2018, mapping=aes(x=x, y=y, fill=layer)) +
scale_fill_viridis(option = "viridis") +
ggtitle("SD prevaia 2018")

ggpsdpc1_2019 <- ggplot() +
geom_raster(sdpc1_2019, mapping=aes(x=x, y=y, fill=layer)) +
scale_fill_viridis(option = "viridis") +
ggtitle("SD postvaia 2019")

ggpsdpc1_2018 + ggpsdpc1_2019

nir2018 <- prevaia2018[[1]]
nir2019 <- postvaia2019[[1]]

sdnir2018 <- focal(nir2018, matrix(1/9, 3, 3), fun=sd)
# sdnir2018 <- focal(nir2018, w=matrix(1/9, nrow=3, ncol=3), fun=sd)
sdnir2019 <- focal(nir2019, matrix(1/9, 3, 3), fun=sd)

clsd <- colorRampPalette(c('blue','green','pink','magenta','orange','brown','red','yellow'))(100) # 

par(mfrow=c(1,2))
plot(sdnir2018, col=clsd)
plot(sdnir2019, col=clsd)

# plotting with ggplot and viridis

ggpnir2018 <- ggplot() +
geom_raster(nir2018, mapping = aes(x=x, y=y, fill=T32TPR_20180718T101031_B08_10m)) +
scale_fill_viridis(option = "viridis") +
ggtitle("NIR 2018 viridis")

ggpnir2019 <- ggplot() +
geom_raster(nir2019, mapping = aes(x=x, y=y, fill=T32TPR_20220722T100559_B08_10m)) +
scale_fill_viridis(option = "viridis") +
ggtitle("NIR 2019 viridis")

ggpsdnir2018 <- ggplot() +
geom_raster(sdnir2018, mapping = aes(x=x, y=y, fill=layer)) +
scale_fill_viridis(option = "viridis") +
ggtitle("Standard Deviation NIR 2018 viridis")

ggpsdnir2019 <- ggplot() +
geom_raster(sdnir2019, mapping = aes(x=x, y=y, fill=layer)) +
scale_fill_viridis(option = "viridis") +
ggtitle("Standard Deviation NIR 2019 viridis")

(ggpsdnir2018 + ggpsdpc1_2018) / (ggpsdnir2019 + ggpsdpc1_2019)

