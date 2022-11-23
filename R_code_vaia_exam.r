

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
prevaia2018 <- brick(vimport2018[[4]], vimport2018[[3]], vimport2018[[2]], vimport2018[[1]])

vlist2019 <- list.files(pattern="T32TPS_2019")
vimport2019 <- lapply(vlist2019, raster)
postvaia2019 <- brick(vimport2019[[4]], vimport2019[[3]], vimport2019[[2]], vimport2019[[1]])

# layer 1 = NIR
# layer 2 = red
# layer 3 = green
# layer 4 = blue

# prevaia2018res <- aggregate(prevaia2018, fact=10)
# postvaia2019res <- aggregate(postvaia2019, fact=10)

# nc = natural color
# cir = color infrared

ggpnc2018 <- ggRGB(prevaia2018, 2, 3, 4, stretch="lin")
ggpnc2019 <- ggRGB(postvaia2019, 2, 3, 4, stretch="lin")

ggpnc2018 + ggpnc2019

ggpcir2018 <- ggRGB(prevaia2018, 1, 2, 3, stretch="lin")
ggpcir2019 <- ggRGB(postvaia2019, 1, 2, 3, stretch="lin")

ggpcir2018 + ggpcir2019

dvi2018 = prevaia2018[[1]] - prevaia2018[[2]]
dvi2019 = postvaia2019[[1]] - postvaia2019[[2]]

ggpdvi2018 <- ggplot() +
geom_raster(dvi2018, mapping =aes(x=x, y=y, fill=layer)) +
scale_fill_viridis(option = "inferno") +
ggtitle("DVI_prevaia2018")

ggpdvi2019 <- ggplot() +
geom_raster(dvi2019, mapping =aes(x=x, y=y, fill=layer)) +
scale_fill_viridis(option = "inferno") +
ggtitle("DVI_postvaia2019")

# pdf("VAIA_comparison.pdf")
ggpdvi2018 + ggpdvi2019
# dev.off()

dvi_dif = dvi2018 - dvi2019

ggpdvi_dif <- ggplot() +
geom_raster(dvi_dif, mapping =aes(x=x, y=y, fill=layer)) +
scale_fill_viridis(option = "inferno") +
ggtitle("DVI_dif")

ggpdvi_dif


# let's calculate NDVI

ndvi2018 = dvi2018 / (prevaia2018[[1]] + prevaia2018[[2]])
ndvi2019 = dvi2019 / (postvaia2019[[1]] + postvaia2019[[2]])

ggpndvi2018 <- ggplot() +
geom_raster(ndvi2018, mapping =aes(x=x, y=y, fill=layer)) +
scale_fill_viridis(option = "inferno") +
ggtitle("NDVI prevaia2018")

ggpndvi2019 <- ggplot() +
geom_raster(ndvi2019, mapping =aes(x=x, y=y, fill=layer)) +
scale_fill_viridis(option = "inferno") +
ggtitle("NDVI postvaia2019")

ggpndvi2018 + ggpndvi2019

ndvi_dif = ndvi2018 - ndvi2019

ggpndvi_dif <- ggplot() +
geom_raster(ndvi_dif, mapping =aes(x=x, y=y, fill=layer)) +
scale_fill_viridis(option = "inferno") +
ggtitle("NDVI_dif")

ggpndvi_dif

set.seed(2)
ndvi_difc <- unsuperClass(ndvi_dif, nClasses=3)

ggpndvi_difc <- ggplot() +
geom_raster(ndvi_difc$map, mapping =aes(x=x, y=y, fill=class)) +
scale_fill_viridis(option = "inferno") +
ggtitle("NDVI_dif_classes")

ggpndvi_difc

freq(ndvi_difc$map)

# class 1: 364324 pixels - forest damage

fstdmgpx <- 364324  # forest damage
imgtotpx <- 3607749  # image total number of pixels
imgarea <- 360.77  # image area (km square)

fstdmgperc <- fstdmgpx * 100 / imgtotpx  # forest damage (percent)
imgareadmg <- imgarea * fstdmgperc / 100  # image area damage km square

class <- c("Forest damage (%)", "img area (km square)", "img area damage (km square)")
values <- c(fstdmgperc, imgarea, imgareadmg)

multitemporal <- data.frame(class, values)
multitemporal

View(multitemporal)

# prevaia2018c <- unsuperClass(ndvi2018, nClasses=3)
# postvaia2019c <- unsuperClass(ndvi2019, nClasses=3)

# freq(prevaia2018c$map)
# class 1  
# class 2  
# class 3  

# total pixel = 

# freq(postvaia2019c$map)

# class 1   
# class 2   
# class 3   

# total pixel = 

# par(mfrow=c(1, 2))
# plot(prevaia2018c$map)
# plot(postvaia2019c$map)


# perc_veg2018 <- _______ * 100 / _______
# perc_soil2018 <- _______ * 100 / _______

# perc_veg2019 <- _______ * 100 / _______
# perc_soil2019 <- ______ * 100 / ________

# perc_veg2018 = _____
# perc_soil2018 = _______

# perc_veg2019 = _______
# perc_soil2019 = ______

# class <- c("Vegetation","soil")
# percent2018 <- c(_____, _____)
# percent2019 <- c(_____, _____)

# multitemporal <- data.frame(class, percent2018, percent2019)
# multitemporal

# View(multitemporal)


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
