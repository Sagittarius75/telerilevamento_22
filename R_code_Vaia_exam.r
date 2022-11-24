##### Forest damage estimation after the Vaia storm (26-30 oct 2018) #####
##### between Bolzano city and Predazzo town. Study area = 360.77 km squared #####

# install.packages("PACKAGE NAME")

library(raster) 
          # Geographic Data Analysis and Modeling:
          # Reading, writing, manipulating, analyzing and modeling of spatial data

library(RStoolbox)  
          # Tools for Remote Sensing Data Analysis: 
          # Toolbox for remote sensing image processing and analysis such as calculating 
          # spectral indices, principal component transformation, unsupervised and 
          # supervised classification or fractional cover analyses

library(ggplot2)  
          # Create Elegant Data Visualisations Using the Grammar of Graphics:
          # A system for 'declaratively' creating graphics, based on "The Grammar of Graphics"

library(patchwork)  
          # The Composer of Plots:
          # The 'ggplot2' package provides a strong API (Application Programming Interfaces) 
          # for sequentially building up a plot

library(viridis)  
          # Colorblind-Friendly Color Maps for R:
          # Color maps designed to improve graph readability for readers with common forms 
          # of color blindness and/or color vision deficiency



### let's set our working directory: ###

# setwd("~/telerilevamento_exam/vaia_crop_02")  # Linux
# setwd("/Users/name/Desktop/telerilevamento_exam/vaia_crop_02")  # Mac 
setwd("C:/telerilevamento_exam/vaia_crop_02")  # Windows



### let's import sentinel-2 data from september 2018 and create our multi-layer ###
### raster object: ###

vlist2018 <- list.files(pattern="T32TPS_2018")  
        # 'list.files' Function (base package) produces a vector 
        # of the names of files which are in the working directory 
        # we chose. The Argument 'pattern' tells the initials of the file

vimport2018 <- lapply(vlist2018, raster)  
        # 'lapply' Function (raster package) applies a Function over a List or Vector. 
        # The Function we apply to the list is 'raster'

prevaia2018 <- brick(vimport2018[[4]], vimport2018[[3]], vimport2018[[2]], vimport2018[[1]])  
        # 'brick' Function (raster package) creates a RasterBrick object. A RasterBrick is a
        # multi-layer raster object in 'prevaia2018' object we decide the order of layers (4 out of 5)

### let's do the same with sentinel-2 data from september 2019 ###

vlist2019 <- list.files(pattern="T32TPS_2019")
vimport2019 <- lapply(vlist2019, raster)
postvaia2019 <- brick(vimport2019[[4]], vimport2019[[3]], vimport2019[[2]], vimport2019[[1]])

# layer 1 = NIR
# layer 2 = red
# layer 3 = green
# layer 4 = blue


### changing resolution: ###

# prevaia2018res <- aggregate(prevaia2018, fact=10)
        # the 'aggregate' Function is used to change the resolution of the resulting raster.
        # the Argument 'fact' decides the new resolution, if 'fact=10 means: 
        # (10m*10m resolution becomes 100m*100m resolution)

# postvaia2019res <- aggregate(postvaia2019, fact=10)


### let's plot using 'ggplot2' and 'patchwork' package our first objects. We plot two ###
### different band combinations ###

# nc = natural color band combinations:

ggpnc2018 <- ggRGB(prevaia2018, 2, 3, 4, stretch="lin")  
        # 'ggRGB' Function (RStoolbox package) creates Raster Plots with RGB from 3 RasterLayers. 
        # The argument 'stretch' can be used to enhance the imagery. In the object 'ggpnc2018' 
        # we pass red, green, blue bands (layers 2, 3, 4) to the red, green and blue
        # which means Natural Color band combinations

ggpnc2019 <- ggRGB(postvaia2019, 2, 3, 4, stretch="lin")

ggpnc2018 + ggpnc2019 # thanks to 'patchwork' package we can plot our objects


# cir = color infrared band combinations:

ggpcir2018 <- ggRGB(prevaia2018, 1, 2, 3, stretch="lin") 
        # In the object 'ggpcir2018' we pass near infrared, red, green bands (layers 1, 2, 3) 
        # to the red, green and blue which means Color Infared band combinations

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
imgarea <- 360.77  # image area (km squared)

fstdmgperc <- fstdmgpx * 100 / imgtotpx  # forest damage (percent)
imgareadmg <- imgarea * fstdmgperc / 100  # image area damage km squared

content <- c("Forest damage (%)", "img area (km squared)", "img area damage (km squared)")
values <- c(fstdmgperc, imgarea, imgareadmg)

tableresults <- data.frame(content, values)
tableresults

View(tableresults)

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
