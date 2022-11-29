##### Forest damage estimation after the Vaia storm (29 oct 2018) #####
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



##### let's write a Function to create ggplot images #####

ggplot_img <- function(temp_obj, temp_title)  {
      # 'function' Function (base package) creates a function in R. The Arguments 'temp_obj' 
      # and 'temp_title' are the ones we pass. We return 'ggp_obj'

    if (typeof(temp_obj) == "S4")  {  # we use the conditional 'if'... 'else if' separating
                                      # instructions depending on the type of object we work
        ggp_obj <- ggplot() +
        geom_raster(temp_obj, mapping =aes(x=x, y=y, fill=layer)) +
        scale_fill_viridis(option = "inferno") +
        ggtitle(temp_title)
        # 'ggplot Function (ggplot2 package) creates a new ggplot. 
        # 'ggplot()' initializes a ggplot object.
        # geom_raster Function (ggplot2 package) permits to create the rectangle for plotting.
        # 'scale_fill_viridis' Function (viridis package) is used to choose a Viridis Color 
        # Scales for ggplot2.
        # 'ggtitle' Function (ggplot2 package) permits us to write a title to the plot
        return(ggp_obj)  # we return the object to the main code, associating the final object.
     }  else if (typeof(temp_obj) == "list")  {
           ggp_obj <- ggplot() +
           geom_raster(temp_obj$map, mapping =aes(x=x, y=y, fill=class)) +
           scale_fill_viridis(option = "inferno") +
           ggtitle(temp_title)
           return(ggp_obj)
     }
}



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
        # multi-layer raster object in 'prevaia2018' object we decide the order of layers (4 out of 4)

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
        # 'ggRGB' Function (RStoolbox package) creates ggplot2 Raster Plots with RGB from 3 RasterLayers. 
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



### let's calculate the Difference Vegetation Index (DVI) ###

dvi2018 = prevaia2018[[1]] - prevaia2018[[2]]
        # DVI is the result of the difference between NIR band and RED band
dvi2019 = postvaia2019[[1]] - postvaia2019[[2]]

### let's plot them ###

ggpdvi2018 <- ggplot_img(dvi2018, "DVI preVaia 2018")   
        # we call the 'ggplot_img' Function created at the beginning. The argument
        # we pass are the object and the title.
ggpdvi2019 <- ggplot_img(dvi2019, "DVI postVaia 2019")

# pdf("VAIA_comparison.pdf")
ggpdvi2018 + ggpdvi2019
# dev.off()


### let's calculate the difference between DVI 2018 and DVI 2019 ###

dvi_dif = dvi2018 - dvi2019

### let's plot the result ###

ggpdvi_dif <- ggplot_img(dvi_dif, "DVI 2018-19 difference")  

ggpdvi_dif



### let's calculate the Normalized Difference Vegetation Index (NDVI) ###

ndvi2018 = dvi2018 / (prevaia2018[[1]] + prevaia2018[[2]])
          # NDVI is the ratio between the DVI index and the NIR band and RED band sum
ndvi2019 = dvi2019 / (postvaia2019[[1]] + postvaia2019[[2]])

### let's plot them ###

ggpndvi2018 <- ggplot_img(ndvi2018, "NDVI preVaia 2018")  
ggpndvi2019 <- ggplot_img(ndvi2019, "NDVI postVaia 2019")  

ggpndvi2018 + ggpndvi2019


### let's calculate the difference between NDVI 2018 and NDVI 2019 ###

ndvi_dif = ndvi2018 - ndvi2019

### let's plot the result ###

ggpndvi_dif <- ggplot_img(ndvi_dif, "NDVI 2018-19 difference")  

ggpndvi_dif



### let's classify the NDVI 2018-19 difference using 3 classes ###

set.seed(2)
        # Set a seed in R is used for reproducing the same output of simulation studies. 
  
ndvi_difc <- unsuperClass(ndvi_dif, nClasses=3)
        # 'unsuperClass' Function (RStoolbox package) permits to have an Unsupervised Classification.
        # Unsupervised classification is where the outcomes (groupings of pixels with common characteristics) 
        # are based on the software analysis of an image without the user providing sample classes. 
        # The computer uses techniques to determine which pixels are related and groups them into classes

### let's plot it ###

ggpndvi_difc <- ggplot_img(ndvi_difc, "Classification NDVI 2018-19 difference")

ggpndvi_difc

freq(ndvi_difc$map)
        # 'freq' Function tells us the number of pixel for each class:
        # in our values the class 1 is made by 364324 pixels and indicates the forest damage


### let's associate some values we need for calculations ###

fstdmgpx <- 364324  # forest damage
imgtotpx <- 3607749  # image total number of pixels
area <- 360.77  # image area (km squared)


### let's calculate the forest damage percent and the area damage (km squared)

fstdmgperc <- fstdmgpx * 100 / imgtotpx  # forest damage (percent)
fstdmgarea <- area * fstdmgperc / 100  # image area damage km squared


### let's create a table results with our values ###

content <- c("area (km squared)", "Forest damage (%)", "Forest damage area (km squared)")
        # we create a first vector with the content
values <- c(area, fstdmgperc, fstdmgarea)
        # we create a second vector with the values

tableresults <- data.frame(content, values)
        # we create the final data frame associating the two vectors we created.
tableresults # we call the data frame

View(tableresults) # we display our data frame as a table results

