
library(raster)
library(RStoolbox)

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

dvi2018 = prevaia2018[[4]] - prevaia2018[[3]]
dvi2022 = postvaia2022[[4]] - postvaia2022[[3]]

cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100) # specifying a color scheme

par(mfrow=c(1, 2))
plot(dvi2018, col=cl)
plot(dvi2022, col=cl)

dvi_dif = dvi2018 - dvi2022
cld <- colorRampPalette(c('blue','white','red'))(100) 
plot(dvi_dif, col=cld)

par(mfrow=c(1, 2))
plotRGB(prevaia2018, r=4, g=3, b=2, stretch="lin")
plotRGB(postvaia2022, r=4, g=3, b=2, stretch="lin")

prevaia2018res <- aggregate(prevaia2018, fact=10)
postvaia2022res <- aggregate(postvaia2022, fact=10)

prevaia2018c <- unsuperClass(prevaia2018res, nClasses=5)

par(mfrow=c(1, 2))
plotRGB(prevaia2018res, r=3, g=2, b=1, stretch="lin")
plot(prevaia2018c$map)

freq(prevaia2018c$map)

# class 1  418001 vegetazione chiara (montando 3, 2, 1)
    # plotRGB(prevaia2018res, r=3, g=2, b=1, stretch="lin")
# class 2  500952 vegetazione in ottimo stato (rosso scuro montando 4, 3, 2) 
    # plotRGB(prevaia2018res, r=4, g=3, b=2, stretch="lin")
# class 3   55420 acqua
# class 4  218936 urbana
# class 5   12295 nuvole

# total pixel = 1205604

postvaia2022c <- unsuperClass(postvaia2022res, nClasses=5)

par(mfrow=c(1, 2))
plotRGB(postvaia2022res, r=3, g=2, b=1, stretch="lin")
plot(postvaia2022c$map)

freq(postvaia2022c$map)

# class 1  348140 vegetazione chiara (montando 3, 2, 1)
    # plotRGB(postvaia2022res, r=3, g=2, b=1, stretch="lin")
# class 2  505327 vegetazione in ottimo stato (rosso scuro montando 4, 3, 2) 
    # plotRGB(postvaia2022res, r=4, g=3, b=2, stretch="lin")
# class 3  294775 urbana
# class 4    6275 nuvole
# class 5   51087 acqua

# total pixel = 1205604

perc_veg_2018 <- (418001 + 500952) * 100 / 1205604
perc_urb_2018 <- 218936 * 100 / 1205604

perc_veg_2022 <- (348140 + 505327) * 100 / 1205604
perc_urb_2022 <- 294775 * 100 / 1205604

# perc_veg_2018 = 76.22345
# perc_urb_2018 = 18.15986

# perc_veg_2022 = 70.79165
# perc_urb_2022 = 24.4504

class <- c("Vegetation","Urban")
percent_2018 <- c(76.22, 18.16)
percent_2022 <- c(70.79, 24.45)

multitemporal <- data.frame(class, percent_2018, percent_2022)
multitemporal

View(multitemporal)




