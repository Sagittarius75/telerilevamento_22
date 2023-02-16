#install.packages("colorist")
library(colorist)
library(ggplot2)

data("fiespa_occ")
data
fiespa_occ

met1<-metrics_pull(fiespa_occ) 
pal<- palette_timecycle(fiespa_occ)

map_multiples(met1, pal, ncol=3, labels=names(fiespa_occ))

p1custom <- palette_timecycle(12, start_hue=60)

map_multiples(met1, p1custom, ncol=3, labels=names(fiespa_occ))

met1_distill<-metrics_distill(fiespa_occ) # we can distill the information

map_single(met1_distill,p1custom) #We then display

data("fisher_ud")  

m2 <- metrics_pull(fisher_ud)

pal2<-palette_timeline(fisher_ud)

head(pal2)

map_multiples(m2,ncol = 3, pal2)
map_multiples(m2,ncol = 3, pal2, lambda_i = -12)

m2_distill<-metrics_distill(fisher_ud)
map_single(m2_distill,pal2,lambda_i = -5)


