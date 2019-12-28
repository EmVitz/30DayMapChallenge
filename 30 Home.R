#### ---- Load libraries and set up parameters ---- ####

library(sf)
library(data.table)
library(ggplot2)
library(grid)
library(png)

folder <- "~/Documents/30 Day Map Challenge/Data/"

#### ---- Read in the data and clean ---- ####
coast <- st_read(paste0(folder, "lds-nz-coastlines-topo-150k-SHP/nz-coastlines-topo-150k.shp"))
home <- fread(paste0(folder, 'home.csv'))

nelson <- readPNG(paste0(folder, 'Nelson v2.png'))
nelson <- rasterGrob(nelson, interpolate=TRUE)

akld <- readPNG(paste0(folder, 'Auckland v2.png'))
akld <- rasterGrob(akld, interpolate=TRUE)

ruapehu <- readPNG(paste0(folder, 'Ruapehu v2.png'))
ruapehu <- rasterGrob(ruapehu, interpolate=TRUE)

wellington <- readPNG(paste0(folder, 'Wellington.png'))
wellington <- rasterGrob(wellington, interpolate=TRUE)

home[, c('V4', 'V5', 'V6', 'V7'):= NULL]
home <- home[1:8,]
home <- home[!(Place %in% c('Cochem', 'Concarneau', 'Chaumont')),]

ggplot(coast) + 
  geom_sf(fill = 'light grey', colour = 'grey') +
  theme_bw() +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), 
        legend.position = "right", legend.text = element_text(size = 11), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(size = 16, hjust = 0.5, face = 'bold'), panel.border = element_blank()) +
  annotation_custom(ruapehu, xmin = 174, xmax = 177, ymax = -37.5, ymin = -40.5) +
  annotation_custom(akld, xmin=173.25, xmax = 176.25, ymin=-35.35, ymax = -38.35) +
  annotation_custom(nelson, xmin=171, xmax = 174, ymin= -40, ymax = -43) +
  annotation_custom(wellington, xmin=174, xmax = 177, ymin= -39.25, ymax = -42.25) +
  ggtitle("Home: Where I Have Lived in New Zealand") +
  geom_sf(data = coast, colour = 'grey', alpha = 1) +
  coord_sf(xlim = c(165, 179), ylim =c(-34, -48)) 

ggsave("30 Home v2.png", device = png(), path = "~/Documents/30 Day Map Challenge/Maps/")

