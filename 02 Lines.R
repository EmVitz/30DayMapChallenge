#### ---- Load libraries and set up parameters ---- ####

library(sf)
library(data.table)
library(ggplot2)
library(grid)

folder <- "~/Documents/30 Day Map Challenge/Data/"

#### ---- Read in the data and clean ---- ####
coast <- st_read(paste0(folder, "lds-nz-coastlines-topo-150k-SHP/nz-coastlines-topo-150k.shp"))
faultlines <- st_read(paste0(folder, "NZAFD/Shapefiles/NZAFD_Dec_2018_WGS_84.shp"))
power <- st_read(paste0(folder, "lds-nz-powerline-centrelines-topo-1250k-SHP/nz-powerline-centrelines-topo-1250k.shp"))
earthquakes <- fread(paste0(folder, "earthquakes.csv"))

faultlines_rec_2000 <- subset(faultlines, faultlines$REC_INTERV == "I")

earthquakes <- earthquakes[eventtype == "earthquake",]
earthquakes <- earthquakes[longitude > 0 & magnitude > 4,]

#### ---- Plot ---- ####
ggplot(data = coast) +
  geom_sf(fill = "#d3d3d3", color = "#a9a9a9") +
  geom_sf(data = faultlines_rec_2000, aes(colour = "Major Faultlines")) +
  geom_sf(data = power, aes(colour = "Major Powerlines")) +
  geom_point(data = earthquakes, aes(x = longitude, y = latitude, size = magnitude), 
             alpha = 0.1, colour = "#ff710f") +
  theme_bw() +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = 
          element_blank(), 
        legend.position = "right", legend.text = element_text(size = 11)) +
  labs(size="Magnitude of Large (>4) \nEarthquakes in Past Year", colour = "") +
  ggtitle("Major Faultlines and Powerlines in New Zealand")

ggsave("02 Lines EQ.png", device = png(), path = "~/Documents/30 Day Map Challenge/Maps/")
  


