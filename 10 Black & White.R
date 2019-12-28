#### ---- Load libraries and set up parameters ---- ####

library(sf)
library(data.table)
library(ggplot2)
library(grid)
library(maps)
library(janitor)
library(rmapshaper)
library(raster)
library(jpeg)

folder <- "~/Documents/30 Day Map Challenge/Data/"

#### ---- Read in the data

DSM <- raster(paste0(folder, 'lds-auckland-lidar-1m-dsm-2013-GTiff/auckland-lidar-1m-dsm-2013.tif'))
DSM_df <- as.data.frame(DSM, xy = TRUE)
DSM_DT <- setDT(DSM_df)
extent <- extent(list(x = c(172, 178), y = c(-39.1, -34.5)))

roads <- st_read(paste0(folder, 'lds-nz-roads-addressing-SHP/nz-roads-addressing.shp'))

labels_df <- as.data.frame(cbind(latitude = c(-36.848, -36.8508, -36.8499, -36.8503), 
              longitude =  c(174.76235, 174.76782, 174.76697, 174.7645),
              label = c('Sky Tower', 'Albert Park', 'Trees', 'Queen Street')))
setDT(labels_df)
labels_df[, latitude:= as.numeric(as.character(latitude))]
labels_df[, longitude:= as.numeric(as.character(longitude))]


ggplot() +
  geom_raster(data = DSM_DT, aes(x = x, y = y, fill = auckland.lidar.1m.dsm.2013)) +
  coord_fixed() +
  scale_fill_gradient2(low = 'black', high = 'white', midpoint = 150) +
  geom_sf(data = roads, colour = 'blue') +
  coord_sf(xlim = c(174.761, 174.7717), ylim = c(-36.85283, -36.84713), expand = FALSE) +
  geom_point(data = labels_df, aes(x = longitude, y = latitude), colour = 'blue') +
  geom_label(data = labels_df, aes(x = longitude, y = latitude, label =label), colour = 'blue') +
  theme_bw() +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(),
        legend.text = element_text(size = 11)) +
  labs(fill = "Elevation (m)") +
  ggtitle("Elevation of Built & Natural Features in the Auckland CBD (LiDAR DSM)")

ggsave("10 Black and White.png", width = 7, height = 4, device = png(), path = "~/Documents/30 Day Map Challenge/Maps/")



setnames(DSM_DT, 'auckland.lidar.1m.dsm.2013', 'dsm_elev')

DSM_DT[, change:= dsm_elev - data.table::shift(dsm_elev, n = 1, type = 'lead')]

ggplot(DSM_DT, aes(x = change)) + 
  geom_histogram()

DSM_DT[abs(change) >= 3, test:= 1]
DSM_DT[abs(change) < 3, test:= 0]


ggplot() +
  geom_raster(data = DSM_DT, aes(x = x, y = y, fill = as.factor(test))) +
  coord_fixed() +
  scale_fill_manual(values = c('black', 'white'))

