
#### ---- Load libraries ---- ####

library(data.table)
library(janitor)
library(sf)
library(ggplot2)
library(rmapshaper)
library(stringr)
library(raster)
library(viridis)
library(parallel)
library(RColorBrewer)
library(dplyr)
library(gridExtra)
library(grid)
library(rvest)

nz_folder <- "K:\\R&D\\Personal Lines\\External Data\\Addressing\\NZ\\"
perils_folder <- "W:\\2019\\NZ Perils19\\Data\\"

#### ---- Read in data and process ---- ####

ta <- st_read(paste0(nz_folder, "Territorial Authorities Shapefiles/territorial-authority-2018-clipped-generalised.shp"))
ta <- ta[ta$TA2018_V_1 != 'Chatham Islands Territory' & ta$TA2018_V_1 != 'Area Outside Territorial Authority',]

coast <- st_read(paste0(perils_folder, 'NZ Coastline Files\\nz-coastlines-and-islands-polygons-topo-150k.shp'))

flood_inputs <- readRDS('W:\\2019\\NZ Perils19\\FNAF\\FNAF_FLOOD_INPUTS.RDS')
setDT(flood_inputs)

crs_code <- 4326
crs_code_meters <- 2193

ta_nzd2000 <- st_transform(ta, crs_code_meters)
centre_ta <- st_centroid(ta_nzd2000)
centre_ta <- st_transform(centre_ta, crs_code)

flood_inputs[distance_coast < 100, by_coast:= 1]
flood_inputs[distance_coast >= 100, by_coast:= 0]

dwellings_by_coast <- flood_inputs[by_coast == 1,]

proportion_by_coast <- flood_inputs[, .(count_by_coast = sum(by_coast, na.rm = T), total = .N), by = TA2018_V_1] 
proportion_by_coast[, proportion:= count_by_coast/total]

centre_ta <- merge(centre_ta, proportion_by_coast, by = 'TA2018_V_1', all.x = T)
centre_ta_DT <- as.data.table(cbind(as.character(centre_ta$TA2018_V_1), st_coordinates(centre_ta), centre_ta$proportion))

setnames(centre_ta_DT, c('V1', 'V4'), c('TA', 'prop_by_coast'))
centre_ta_DT[, X:= as.numeric(X)]
centre_ta_DT[, Y:= as.numeric(Y)]
centre_ta_DT[, prop_by_coast:= as.numeric(prop_by_coast)]
centre_ta_DT <- centre_ta_DT[prop_by_coast != 0,]


#### ---- Plot: North Island ---- ####

ggplot(ta) +
  geom_sf(fill = 'black', colour = '#848484') + 
  coord_sf(xlim = c(172.75, 178.75), ylim = c(-34, -41.5)) +
  geom_point(data = dwellings_by_coast, aes(x = X_COORDINATE, y = Y_COORDINATE),  size = 1, colour = '#0061FF') +
  geom_text(data = centre_ta_DT, aes(x = X, y = Y, label = scales::percent(round(prop_by_coast, 3))), check_overlap = T, fontface = 'bold', colour = 'white', 
            size = 6) +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), panel.background = element_rect(fill = 'black'),  
        plot.title = element_text(hjust = 0.5, size = 16, colour = 'white', face = 'bold'), panel.grid.major = element_blank(), plot.background = element_rect(fill = 'black')) +
  ggtitle("Dwellings less than 100m from Coast \n(Proportion by Territorial Authority)")

ggsave("24 Statistics North Island.png", width = 8, height = 9, device = png(), path = "C:\\Users\\emma.vitz\\Documents\\")

#### ---- Plot: South Island ---- ####

ggplot(ta) +
  geom_sf(fill = 'black', colour = '#848484') + 
  coord_sf(xlim = c(166.75, 174), ylim = c(-40.25, -47.25)) +
  geom_point(data = dwellings_by_coast, aes(x = X_COORDINATE, y = Y_COORDINATE), size = 1, colour = '#0061FF') +
  geom_text(data = centre_ta_DT, aes(x = X, y = Y, label = scales::percent(round(prop_by_coast, 3))), check_overlap = T, colour = 'white', 
            size = 6) +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), panel.background = element_rect(fill = 'black'),  
        plot.title = element_text(hjust = 0.5, size = 16, colour = 'white', face = 'bold'), panel.grid.major = element_blank(), 
        plot.background = element_rect(fill = 'black')) +
  ggtitle("Dwellings less than 100m from Coast \n(Proportion by Territorial Authority)")

ggsave("24 Statistics South Island.png", width = 8, height = 9, device = png(), path = "C:\\Users\\emma.vitz\\Documents\\")






