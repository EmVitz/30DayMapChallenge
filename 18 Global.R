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

#### ---- Set up parameters and read in data ---- ####

stats_nz_folder <- "K:/R&D/Personal Lines/External Data/Stats NZ/"
nz_folder <- "K:\\R&D\\Personal Lines\\External Data\\Addressing\\NZ\\"
perils_folder <- "W:\\2019\\NZ Perils19\\"
lidar_folder <- "//atlas/F/FINITY/NZ perils/2 Geospatial Data/2 Processed Data/LiDAR merged/"

crs_code <- 4326
crs_code_meters <- 2193
FNAF_version <- "1906"

#### ---- Read in the data ---- ####
world <- st_read('W:\\2019\\NZ Perils19\\Data\\World Boundaries\\world-country-boundaries.shp')
world <- world[world$GMI_CNTRY != 'ATA',]

unemp <- fread('W:\\2019\\NZ Perils19\\Temp\\Unemployment.csv', skip = 1, header = T)
unemp <- clean_names(unemp)
unemp19 <- unemp[, c('country_name', 'country_code', 'x2019')]

world_unemp <- merge(world, unemp19, by.x = 'GMI_CNTRY', by.y = 'country_code', all.x = T)

ggplot(data = world_unemp, aes(fill = x2019/100, colour = x2019/100)) + 
  geom_sf() +
  scale_fill_viridis(labels = scales::percent_format(accuracy = 1)) + 
  scale_colour_viridis(guide = F) + 
  labs(fill = 'Percentage \nUnemployed') +
  theme(panel.background = element_rect(fill = 'white'), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), 
        plot.title = element_text(size = 16, hjust = 0.5, vjust = 0.5, face = 'bold')) + #, legend.title = element_text(face = 'bold')) +
  ggtitle("Unemployment in 2019 by Country (Percentage of Total Labour Force)")

ggsave("18 Globe.png", width = 10, height = 6, device = png(), path = "C:\\Users\\emma.vitz\\Documents\\")
ggsave("18 Globe v3.png", width = 15, height = 6, device = png(), path = "C:\\Users\\emma.vitz\\Documents\\")





