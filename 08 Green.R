
#### ---- Load libraries and set up parameters ---- ####

library(data.table)
library(janitor)
library(ggplot2)
library(sf)
library(viridis)
library(rvest)
library(dplyr)
library(raster)

data_folder <- "C:\\Users\\emma.vitz\\Documents\\30 Day Map Challenge\\Data\\"
perils_folder <- "W:\\2019\\NZ Perils19\\Data\\"

crs_code <- 4326
crs_code_meters <- 2193

#### ---- Read in the data ---- ####

property <- st_read(paste0(perils_folder, 'Property Shapefiles\\nz-property-titles.shp'))

nelson_property <- property[property$land_distr == "Nelson",]

nelson_extent <- extent(c(172, 174, -41.5,  -40))

nelson_cropped_index <- st_covers(nelson_property, nelson_extent)

nelson_property$time_held <- as.numeric(as.Date("2019-11-08") - nelson_property$issue_date)/365

ggplot(nelson_property, aes(fill = time_held, colour = time_held)) +
  geom_sf() +
  coord_sf(xlim = c(172.9, 173.5), ylim = c(-41.1, -41.4), expand = FALSE) + 
  theme_bw() +
  scale_fill_gradient(low = '#D1F0B1', high = '#1B3022') +
  scale_colour_gradient(low = '#D1F0B1', high = '#1B3022', guide = F) +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank()) +
  ggtitle("Duration of Property Ownership (Years) in Nelson, New Zealand") +
  labs(fill = "Duration \n(Years)")

ggsave("08 Green v2.png", width = 8, height = 6, device = png(), path = "C:\\Users\\emma.vitz\\Documents\\")





  