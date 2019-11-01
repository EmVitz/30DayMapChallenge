
#### ---- Load libraries and set up parameters ---- ####

library(data.table)
library(janitor)
library(ggplot2)
library(sf)
library(viridis)

data_folder <- "C:\\Users\\emma.vitz\\Documents\\30 Day Map Challenge\\Data\\"
perils_folder <- "W:\\2019\\NZ Perils19\\Data\\"

crs_code <- 4326
crs_code_meters <- 2193

#### ---- Read in the data  and clean ---- ####

# Schools data 
schools <- fread(paste0(data_folder, 'Directory-School-Current.csv'))
schools <- clean_names(schools)

# NZ coastline shapefile
coast <- st_read(paste0(perils_folder, "NZ Coastline Files\\nz-coastlines-and-islands-polygons-topo-150k.shp"))

# Regional council shapefiles
reg_council <- st_read("K:\\R&D\\Personal Lines\\External Data\\Addressing\\NZ\\Regional Council Shapefiles\\regional-council-2018-generalised.shp")

# Check missing lat/long and remove
nrow(schools[is.na(latitude) | is.na(longitude)]) #29
schools <- schools[!is.na(latitude) & !is.na(longitude),]

# Correct missing values coded as 99
schools[decile == 99, decile:= NA]

# Remove Chatham Islands - messes with maps
schools <- schools[postal_address_3 != "Chatham Islands",]

schools[, prop_pakeha:= european_pakeha / total_school_roll]
schools[, prop_maori:= maori / total_school_roll]
schools[, prop_pacific:= pacific / total_school_roll]
schools[, prop_asian:= asian / total_school_roll]

#### --- Plot ---- ####

# Auckland: Pakeha
ggplot(data = coast) + 
  geom_sf() +
  geom_point(data = schools, aes(x = longitude, y = latitude, colour = prop_pakeha, size = total_school_roll), alpha = 0.7) +
  coord_sf(xlim = c(174.55, 175.1), ylim = c(-36.7, -37.1), expand = FALSE) + 
  theme_bw() +
  scale_colour_viridis(option = "plasma") +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank()) +
  labs(colour = "Proportion\n P\u101keh\u101", size = "Total\n School Roll") +
  ggtitle("Schools in Auckland by Size and Student Ethnicity")

ggsave("01 Points Auckland.png", device = png(), path = "C:\\Users\\emma.vitz\\Documents\\30 Day Map Challenge\\")


# Wellington: Pakeha
ggplot(data = coast) + 
  geom_sf() +
  geom_point(data = schools, aes(x = longitude, y = latitude, colour = prop_pakeha, size = total_school_roll), alpha = 0.7) +
  coord_sf(xlim = c(174.7, 175), ylim = c(-41.07, -41.37), expand = FALSE) + 
  theme_bw() +
  scale_colour_viridis(option = "plasma") +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank()) +
  labs(colour = "Proportion\n P\u101keh\u101",  size = "Total\n School Roll") +
  ggtitle("Schools in Wellington by Size and Student Ethnicity")

ggsave("01 Points Wellington.png", device = png(), path = "C:\\Users\\emma.vitz\\Documents\\30 Day Map Challenge\\")

# Auckland: Maori
ggplot(data = coast) + 
  geom_sf() +
  geom_point(data = schools, aes(x = longitude, y = latitude, colour = prop_maori, size = total_school_roll), alpha = 0.7) +
  coord_sf(xlim = c(174.55, 175.1), ylim = c(-36.7, -37.1), expand = FALSE) + 
  theme_bw() +
  scale_colour_viridis(option = "plasma") +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank()) +
  labs(colour = "Proportion\n M\u101ori", family = "sans") +
  ggtitle("Schools in Auckland by Student Ethnicity")


# Wellington: Maori
ggplot(data = coast) + 
  geom_sf() +
  geom_point(data = schools, aes(x = longitude, y = latitude, colour = prop_maori, size = total_school_roll), alpha = 0.7) +
  coord_sf(xlim = c(174.7, 175), ylim = c(-41.07, -41.37), expand = FALSE) + 
  theme_bw() +
  scale_colour_viridis(option = "plasma") +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank()) +
  labs(colour = "Proportion\n M\u101ori",  size = "Total\n School Roll") +
  ggtitle("Schools in Wellington by Student Ethnicity")

