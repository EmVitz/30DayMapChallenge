
#### ---- Load libraries and set up parameters ---- ####

library(data.table)
library(janitor)
library(ggplot2)
library(sf)
library(viridis)

data_folder <- "C:\\Users\\emma.vitz\\Documents\\30 Day Map Challenge\\Data\\"
perils_folder <- "W:\\2019\\NZ Perils19\\Data\\"
nz_folder <- "K:\\R&D\\Personal Lines\\External Data\\Addressing\\NZ\\"

crs_code <- 4326
crs_code_meters <- 2193

#### ---- Read in the data  and clean ---- ####

coast <- st_read(paste0(perils_folder, "NZ Coastline Files\\nz-coastlines-and-islands-polygons-topo-150k.shp"))

# Postcode shapefiles
postcodes <- st_read(paste0(nz_folder, "Postcode Shapefiles\\PNF_V2019Q2V01\\PN_V2019Q2V01_POLYGONS_region.shp"))
postcodes <- st_transform(postcodes, crs = crs_code)

# Regional council shapefiles
reg_council <- st_read(paste0(nz_folder, "Regional Council Shapefiles\\regional-council-2018-generalised.shp"))

#### ---- Identify postcodes in more than one regional council ---- ####

intersects <- st_intersects(postcodes, reg_council)

intersects <- as.data.table(intersects)
colnames(intersects) <- c('postcode_id', 'reg_council_id')

postcode_dups <- intersects[duplicated(postcode_id),]
postcode_dups[, reg_council:= 'Multiple']

postcodes$postcode_id <- seq.int(nrow(postcodes))

postcodes <- merge(postcodes, postcode_dups, by = 'postcode_id', all.x = T)

postcodes[is.na(postcodes$reg_council),]$reg_council <- "Single"

#### --- Plot ---- ####

ggplot(data = postcodes) + 
  geom_sf(aes(fill = reg_council)) +
  scale_fill_manual(values = c('#CFF2F7', '#798193')) + 
  geom_sf(data = reg_council, colour = 'black', alpha = 0, size = 0.25) +
  coord_sf(xlim = c(166, 179), ylim = c(-33.6, -48), expand = FALSE) +
  theme_bw() +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank()) +
  labs(fill = "Number of Regional \nCouncils by Postcode") +
  ggtitle("The Intersection of Postcodes and Regional Councils in New Zealand")

ggsave("03 Polygons.png", device = png(), path = "C:\\Users\\emma.vitz\\Documents\\30 Day Map Challenge\\")





