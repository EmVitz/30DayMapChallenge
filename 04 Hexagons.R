library(data.table)
library(geogrid)
library(sf)
library(ggplot2)
library(rmapshaper)
library(stringr)
library(raster)
library(viridis)
library(parallel)
library(RColorBrewer)

#### ---- Set up parameters and read in data ---- ####

stats_nz_folder <- "K:/R&D/Personal Lines/External Data/Stats NZ/"
nz_folder <- "K:\\R&D\\Personal Lines\\External Data\\Addressing\\NZ\\"
perils_folder <- "W:\\2019\\NZ Perils19\\Data\\"

crs_code <- 4326
crs_code_meters <- 2193

census <- fread(paste0(stats_nz_folder, "Census 2013/To Use/c13_mb.csv"))

coast <- st_read(paste0(perils_folder, "NZ Coastline Files\\nz-coastlines-and-islands-polygons-topo-150k.shp"))

mb <- st_read(paste0(stats_nz_folder, "Meshblock Shapefiles/meshblock-2013.shp"))

roads <- st_read(paste0(perils_folder, "Roads (Auckland CBD)\\Addressing\\nz-roads-addressing.shp"))

#### ---- Prepare data ---- ####

# Define extent of map
extent_auckland_cbd <- extent(list(x = c(174.71, 174.79), y = c(-36.81, -36.87)))

# Subset meshblocks to this extent
mb_auckland_cbd <- subset(mb, mb$UrbanAreaT == "Main Urban Area" & mb$UrbanAreaN == "Central Auckland Zone")
mb_cropped <- st_crop(mb_auckland_cbd, extent_auckland_cbd)

# Simplify 
mb_akl_simplified <- ms_simplify(mb_cropped, keep = 0.02, keep_shapes = T)

# Create hexagons 
hex_grid_auck <- calculate_grid(mb_akl_simplified, learning_rate = 0.05, grid_type = "hexagonal")
grid_shapes <- assign_polygons(mb_akl_simplified, hex_grid_auck)

extent_auckland_cbd <- extent(list(x = c(174.71, 174.79), y = c(-36.81, -36.87)))

coast_auckland <- st_crop(coast, extent_auckland_cbd)
roads_auckland <- st_crop(roads, extent_auckland_cbd)

commute <- grep('Comm', colnames(census), value = T)

census_commute <- census[, c('MB_2013_code', commute), with = F]
census_commute[, c('c13_Occ_ANZSCO_Comm', 'c13_OccWk_ANZSCO_Comm'):= NULL]

census_commute[, meshblock_code:= str_pad(MB_2013_code, width = 7, side = "left", pad = "0")]
grid_shapes_census <- merge(grid_shapes, census_commute, by.x = 'MeshblockN', by.y = 'meshblock_code', all.x = T)
 
#### ---- Plot ---- ####

lengths <- st_length(roads_auckland$geometry)

roads_restr <- roads_auckland[as.numeric(lengths) > 1000,]

long_roads_index <- which(as.numeric(lengths) > 2000)

long_roads <- roads_auckland[long_roads_index,]
long_roads <- long_roads[long_roads$road_id != "3055877",]

ggplot(data = coast_auckland) +
  geom_sf(colour = "white") +
  theme_bw() +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank()) +
  labs(fill = "Proportion who \nCommute by Car") +
  geom_sf(data = grid_shapes_census,aes(fill = c13_H1_Comm_PrivateVehicle), colour = "white") +
  scale_fill_gradient(low = '#e1eec3', high = '#f05053') +
  geom_sf(data = roads_restr, size = 0.25) +
  geom_sf_label(data = long_roads , aes(label = road_nam_6), colour = 'black') +
  ggtitle("Proportion of People who Commute by Car in Central Auckland")

ggsave("04 Hexagons Auckland.png", device = png(), path = "C:\\Users\\emma.vitz\\Documents\\")


