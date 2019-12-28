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
library(extrafont)
library(gridExtra)
library(grid)

#### ---- Set up parameters and read in data ---- ####

stats_nz_folder <- "K:/R&D/Personal Lines/External Data/Stats NZ/"
nz_folder <- "K:\\R&D\\Personal Lines\\External Data\\Addressing\\NZ\\"
perils_folder <- "W:\\2019\\NZ Perils19\\"
lidar_folder <- "//atlas/F/FINITY/NZ perils/2 Geospatial Data/2 Processed Data/LiDAR merged/"

crs_code <- 4326
crs_code_meters <- 2193
FNAF_version <- "1906"

chch_extent <- extent(x = c(172.65, 172.7), y = c(-43.53, -43.495))

#### ---- Read in the data ---- ####
coast <- st_read('W:\\2019\\NZ Perils19\\Data\\NZ Coastline Files\\nz-coastlines-and-islands-polygons-topo-150k.shp')

redzone <- st_read(paste0(perils_folder, 'Temp\\Redzone\\Land_Check_Colour_Zones_WGS84_20131204_Explode.shp'))
redzone_only <- redzone[redzone$Class == "Red",]

# Crop to Christchurch extent
redzone_cropped <- st_crop(redzone_only, chch_extent)

# Read in addresses
FNAF <- readRDS(paste0(nz_folder, FNAF_version, "\\Output\\NZ_FNAF.RDS"))
setDT(FNAF)

# Cut down to Christchurch
chch_dwellings <- FNAF[X_COORDINATE <= 172.7 & X_COORDINATE >= 172.65 & Y_COORDINATE <= -43.495 & Y_COORDINATE >= -43.53,]
chch_dwellings_sf <- st_as_sf(chch_dwellings, coords = c('X_COORDINATE', 'Y_COORDINATE'), crs = crs_code)

# Get elevation and crop to 
elev <- raster(paste0(perils_folder, "Temp\\Redzone\\Elevations_PostDec2011_WGS84.tif"))
elev_chch <- crop(elev, chch_extent)

elev_chch_DT <- as.data.frame(elev_chch, xy = T)
setDT(elev_chch_DT)

intersects_redzone <- st_intersects(redzone_cropped, chch_dwellings_sf)
intersects_redzone <- unlist(intersects_redzone)

chch_dwellings_sf$in_redzone <- as.character(0)
chch_redzone <- chch_dwellings_sf[intersects_redzone, ]

ggplot() +
  geom_raster(data = elev_chch_DT, aes(x = x, y = y, fill = Elevations_PostDec2011_WGS84)) +
  coord_fixed() +
  geom_sf(data = redzone_cropped, colour = '#E94F37', fill ='#E94F37', alpha = 0) +
  coord_sf(xlim = c(172.65, 172.7), ylim = c(-43.495, -43.53)) +
  geom_sf(data = chch_dwellings_sf[intersects_redzone, ], colour = 'red', size = 0.25) +
  labs(fill = 'Elevation (m)') +
  ggspatial::annotation_scale() +
  theme(panel.background = element_rect(fill = 'white'), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), 
        plot.title = element_text(size = 16, hjust = 0.5, vjust = 0.5, face = 'bold'), legend.title = element_text(face = 'bold')) +
  ggtitle("Dwellings in Part of the Christchurch Redzone")
  
ggsave("17 Zones.png", device = png(), path = "C:\\Users\\emma.vitz\\Documents\\")
ggsave("17 Zones v3.png", height = 6, width = 9, device = png(), path = "C:\\Users\\emma.vitz\\Documents\\")

