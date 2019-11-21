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
library(ggsn)
library(extrafont)
library(gridExtra)
library(grid)

#### ---- Set up parameters and read in data ---- ####

stats_nz_folder <- "K:/R&D/Personal Lines/External Data/Stats NZ/"
nz_folder <- "K:\\R&D\\Personal Lines\\External Data\\Addressing\\NZ\\"
perils_folder <- "W:\\2019\\NZ Perils19\\Data\\"

crs_code <- 4326
crs_code_meters <- 2193

coast <- st_read('W:\\2019\\NZ Perils19\\Data\\NZ Coastline Files\\nz-coastlines-and-islands-polygons-topo-150k.shp')
roads <- st_read(paste0(perils_folder, 'Roads Addressing\\nz-roads-addressing.shp'))

# Top ten road names (excluding the suffix)
road_names <- as.data.table(table(roads$full_roa_1))
setorder(road_names, -N)
roads_suffix <- as.vector(road_names[1:10,V1])

# Top ten road names (excluding the suffix)
road_names2 <- as.data.table(table(roads$road_nam_7))
setorder(road_names2, -N)
roads_no_suffix <- as.vector(road_names2[1:10,V1])

top_roads1 <- roads[roads$full_roa_1 %in% roads_suffix,]
top_roads2 <- roads[roads$road_nam_7 %in% roads_no_suffix,]
top_roads3 <- roads[roads$road_nam_7 %in% roads_no_suffix[1:5],]


top_roads2_nzd2000 <- st_transform(top_roads2, crs = crs_code_meters)
top_roads_centre <- st_centroid(top_roads2_nzd2000)
top_roads_centre <- st_transform(top_roads_centre, crs_code)

top_roads3_nzd2000 <- st_transform(top_roads3, crs = crs_code_meters)
top_roads3_centre <- st_centroid(top_roads3_nzd2000)
top_roads3_centre <- st_transform(top_roads3_centre, crs_code)


# Top roads centre
# Auckland
ggplot(coast) + 
  geom_sf() + 
  geom_sf(data = top_roads3_centre, aes(colour = road_nam_7), size = 3, alpha = 0.8) +
  coord_sf(xlim = c(174, 176), ylim = c(-36, -37.5)) 

# NZ
ggplot(coast) + 
  geom_sf(fill = 'white') + 
  geom_sf(data = top_roads3_centre, aes(colour = road_nam_7), size = 2, alpha = 0.75) +
  coord_sf(xlim = c(166, 179), ylim = c(-34, -48)) +
  scale_colour_manual(values = c('#FFE123', '#274060','#A8D84E', '#EA8800', '#4A8FE7')) +
  labs(colour = 'Top 5 Street Names') +
  theme(panel.background = element_rect(fill = 'white'), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), plot.title = element_text(size = 16, 
        hjust = 0.5, face = 'bold'), legend.position = c(0.8, 0.2), legend.title = element_text(face = 'bold')) +
  ggtitle("Most Common Street Names in New Zealand")

ggsave("15 Names.png", device = png(), path = "C:\\Users\\emma.vitz\\Documents\\")


