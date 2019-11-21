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

#### ---- Read in the data ---- ####
property <- st_read(paste0(perils_folder, 'Property Shapefiles\\nz-property-titles.shp'))
roads <- st_read('W:\\2019\\NZ Perils19\\Temp\\Roads Nelson\\nz-roads-addressing.shp')
coast <- st_read('W:\\2019\\NZ Perils19\\Data\\NZ Coastline Files\\nz-coastlines-and-islands-polygons-topo-150k.shp')

movement <- st_read(paste0('W:\\2019\\NZ Perils19\\Temp\\Cat Movement\\lines.shp'))
movement <- movement[movement$name != 'Cornwall Lola-CornwallLolaTag',]

movement_DT <- fread('W:\\2019\\NZ Perils19\\Temp\\Cat Movement\\Pet Cats New Zealand.csv')

#### ---- Clean the data ---- ####

property <- property[property$land_distr %in% c('Nelson', 'Wellington'),]

prop_nelson <- property[property$land_distr == 'Nelson',]
prop_wellington <- property[property$land_distr == 'Wellington',]

nelson_boundaries <- st_polygon(list(matrix(c(173.1,-41.2, 173.2,-41.2, 173.2,-41.4, 173.1, -41.4, 173.1,-41.2), ncol = 2, byrow = T)))
wgtn_boundaries <- st_polygon(list(matrix(c(175.015,-41.14, 175.025,-41.14, 175.025,-41.15, 175.015, -41.15, 175.015,-41.14), ncol = 2, byrow = T)))

wgtn_boundaries2 <- st_polygon(list(matrix(c(174.6,-40.8, 175.2,-40.8, 175.2, -41.4, 174.6, -41.4, 174.6,-40.8), ncol = 2, byrow = T)))


# Get property that intersects with this box
prop_nelson_box <- st_intersects(nelson_boundaries, prop_nelson)
nelson_intersects <- prop_nelson[unlist(prop_nelson_box),]

prop_wgtn_box <- st_intersects(wgtn_boundaries, prop_wellington)
wgtn_intersects <- prop_wellington[unlist(prop_wgtn_box),]

prop_wgtn_box2 <- st_intersects(wgtn_boundaries2, prop_wellington)
wgtn_intersects2 <- prop_wellington[unlist(prop_wgtn_box2),]


# Get roads that intersect with this box
roads_nelson_box <- st_intersects(nelson_boundaries, roads)
roads_intersects <- roads[unlist(roads_nelson_box),]

roads_wgtn_box <- st_intersects(wgtn_boundaries, roads)
roads_wgtn <- roads[unlist(roads_wgtn_box),]

roads_wgtn_box2 <- st_intersects(wgtn_boundaries2, roads)
roads_wgtn2 <- roads[unlist(roads_wgtn_box2),]

# Clean up movement data
movement_DT <- clean_names(movement_DT)
movement_DT[, date:= as.Date(substr(study_local_timestamp, 1, 10))]
movement_DT[, study_local_timestamp:= as.POSIXct(study_local_timestamp)]

#### ---- Calculate metrics by cat ---- ####

# Distance covered: total and per day
summary_by_cat <- movement_DT[, .(days_under_obs = as.numeric(max(study_local_timestamp) - min(study_local_timestamp))/(24*60*60)),
                              by = individual_local_identifier]

summary_by_cat[, distance_covered:= as.numeric(st_length(movement))]
summary_by_cat[, distance_per_day:= distance_covered / days_under_obs]

# Number of encounters with other cars
encounters <- st_intersects(movement, movement)
summary_by_cat[, encounters:= unlist(lapply(encounters, FUN = length))-1]

setorder(summary_by_cat, distance_per_day)


#### ---- Plot: Movement ---- ####

blue_nelson <- movement[movement$name == "BlueNelson-BlueNelsonTag",]

blue_nelson_plot <- ggplot(nelson_intersects) +
  geom_rect(xmin = 173.0, xmax = 173.13, ymax = -41.2925, ymin = -41.5, fill = '#FAF3DD') +
  geom_sf(fill = '#FAF3DD') +
  geom_sf(data = roads_intersects, colour = '#8F918E', size = 1.5) +
  geom_sf(data = blue_nelson, colour = '#F21B18', size = 0.75) +
  coord_sf(xlim = c(173.11, 173.14), ylim = c(-41.2925, -41.315)) +
  theme(panel.background = element_rect(fill = '#BCE7FD'), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), plot.title = element_text(size = 16, hjust = 0.5)) +
  ggspatial::annotation_scale() +
  ggtitle("Biggest Feline Mover and Shaker: Blue Nelson. \nAverage of 3.9 kilometres per day")

tasha <- movement[movement$name == "Tasha-TashaTag",]

tasha_plot <- ggplot(wgtn_intersects) +
  geom_sf(fill = '#FAF3DD') +
  geom_sf(data = roads_wgtn, colour = '#8F918E', size = 1.5) +
  geom_sf(data = tasha, colour = '#F21B18', size = 0.75) +
  coord_sf(xlim = c(175.015, 175.025), ylim = c(-41.14, -41.1475)) +
  theme(panel.background = element_rect(fill = '#FAF3DD'), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), plot.title = element_text(size = 16, hjust = 0.5)) +
  ggspatial::annotation_scale() +
  ggtitle("Biggest Feline Couch Potato: Tasha. \nAverage of 42 metres per day")

comparison <- arrangeGrob(blue_nelson_plot, tasha_plot, nrow = 1, top = textGrob("Most and Least Active Cats in New Zealand",
                                                                                 gp = gpar(cex = 1.75)))

ggsave('C:\\Users\\emma.vitz\\Documents\\12 Movement v2.png', width = 10, height = 6, comparison)

grid.arrange(blue_nelson_plot, tasha_plot, nrow = 1, top = textGrob("Most and Least Active Cats in New Zealand", vjust = 7, gp = gpar(cex = 1.75)))
ggsave("12 Movement.png", width = 8, height = 6, device = png(), path = "C:\\Users\\emma.vitz\\Documents\\")

#### ---- Plot: Tracks ---- ####
summary_by_cat[, encounters_per_day:= encounters/days_under_obs]

setorder(summary_by_cat, encounters)

most_encounters <- unlist(encounters[which(movement$name == "MontyII-MontyIITag")])
montyII_encounters <- movement[most_encounters, ]
montyII_encounters$name <- c('Monty II', 'Iggy', 'Zeno', 'Todd')

# Plot of most interactions
monty_plot <- ggplot(coast) +
  geom_sf(fill = '#B1D3B6', colour = '#79AF80') +
  geom_sf(data = wgtn_intersects2, fill = '#B1D3B6', colour = '#79AF80') +
  geom_sf(data = montyII_encounters, aes(colour = name), alpha = 0.5, fill = 'white') +
  scale_colour_manual(values = c('#00BBFF', '#303036', '#FFD82D', '#FC5130')) +
  coord_sf(xlim = c(174.78, 174.79), ylim = c(-41.314, -41.322)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = '#BCE7FD'), plot.title = element_text(size = 14.5, hjust = 0.5)) +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), legend.text = element_text(size = 11), legend.title = 
          element_text(size = 12, face = 'bold'), legend.position = c(0.85, 0.15), legend.background = element_rect(fill = 'white'), 
        legend.key = element_rect(fill = 'white')) +
  ggspatial::annotation_scale() +
  labs(colour = 'Name') +
  ggtitle("Social Butterfly: Monty II has the most Feline Friends")

fewest_encounters <- movement[which(unlist(lapply(encounters, FUN = length)) == 1),]

# Plot of no interactions
alone_plot <- ggplot(coast) +
  geom_sf(fill = '#B1D3B6', colour = '#79AF80') +
  geom_sf(data = wgtn_intersects2, fill = '#B1D3B6', colour = '#79AF80') +
  geom_sf(data = fewest_encounters, colour = '#F21B18', size = 0.75) +
  coord_sf(xlim = c(174.735, 174.835), ylim = c(-41.27, -41.35)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = '#BCE7FD'), plot.title = element_text(size = 14.5, hjust = 0.5)) +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank()) +
  ggspatial::annotation_scale() +
  ggtitle("One is the Loneliest Number: Cats with No Interactions")

social_comparison <- grid.arrange(monty_plot, alone_plot, nrow = 1, top = textGrob("Most and Least Social Cats in New Zealand", gp = gpar(cex = 1.75)))
ggsave('C:\\Users\\emma.vitz\\Documents\\13 Tracks final.png', width = 10, height = 6, social_comparison)
