
#### ---- Load libraries and set up parameters ---- ####

library(data.table)
library(sf)
library(ggplot2)

NZ_address_folder <- "K:\\R&D\\Personal Lines\\External Data\\Addressing\\NZ\\"
data_folder <- "C:\\Users\\emma.vitz\\Documents\\30 Day Map Challenge\\Data\\"
perils_folder <- "W:\\2019\\NZ Perils19\\Data\\"
stats_nz_folder <- "K:/R&D/Personal Lines/External Data/Stats NZ/"

crs_code <- 4326
crs_code_meters <- 2193

# Change to latest version of FNAF
FNAF_version <- "1906"
pc_version <- "V2019Q2V01"

#### ---- Read in the data ---- ####

# Read in addresses
FNAF <- readRDS(paste0(NZ_address_folder, FNAF_version, "\\Output\\NZ_FNAF.RDS"))
setDT(FNAF)
FNAF <- FNAF[!duplicated(FNAF[, c('X_COORDINATE', 'Y_COORDINATE', 'POSTCODE')]), c('FIN_ID', 'X_COORDINATE', 'Y_COORDINATE', 'POSTCODE')]
FNAF_sf <- st_as_sf(FNAF, coords = c('X_COORDINATE', 'Y_COORDINATE'), crs = crs_code)

coast <- st_read(paste0(perils_folder, 'NZ Coastline Files\\nz-coastlines-and-islands-polygons-topo-150k.shp'))
mb <- st_read(paste0(stats_nz_folder, "Meshblock Shapefiles/meshblock-2013.shp"))

mb$urban_simpl <- as.character(0)
mb[mb$UrbanAreaT %in% c('Main Urban Area', 'Minor Urban Area', 'Secondary Urban Area'),]$urban_simpl <- 'Urban'
mb[mb$UrbanAreaT %in% c('Rural Centre', 'Rural (Incl.some Off Shore Islands)'),]$urban_simpl <- 'Rural'
mb[mb$urban_simpl == 0, ]$urban_simpl <- 'Other'

# Read in data on H&M
hm <- fread("W:\\2019\\NZ Perils19\\Temp\\hm.csv")
hm_sf <- st_as_sf(hm, coords = c('Longitude', 'Latitude'), crs = crs_code)

#### ---- Calculate distances ---- ####
distances <- st_distance(hm_sf, FNAF_sf)
distances_DT <- as.data.table(matrix(as.numeric(distances), ncol = 8, byrow = T))
colnames(distances_DT) <- hm$Store
distances_DT[, FIN_ID:= FNAF$FIN_ID]

distances_DT[, min_distance:= Rfast::rowMins(as.matrix(distances_DT[, c('Cashel Street', 'Riccarton', 'Queensgate', 'Tauranga', 'Hamilton', 'Botany', 'Commercial Bay', 
                                                                     'Sylvia Park')]), value = T)]

distances_DT[, min_distance_km:= min_distance/1000]
distances_DT[, random_no:= runif(nrow(distances_DT), 0, 1)]

sample_dwellings <- distances_DT[random_no < 0.3,]

sample_dwellings <- merge(sample_dwellings, FNAF[, c('FIN_ID', 'X_COORDINATE', 'Y_COORDINATE')], by = 'FIN_ID', all.x = T)

#### ---- Plot ---- ####

# Raw distance
hm_plot <- ggplot(coast) + 
    geom_sf(colour = 'white', fill = 'white') +
    coord_sf(xlim = c(166, 179), ylim = c(-33.5, -47.5)) +
    geom_point(data = sample_dwellings, aes(x = X_COORDINATE, y = Y_COORDINATE, colour = min_distance_km), size = 0.1, alpha = 0.8) +
    scale_colour_gradient2(low = '#0088E2', mid = '#5CB731', high = '#FFDA38', midpoint = 350) +
    theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), panel.background = element_rect(fill = 'white'), 
        plot.title = element_text(hjust = 0.5, size = 16), legend.position = c(0.8, 0.2)) +
    ggtitle("Urban vs Rural: Distance to \nNearest H&M from Every House in NZ") +
    labs(colour = "Distance (km)")
  

mb_plot <- ggplot(mb[mb$urban_simpl != 'Other',], aes(fill = UrbanAreaT, colour = UrbanAreaT)) + 
  geom_sf() +
  coord_sf(xlim = c(166, 179), ylim = c(-33.5, -47.5)) +
  scale_fill_manual(values = c('#0088E2', '#18C9B7', '#FFDA38', '#99DB30', '#5CB731')) + #, labels = c('Main Urban Area', 'Minor Urban Area', 'Secondary Urban Area', 
                                                                                                 #'Rural Centre', 'Rural (Incl. some Off Shore Islands')) +
  scale_colour_manual(values = c('#0088E2', '#18C9B7', '#FFDA38', '#99DB30', '#5CB731'), guide = F) + #, labels = c('Main Urban Area', 'Minor Urban Area', 'Secondary Urban Area', 
                                                                                                  #'Rural Centre', 'Rural (Incl. some Off Shore Islands'), guide = F) +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), panel.background = element_rect(fill = 'white'), 
        plot.title = element_text(hjust = 0.5, size = 16), legend.position = c(0.85, 0.1)) +
  ggtitle("Urban vs Rural: \nType of Area by Meshblock") +
  labs(fill = "Type of Area")

hm_comparison <- gridExtra::grid.arrange(hm_plot, mb_plot, nrow = 1, top = grid::textGrob("Urban and Rural Areas in New Zealand", gp = gpar(cex = 1.75)))
ggsave('C:\\Users\\emma.vitz\\Documents\\19 Urban v2.png', width = 14, height = 8, hm_comparison)



