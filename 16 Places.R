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
historical_sites <- st_read('W:\\2019\\NZ Perils19\\Data\\Historical Sites NZ\\nz-historic-site-points-topo-150k.shp')

historical_DT <- data.table(table(historical_sites$descriptn))
setorder(historical_DT, -N)

historical_sites$type <- as.character(0)
historical_sites <- historical_sites[!is.na(historical_sites$descriptn),]


# War, guns and battles
war <- c('gun', 'battle', 'war', 'defence')
war_index <- grep(paste(war, collapse='|'), historical_sites$descriptn, ignore.case = T)

historical_sites[war_index, ]$type <- 'Gun emplacement/battle'

# Burials and graves
burials <- c('burial', 'grave', 'cemetery')
burial_index <- grep(paste(burials, collapse='|'), historical_sites$descriptn, ignore.case = T)

historical_sites[burial_index, ]$type <- 'Burial place'

# Gold and mining
mining <- c('gold', 'mine', 'mining', 'quarry')
mine_index <- grep(paste(mining, collapse='|'), historical_sites$descriptn, ignore.case = T)

historical_sites[mine_index, ]$type <- 'Mine/Gold discovery'

# Church
church <- c('church', 'religion', 'covenant')
church_index <- grep(paste(church, collapse='|'), historical_sites$descriptn, ignore.case = T)

historical_sites[church_index, ]$type <- 'Church'

# Stations
stations <- c('station', 'rail', 'train', 'engine')
stations_index <- grep(paste(stations, collapse='|'), historical_sites$descriptn, ignore.case = T)
historical_sites[stations_index, ]$type <- 'Railway station'


# Whaling
whaling <- c('whale', 'whaling')
whale_index <- grep(paste(whaling, collapse='|'), historical_sites$descriptn, ignore.case = T)
historical_sites[whale_index, ]$type <- 'Whaling station'

# Kiln
kiln <- c('kiln')
kiln_index <- grep(paste(kiln, collapse='|'), historical_sites$descriptn, ignore.case = T)
historical_sites[kiln_index, ]$type <- 'Kiln'

# Battery
battery <- c('battery')
battery_index <- grep(paste(battery, collapse='|'), historical_sites$descriptn, ignore.case = T)
historical_sites[battery_index, ]$type <- 'Battery'

# Waitangi
waitangi <- c('waitangi', 'treaty')
waitangi_index <- grep(paste(waitangi, collapse='|'), historical_sites$descriptn, ignore.case = T)
historical_sites[waitangi_index, ]$type <- 'Treaty of Waitangi'

# Hut / Cottage
huts <- c('hut', 'cottage', 'camp', 'house', 'building', 'hotel')
huts_index <- grep(paste(huts, collapse='|'), historical_sites$descriptn, ignore.case = T)
historical_sites[huts_index, ]$type <- 'Accommodation'

# School
school <- c('school')
school_index <- grep(paste(school, collapse='|'), historical_sites$descriptn, ignore.case = T)
historical_sites[school_index, ]$type <- 'School'

# Maori drawings
maori <- c('drawings')
maori_index <- grep(paste(maori, collapse='|'), historical_sites$descriptn, ignore.case = T)
historical_sites[maori_index, ]$type <- 'Maori Drawings'

# Mill/Farm 
mill <- c('mill', 'farm', 'wool', 'shed')
mill_index <- grep(paste(mill, collapse='|'), historical_sites$descriptn, ignore.case = T)
historical_sites[mill_index, ]$type <- 'Mill/Farm'

# Electricity 
electric <- c('electric', 'plant', 'power')
electric_index <- grep(paste(electric, collapse='|'), historical_sites$descriptn, ignore.case = T)
historical_sites[electric_index, ]$type <- 'Electricity Plant'

historical_sites[historical_sites$type == 0,]$type <- 'Other'


# NZ
ggplot(coast) + 
  geom_sf(fill = 'white') + 
  geom_sf(data = historical_sites[historical_sites$type != 'Other',], aes(colour = type), size = 2, alpha = 0.75) +
  coord_sf(xlim = c(166, 179), ylim = c(-34, -48)) +
  scale_colour_manual(values = c('#FFE123', '#274060','#A8D84E', '#EA8800', '#4A8FE7')) +
  labs(colour = 'Top 5 Street Names') +
  theme(panel.background = element_rect(fill = 'white'), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), plot.title = element_text(size = 16, 
                                                                                                                          hjust = 0.5, face = 'bold'), legend.position = c(0.8, 0.2), legend.title = element_text(face = 'bold')) +
  ggtitle("Most Common Street Names in New Zealand")



# Auckland
ggplot(coast) + 
  geom_sf() + 
  geom_sf(data = historical_sites, aes(colour = type), size = 2, alpha = 0.8) +
  coord_sf(xlim = c(174, 176), ylim = c(-36, -37.5)) 


# North Island
ggplot(coast) + 
  geom_sf() + 
  geom_sf(data = historical_sites[historical_sites$type != 'Other',], aes(colour = type), size = 2, alpha = 0.8) +
  coord_sf(xlim = c(170, 179), ylim = c(-34, -41.5)) 




# South Island
ggplot(coast) + 
  geom_sf(fill = 'white') + 
  geom_sf(data = historical_sites[historical_sites$type != 'Other',], aes(colour = type), size = 3, alpha = 0.85) +
  coord_sf(xlim = c(166, 174.15), ylim = c(-40.25, -47.6)) +
  scale_colour_manual(values = c('#FF1053', '#6C6EA0','#66C7F4', '#C1CAD6', '#FFBC42', '#BC8034',
                                 '#8F2D56', '#C8E9A0', '#20BF55', '#090446', '#786F52', '#A06CD5', '#EAE151', '#EE6C4D')) +
  labs(colour = 'Type of Historic Place') +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), panel.background = element_rect(fill = 'white'), 
        plot.title = element_text(size = 16, hjust = 0.5, face = 'bold'), legend.position = c(0.15, 0.75), legend.title = element_text(face = 'bold')) +
  ggtitle("Historic Places in New Zealand's South Island")
  
ggsave("16 Places South Island.png", width = 8, height = 10, device = png(), path = "C:\\Users\\emma.vitz\\Documents\\")


# North Island
ggplot(coast) + 
  geom_sf(fill = 'white') + 
  geom_sf(data = historical_sites[historical_sites$type != 'Other',], aes(colour = type), size = 3, alpha = 0.85) +
  coord_sf(xlim = c(170, 179), ylim = c(-34, -41.5))+
  scale_colour_manual(values = c('#FF1053', '#6C6EA0','#66C7F4', '#C1CAD6', '#FFBC42', '#BC8034',
                                 '#8F2D56', '#C8E9A0', '#20BF55', '#090446', '#786F52', '#A06CD5', '#EAE151', '#EE6C4D')) +
  labs(colour = 'Type of Historic Place') +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), panel.background = element_rect(fill = 'white'), 
        plot.title = element_text(size = 16, hjust = 0.5, face = 'bold'), legend.position = c(0.15, 0.75), legend.title = element_text(face = 'bold')) +
  ggtitle("Historic Places in New Zealand's North Island")

ggsave("16 Places North Island.png", width = 8, height = 10, device = png(), path = "C:\\Users\\emma.vitz\\Documents\\")

