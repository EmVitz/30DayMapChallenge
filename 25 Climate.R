
#### ---- Load libraries ---- ####

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
library(openxlsx)
library(gganimate)
library(lubridate)
library(xlsx)

AACI_folder <- 'M:/Perils19/Climate/AACI/2019_3/Data/'

#### ---- Read in the data and clean ---- ####

aus <- st_read("M:/finpoint19/R&D/Spatial Data/ABS_Shapefiles/State/STE_2016_AUST.shp")
aus <- st_union(aus)

stations1 <- xlsx::read.xlsx("N:/Perils17/Climate Change/Australian Actuarial Climate Index/0. Data/Clusters/Weatherstation_NRM_clusters.xlsm", 
                       sheetName = 'Wind')
setDT(stations1)
stations1 <- clean_names(stations1)
stations1[, c('cluster', 'sub_cluster', 'na', 'na_1'):= NULL]


stations2 <- xlsx::read.xlsx("N:/Perils17/Climate Change/Australian Actuarial Climate Index/0. Data/Clusters/Weatherstation_NRM_clusters.xlsm", 
                       sheetName = 'ACORN-SAT')
setDT(stations2)
stations2 <- clean_names(stations2)
stations2[, c('cluster', 'subcluster'):= NULL]

setnames(stations2, c('station_na', 'number'), c('station_name', 'station_number'))
setcolorder(stations2, neworder = c('station_number', 'station_name', 'latitude', 'longitude'))

stations2 <- stations2[!(station_number %in% unique(stations1$station_number)), ]

stations <- rbind(stations1, stations2)

#### ---- Process all weather observations ---- ####

file_names <- list.files(paste0(AACI_folder, 'Temperature'))
temp_list <- list()

for(file in file_names) {
  
  i <- which(file_names == file)
  
  temp <- read.table(paste0(AACI_folder, 'Temperature/', file), sep = ',', header = T)
  setDT(temp)
  temp[, c('dc', 'X', 'Days.of.accumulation.of.minimum.temperature', 'Days.of.accumulation.of.maximum.temperature'):= NULL]
  setnames(temp, c('Maximum.temperature.in.24.hours.after.9am..local.time..in.Degrees.C', 'Minimum.temperature.in.24.hours.before.9am..local.time..in.Degrees.C', 
                   'Quality.of.maximum.temperature.in.24.hours.after.9am..local.time.', 'Quality.of.minimum.temperature.in.24.hours.before.9am..local.time.'),
           c('max_temp', 'min_temp', 'quality_max_temp', 'quality_min_temp'))
  
  temp_list[[i]] <- temp
  
  print(paste0("File ", i, " of ", length(file_names), ' done'))
  
}

temp_all <- rbindlist(temp_list)
temp_all <- clean_names(temp_all)

temp_all <- merge(temp_all, stations, by = 'station_number', all.x = T)

temp_all[, date:= as.Date(paste0(year, '-', month, '-', day))]

monthly_max <- temp_all[, .(monthly_high = max(max_temp, na.rm = T), latitude = max(latitude), longitude = max(longitude)), by = c('station_number', 'month', 'year')]
monthly_max <- monthly_max[!is.infinite(monthly_high), ]

monthly_max[, month_beg:= as.Date(paste0(year, '-', month, '-01'))]

stations_remove <- monthly_max[,.(count = .N), by = c('station_number', 'latitude', 'longitude')][count != 19, station_number]
monthly_max <- monthly_max[!(station_number %in% stations_remove),]

#### ---- Plot and animate ---- ####

plot <- ggplot(aus) +
  geom_sf(fill = 'white') +
  coord_sf(xlim = c(112, 155)) +
  geom_point(data = monthly_max, aes(x = longitude, y = latitude, colour = monthly_high), size = 3) + 
  scale_colour_viridis(option = "magma", direction = -1) +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(),panel.background = element_rect(fill = 'white'),
        plot.title = element_text(hjust = 0.5, size = 16, colour = 'black', face = 'bold'), panel.grid.major = element_blank(), legend.position = c(0.9, 0.8)) +
  ggtitle("Monthly Maximum Temperatures from April 2018 to Oct 2019") +
  labs(colour = "Monthly \nMax Temp")

plot + 
  transition_time(month_beg) + 
  labs(title = "Maximum Temperature in Month Beginning: \n{round_date(frame_time, unit = 'month')}")

anim_save('25 Climate 2.gif',  path = "C:\\Users\\emma.vitz\\Documents\\")

