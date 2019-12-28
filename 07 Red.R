
#### ---- Load libraries and set up parameters ---- ####

library(data.table)
library(janitor)
library(ggplot2)
library(sf)
library(viridis)
library(rvest)
library(dplyr)

data_folder <- "C:\\Users\\emma.vitz\\Documents\\30 Day Map Challenge\\Data\\"
perils_folder <- "W:\\2019\\NZ Perils19\\Data\\"

crs_code <- 4326
crs_code_meters <- 2193

#### ---- Read in the data  and clean ---- ####

# Schools data 
firestations <- fread(paste0(data_folder, 'FENZ Fire Station 23042019.csv'))
coast <- st_read(paste0(perils_folder, "NZ Coastline Files\\nz-coastlines-and-islands-polygons-topo-150k.shp"))


firestations_sf <- st_as_sf(firestations, coords = c('Xcoord', 'Ycoord'), crs = crs_code_meters)
firestations_sf <- st_transform(firestations_sf, crs_code)

coords <- st_coordinates(firestations_sf)

firestations <- cbind(firestations, coords)

firestations <- firestations[X > 0,]

ggplot(data = firestations, aes(x = X, y = Y, colour = StationType)) + 
  geom_point() 

#### ---- Get incidents data ---- ####

url <- "https://fireandemergency.nz/incident-reports/"
days_of_week <- c('Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday', 'Monday', 'Tuesday')
regions <- c(1, 2, 3)

for(region in regions){
  
  list_regions <- list() 
  
  for(day in days_of_week){
    
    print(paste0("Up to day ", day))
    
    i <- which(days_of_week == day)
    
    url_complete <- paste0(url, 'incidents?region=', region, '&day=', day)
    
    webpage <- read_html(url_complete)
    
    text <- html_text(webpage)
    text <- gsub("\t", "", text)
    text <- strsplit(text, '\n')
    text <- unlist(text)
    text <- text[text!= ""]
    
    first <- which(text == "Incident number")[[1]]
    text <- text[first:length(text)] # 1:12 
    
    incidents_index <- which(text == 'Incident number')
    incidents_no <- text[incidents_index+1]
    
    date_index <- which(text == 'Date and time')
    date_time <- text[date_index+1]
    
    loc_index <- which(text == 'Location')
    locations <- text[loc_index+1]
    
    dur_index <- which(text == 'Duration')
    durations <- text[dur_index+1]
    
    station_index <- which(text == 'Station')
    stations <- text[station_index+1]
    
    result_index <- which(text == 'Result')
    results <- text[result_index+1]
    
    incidents_DT <- as.data.table(cbind(incidents_no, date_time, locations, durations, stations, results))
    
    list_regions[[i]] <- incidents_DT
    
  }
  
  region_DT <- rbindlist(list_regions)
  
  fwrite(region_DT, paste0(data_folder, "Incidents\\Region_Number_", region, ".csv"))
  
  print(paste0("Region ", region, " done."))
  
}

#### ---- Clean and merge ---- ####
region1_DT <- fread(paste0(data_folder, "Incidents\\Region_Number_1.csv"))
region2_DT <- fread(paste0(data_folder, "Incidents\\Region_Number_2.csv"))
region3_DT <- fread(paste0(data_folder, "Incidents\\Region_Number_3.csv"))

firestations <- clean_names(firestations)

firestations[, station_name:= toupper(station_name)]
firestations[, locality_name:= toupper(locality_name)]

#### ---- Region 1 ---- ####

region1_DT[, locations_join:= gsub(" AUCKLAND", "", locations)]
region1_DT[, stations:= toupper(stations)]
region1_DT[stations == "RAGLAN EMERGENCY SERVICES BUILDING", stations:= "RAGLAN"]

# Coordinates
region1_stations <- merge(region1_DT, firestations[, -c('xcoord', 'ycoord')], by.x = 'locations_join', by.y = 'station_name', all.x = T)
region1_stations <- merge(region1_stations, firestations[, -c('xcoord', 'ycoord')], by.x = 'stations', by.y = 'locality_name', all.x = T)

region1_stations[!is.na(x.x), longitude:= x.x]
region1_stations[!is.na(x.y), longitude:= x.y]

region1_stations[!is.na(y.y), latitude:= y.y]
region1_stations[!is.na(y.x), latitude:= y.x]

region1_stations[, c('x.x', 'x.y', 'y.y', 'y.x'):= NULL]

# Territorial Authority
region1_stations[!is.na(territorial_authority_name.x), territorial_authority:= territorial_authority_name.x]
region1_stations[!is.na(territorial_authority_name.y), territorial_authority:= territorial_authority_name.y]

region1_stations[, c('territorial_authority_name.x', 'territorial_authority_name.y'):= NULL]

region1_stations[!is.na(y.y), latitude:= y.y]
region1_stations[!is.na(y.x), latitude:= y.x]

region1_stations[, c('x.x', 'x.y', 'y.y', 'y.x'):= NULL]

# Primary Address
region1_stations[!is.na(primary_address.x), primary_address:= primary_address.x]
region1_stations[!is.na(primary_address.y), primary_address:= primary_address.y]

region1_stations[, c('primary_address.x', 'primary_address.y'):= NULL]

# Primary Address
region1_stations[!is.na(station_type.x), station_type:= station_type.x]
region1_stations[!is.na(station_type.y), station_type:= station_type.y]

region1_stations[, c('station_type.x', 'station_type.y'):= NULL]

# Join using station name (firestations) and stations (region1_stations)
region1_stations <- merge(region1_stations, firestations[, c('station_name','x', 'y')], by.x = 'stations', by.y = 'station_name', all.x = T)

region1_stations[is.na(longitude), longitude:= x]
region1_stations[is.na(latitude), longitude:= y]
region1_stations[, c('x', 'y'):= NULL]

region1_stations <- region1_stations[!is.na(longitude),]

region1_stations[, hours:= as.integer(substr(durations, 1, 2))]
region1_stations[, minutes:= as.integer(substr(durations, 4, 5))]
region1_stations[, seconds:= as.integer(substr(durations, 7, 8))]

region1_stations[, total_time_mins:= (hours*60) + minutes + (seconds/60)]

region1_stations <- region1_stations[longitude > 0,]

region1_stations[, code:= substr(results, 1, 4)]

region1_stations[code == "7100", type:= "FALSE ALARM"]
region1_stations[code == "1300", type:= "VEGETATION FIRE"]
region1_stations[code == "1100", type:= "STRUCTURE FIRE"]
region1_stations[code == "1500", type:= "MISC FIRE"]
region1_stations[code == "3100", type:= "MEDICAL EMERGENCY"]
region1_stations[code == "9900", type:= "OTHER NON-FIRE"]
region1_stations[code == "1400", type:= "CHEMICAL FIRE"]
region1_stations[code == "1200", type:= "MOBILE PROPERTY FIRE"]
region1_stations[is.na(type), type:= NA]


region1_stations[, date:= substr(date_time, 1, 10)]



#### ---- Region 2 ---- ####
region2_DT[, stations:= toupper(stations)]

# Coordinates
region2_stations <- merge(region2_DT, firestations[, -c('xcoord', 'ycoord')], by.x = 'stations', by.y = 'locality_name', all.x = T)
region2_stations <- merge(region2_stations, firestations[, -c('xcoord', 'ycoord')], by.x = 'stations', by.y = 'station_name', all.x = T)

region2_stations[!is.na(x.x), longitude:= x.x]
region2_stations[!is.na(x.y), longitude:= x.y]

region2_stations[!is.na(y.y), latitude:= y.y]
region2_stations[!is.na(y.x), latitude:= y.x]

region2_stations[, c('x.x', 'x.y', 'y.y', 'y.x'):= NULL]

# Territorial Authority
region2_stations[!is.na(territorial_authority_name.x), territorial_authority:= territorial_authority_name.x]
region2_stations[!is.na(territorial_authority_name.y), territorial_authority:= territorial_authority_name.y]

region2_stations[, c('territorial_authority_name.x', 'territorial_authority_name.y'):= NULL]

# Primary Address
region2_stations[!is.na(primary_address.x), primary_address:= primary_address.x]
region2_stations[!is.na(primary_address.y), primary_address:= primary_address.y]

region2_stations[, c('primary_address.x', 'primary_address.y'):= NULL]

# Primary Address
region2_stations[!is.na(station_type.x), station_type:= station_type.x]
region2_stations[!is.na(station_type.y), station_type:= station_type.y]

region2_stations[, c('station_type.x', 'station_type.y'):= NULL]

region2_stations[, hours:= as.integer(substr(durations, 1, 2))]
region2_stations[, minutes:= as.integer(substr(durations, 4, 5))]
region2_stations[, seconds:= as.integer(substr(durations, 7, 8))]

region2_stations[, total_time_mins:= (hours*60) + minutes + (seconds/60)]

region2_stations <- region2_stations[longitude > 0,]

region2_stations[, code:= substr(results, 1, 4)]

region2_stations[code == "7100", type:= "FALSE ALARM"]
region2_stations[code == "1300", type:= "VEGETATION FIRE"]
region2_stations[code == "1100", type:= "STRUCTURE FIRE"]
region2_stations[code == "1500", type:= "MISC FIRE"]
region2_stations[code == "3100", type:= "MEDICAL EMERGENCY"]
region2_stations[code == "9900", type:= "OTHER NON-FIRE"]
region2_stations[code == "1400", type:= "CHEMICAL FIRE"]
region2_stations[code == "1200", type:= "MOBILE PROPERTY FIRE"]
region2_stations[is.na(type), type:= NA]

region2_stations[, date:= substr(date_time, 1, 10)]



#### ---- Region 3 ---- ####
region3_DT[, stations:= toupper(stations)]

# Coordinates
region3_stations <- merge(region3_DT, firestations[, -c('xcoord', 'ycoord')], by.x = 'stations', by.y = 'locality_name', all.x = T)
region3_stations <- merge(region3_stations, firestations[, -c('xcoord', 'ycoord')], by.x = 'stations', by.y = 'station_name', all.x = T)

region3_stations[!is.na(x.x), longitude:= x.x]
region3_stations[!is.na(x.y), longitude:= x.y]

region3_stations[!is.na(y.y), latitude:= y.y]
region3_stations[!is.na(y.x), latitude:= y.x]

region3_stations[, c('x.x', 'x.y', 'y.y', 'y.x'):= NULL]

chch_flag <- grep('CHRISTCHURCH CITY', region3_stations$locations)
region3_stations[chch_flag, stations:= 'CHRISTCHURCH CITY']

region3_stations <- merge(region3_stations, firestations[, -c('xcoord', 'ycoord')], by.x = 'stations', by.y = 'station_name', all.x = T)

region3_stations[is.na(longitude), longitude:= x]
region3_stations[is.na(latitude), latitude:= y]

region3_stations[, c('x', 'y'):= NULL]


# Territorial Authority
region3_stations[!is.na(territorial_authority_name.x), territorial_authority:= territorial_authority_name.x]
region3_stations[!is.na(territorial_authority_name.y), territorial_authority:= territorial_authority_name.y]

region3_stations[, c('territorial_authority_name.x', 'territorial_authority_name.y'):= NULL]

# Primary Address
region3_stations[!is.na(primary_address.x), primary_address:= primary_address.x]
region3_stations[!is.na(primary_address.y), primary_address:= primary_address.y]

region3_stations[, c('primary_address.x', 'primary_address.y'):= NULL]

# Primary Address
region3_stations[!is.na(station_type.x), station_type:= station_type.x]
region3_stations[!is.na(station_type.y), station_type:= station_type.y]

region3_stations[, c('station_type.x', 'station_type.y'):= NULL]


# Loicality
region3_stations[!is.na(locality_name.x), locality_name:= locality_name.x]
region3_stations[!is.na(locality_name.y), locality_name:= locality_name.y]

region3_stations[, c('locality_name.x', 'locality_name.y'):= NULL]

region3_stations[, hours:= as.integer(substr(durations, 1, 2))]
region3_stations[, minutes:= as.integer(substr(durations, 4, 5))]
region3_stations[, seconds:= as.integer(substr(durations, 7, 8))]

region3_stations[, total_time_mins:= (hours*60) + minutes + (seconds/60)]

region3_stations <- region3_stations[longitude > 0,]

region3_stations[, code:= substr(results, 1, 4)]

region3_stations[code == "7100", type:= "FALSE ALARM"]
region3_stations[code == "1300", type:= "VEGETATION FIRE"]
region3_stations[code == "1100", type:= "STRUCTURE FIRE"]
region3_stations[code == "1500", type:= "MISC FIRE"]
region3_stations[code == "3100", type:= "MEDICAL EMERGENCY"]
region3_stations[code == "9900", type:= "OTHER NON-FIRE"]
region3_stations[code == "1400", type:= "CHEMICAL FIRE"]
region3_stations[code == "1200", type:= "MOBILE PROPERTY FIRE"]
region3_stations[is.na(type), type:= NA]

region3_stations[, date:= substr(date_time, 1, 10)]
region3_stations[, territorial_authority_name:= NULL]
region1_stations[, locations_join:= NULL]

regions <- rbind(region1_stations, region2_stations, region3_stations)

# Correct latitude
regions[stations == "ROSLYN", latitude := -45.86420]

# Classify types
regions[, type_broad:= 'Fire']
regions[type %in% c(NA, 'OTHER NON-FIRE', 'MEDICAL EMERGENCY', 'FALSE ALARM'), type_broad:= 'Non-Fire']

#### ---- Plot ---- ####
ggplot(data = regions, aes(x = longitude, y = latitude, size = total_time_mins, colour = type_broad)) + 
  geom_point(alpha = 0.7) +
  scale_colour_manual(values = c('#F63E02', '#53B3CB')) +
  theme_bw() +
  coord_fixed() +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), legend.position = 'bottom')


ggplot(data = coast) + 
  geom_sf(colour = 'white') +
  coord_sf(xlim = c(166, 179), ylim = c(-33.6, -48), expand = FALSE) +
  geom_point(data = regions, aes(x = longitude, y = latitude, size = total_time_mins, colour = type_broad), alpha = 0.7) +
  scale_colour_manual(values = c('#F63E02', '#F9C22E')) +
  theme_bw() +
  labs(size = "Total Time of \nIncident (Mins)", colour = "Type of Incident") +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), legend.position = "right") +
  ggtitle("Incidents Reported to Firestations in \nNew Zealand in the Week of Guy Fawkes 2019")

ggsave("07 Red.png", device = png(), path = "C:\\Users\\emma.vitz\\Documents\\30 Day Map Challenge\\")



