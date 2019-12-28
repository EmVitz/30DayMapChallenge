library(rvest)
library(stringr)
library(ggplot2)
library(data.table)
library(sf)

crs_code <- 4326
crs_code_meters <- 2193

#water_ids <- c(39536, 39537, 39538, 39539, 39540, 39399, 39400, 39401, 39402, 39403)    
#water_ids <- c(39536, 39537, 39538) #, 39539, 39540, 39399, 39400, 39401, 39402, 39403)    


water_id <- "39399"

url <- "https://water.blue-dot-observatory.com/api/waterbodies/"


dates <- seq.Date(from = as.Date("2017-01-01"), to = as.Date(Sys.time()), by = "day")

polygon_list <- list()

#i <- 1

#for(water in water_ids){
  
  #k <- which(water_ids == water)
  
  for(j in 1:length(dates)) {
    
    date <- as.character(dates[[j]])
    
    url_water <- paste0(url, water_id, "/measurements/", as.character(date), "/")
    
    webpage <- read_html(url_water)
    
    text <- html_text(webpage)
    
    text <- strsplit(text, '\n')
    
    text <- unlist(text)
    
    if(length(grep("coordinates", text[2])) > 0) {
      
      print(paste0("A record exists for ", date))
      
      longitude <- text[seq(6, length(text), 4)]
      longitude <- str_trim(longitude, side = 'both')
      end_point_long <- which(substr(longitude, 1, 1) != "1")[1]
      
      latitude <- text[seq(7, length(text), 4)]
      latitude <- str_trim(latitude, side = 'both')
      end_point_lat <- which(substr(latitude, 1, 1) != "-")[1]
      
      polygon <- as.data.table(cbind(latitude[1:end_point_lat-1], longitude[1:end_point_long-1]))
      colnames(polygon) <- c('latitude', 'longitude')
      
      polygon[, longitude:= as.numeric(gsub(',', '', longitude))]
      polygon[, latitude:= as.numeric(gsub(',', '', latitude))]
      
      polygon[, date:= date]
      polygon[, polyid:= 1]
      
      polygon[, order:= .I]
      
      if(end_point_long < length(longitude)) {
        
        polygon2 <- as.data.table(cbind(latitude[(end_point_lat+1):(length(latitude)-1)], longitude[(end_point_long+1):(length(longitude)-1)]))
        colnames(polygon2) <- c('latitude', 'longitude')
        
        polygon2[, longitude:= as.numeric(gsub(',', '', longitude))]
        polygon2[, latitude:= as.numeric(gsub(',', '', latitude))]
        
        polygon2[, date:= date]
        polygon2[, polyid:= 2]
        
        polygon2[, order:= .I]
        
        polygon <- rbind(polygon, polygon2)
        
      }
      
      polygon_list[[j]] <- polygon
      
      #i <- i + 1
      
    }
    
    #water_list[[k]] <- polygon_list
    
    #rm(polygon_list)
    
    #polygon_list <- list()
    #i <- 1
    
  }
  
#}

polygons <- rbindlist(polygon_list)

max_date <- as.Date(max(polygons$date))


polygons_plot <- polygons[date %in% c('2017-01-18', '2018-01-18', '2019-01-18', "2019-12-21"),]
polygons_plot <- polygons_plot[!is.na(longitude),]

ggplot(polygons_plot, aes(x = longitude, y = latitude)) +
  geom_point(colour = "#2BC0E4", size = 0.7) +
  coord_fixed() +
  facet_wrap(.~ as.factor(date)) +
  theme_bw() +
  ggtitle('Burrendong Dam (NSW) over time (2017 - 2019)') +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), panel.border = element_rect(color = '#2a2a2a'),
        strip.text = element_text(face = "bold", size = 12, colour = 'white'), strip.background = element_rect(fill = '#2a2a2a'), 
        plot.title = element_text(face = 'bold', size = 14, colour = '#2a2a2a')) 
  

ggsave("06 Blue Burrendong Dam.png", device = png(), path = "C:\\Users\\emma.vitz\\Documents\\30 Day Map Challenge\\")

