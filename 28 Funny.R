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
library(ggsn)
library(extrafont)
library(gridExtra)
library(grid)
library(gganimate)
library(transformr)


#### ---- Set up parameters and read in data ---- ####

stats_nz_folder <- "K:/R&D/Personal Lines/External Data/Stats NZ/"
nz_folder <- "K:\\R&D\\Personal Lines\\External Data\\Addressing\\NZ\\"
perils_folder <- "W:\\2019\\NZ Perils19\\Data\\"

crs_code <- 4326
crs_code_meters <- 2193

ta <- st_read(paste0(nz_folder, "Territorial Authorities Shapefiles/territorial-authority-2018-clipped-generalised.shp"))
ta <- ta[ta$TA2018_V_1 != 'Chatham Islands Territory' & ta$TA2018_V_1 != 'Area Outside Territorial Authority',]

dogs <- fread('C:\\Users\\emma.vitz\\Documents\\30 Day Map Challenge\\Data\\Dogs.csv')
dogs <- clean_names(dogs)
dogs[grep('Female', measure), dog_sex:= 'Female']
dogs[grep('Male', measure), dog_sex:= 'Male']
dogs[territorial_authority_code == 'Wanganui District', territorial_authority_code:= 'Whanganui District']


dog_names <- fread('C:\\Users\\emma.vitz\\Documents\\30 Day Map Challenge\\Data\\2017.csv')
dog_names <- clean_names(dog_names)

lab <- jpeg::readJPEG('C:\\Users\\emma.vitz\\Documents\\30 Day Map Challenge\\Data\\Labrador.jpg')
jack <- jpeg::readJPEG('C:\\Users\\emma.vitz\\Documents\\30 Day Map Challenge\\Data\\jack.jpg')
vs <- jpeg::readJPEG('C:\\Users\\emma.vitz\\Documents\\30 Day Map Challenge\\Data\\vs.jpg')

#### ---- Clean data (breed mapping etc) ---- ####

dogs <- dogs[grouping_variable == 'Registered dogs' & measure %in% c('Female dogs', 'Male dogs'),]
dogs[, category:= toupper(category)]
dogs[, c('value_unit', 'value_label', 'null_reason'):= NULL]

dog_names[grep('Female', license_type), dog_sex:= 'Female']
dog_names[grep('Male', license_type), dog_sex:= 'Male']
dog_names[grep('Duplicate', license_type), remove:= 1]
dog_names[grep('Transfer', license_type), remove:= 1]
dog_names <-dog_names[is.na(remove),]
dog_names[, remove:= NULL]

most_common_names <-dog_names[,.(count = .N), by = c('breed', 'dog_sex', 'dog_name')]
setorder(most_common_names, 'breed', 'dog_sex', -'count')
most_common_names <- most_common_names[, head(.SD, 1), by = c('breed', 'dog_sex')]

dogs[category == 'TERRIER, JACK RUSSELL', category:= 'JACK RUSSEL TERRIER']
dogs[category == 'TERRIER, FOX (SMOOTH)', category:= 'FOX TERRIER SMOOTH']
dogs[category %in% c('SPANIEL, COCKER', "SPANIEL, CAVALIER KING CHARLES"), category:= 'COCKER SPANIEL']
dogs[category == 'SPANIEL, ENGLISH SPRINGER', category:= 'ENG COCKER SPANIEL']
dogs[category %in% c('CHIHUAHUA, LONG COAT', 'CHIHUAHUA, SMOOTH COAT'), category:= 'CHIHUAHUA']
dogs[category  == 'COLLIE, BEARDED', category:= 'BEARDED COLLIE']
dogs[category  == 'COLLIE, BORDER', category:= 'BORDER COLLIE']
most_common_names[breed == 'BORD COLLIE', breed:= 'BORDER COLLIE']

dogs[category  %in% c('COLLIE, SMOOTH', 'COLLIE, ROUGH'), category:= 'COLLIE']
dogs[category  == 'CATTLE, AUSTRALIAN', category:= 'AUS CATTLE DOG']
dogs[category  == 'POODLE, TOY', category:= 'POODLE TOY']
dogs[category  == 'POODLE, MINIATURE', category:= 'POODLE MIN']
dogs[category  == 'RETRIEVER, GOLDEN', category:= 'GOLDEN RETRIEVER']
dogs[category  == 'RETRIEVER, LABRADOR', category:= 'LABRADOR RETRIEVER']
dogs[category  == "TERRIER, WEST HIGHLAND WHITE", category:= 'W HGHLND WH TERRIER']
dogs[category  == "TERRIER, STAFFORDSHIRE BULL", category:= 'AM STAFF TERRIER']
dogs[category  == "TERRIER, AMERICAN PIT BULL", category:= 'AM PIT BULL TERRIER']
dogs[category  == "SHEPHERD, GERMAN", category:= 'GER SHEPHERD']
dogs[category  == "SCHNAUZER, MINIATURE", category:= 'SCHNAUZER MIN']
dogs[category  == "BRAZILIAN FILA", category:= 'FILA BRASILEIRO']

dogs_w_name <- merge(dogs, most_common_names, by.x = c('category', 'dog_sex'), by.y = c('breed', 'dog_sex'), all.x = T)
dogs_w_name <- dogs_w_name[!(category %in% c('ALL CROSS BREEDS', 'ALL PURE AND CROSS BREEDS', 'ALL PURE BREEDS', 'OTHER PURE BREEDS')) 
                           & !is.na(dog_name) & value != 0,]

summary <- dogs_w_name[,.(n_dogs = sum(value)), by = c('category', 'dog_sex', 'year_ended_june', 'territorial_authority_code', 'dog_name')]

setorder(summary, 'territorial_authority_code', 'year_ended_june', 'dog_sex', -'n_dogs')

most_common_overall <- summary[, .(count = sum(n_dogs)), by = c('territorial_authority_code', 'category', 'dog_sex', 'dog_name')]
most_common_overall <- most_common_overall[, head(.SD, 1), by = c('dog_sex', 'territorial_authority_code')]

male_most_common <- most_common_overall[dog_sex == 'Male',]
male_most_common[, breed_name:= paste0(category, ': ', dog_name)]


female_most_common <- most_common_overall[dog_sex == 'Female',]
female_most_common[, breed_name:= paste0(category, ': ', dog_name)]

ta_dogs_alltime_male <- merge(ta, male_most_common, by.x = 'TA2018_V_1', by.y ='territorial_authority_code', all.x = T)
ta_dogs_alltime_female <- merge(ta, female_most_common, by.x = 'TA2018_V_1', by.y ='territorial_authority_code', all.x = T)

lab <- rasterGrob(lab, interpolate = T)
jack <- rasterGrob(jack, interpolate = T)
vs <- rasterGrob(vs, interpolate = T)

#### ---- Plot results ---- ####

male_dogs <- ggplot(ta_dogs_alltime_male, aes(fill = breed_name, colour = breed_name)) +
  geom_sf() +
  scale_fill_manual(values = c('#F7F380', '#A4D89C', '#B24343', '#4A93CE', '#FFA954'), 
                    limits = c('LABRADOR RETRIEVER: COOPER', 'BORDER COLLIE: MAX', 'FOX TERRIER SMOOTH: DEACON', 'COLLIE: LASSIE', 
                               'JACK RUSSEL TERRIER: JACK')) +
  scale_colour_manual(values = c('#F7F380', '#A4D89C', '#B24343', '#4A93CE', '#FFA954'), guide = F, 
                      limits = c('LABRADOR RETRIEVER: COOPER', 'BORDER COLLIE: MAX', 'FOX TERRIER SMOOTH: DEACON', 'COLLIE: LASSIE', 
                                 'JACK RUSSEL TERRIER: JACK')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white')) +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), legend.position = c(0.2, 0.8), 
        plot.title = element_text(size = 16, hjust = 0.5, vjust = 0.5, face = 'bold')) +
  annotation_custom(lab, ymin = -49, xmin = 169, xmax = 171) +
  annotation_custom(jack, ymin = -43, ymax = -45, xmin = 177) +
  annotation_custom(vs, ymin = -34, ymax = -35, xmin = 177) +
  ggtitle("Most Common Dog Breed & Name for Male Dogs by \nTerritorial Authority (2013 to 2019)") +
  labs(fill = 'Most Common Breed and Name')

ggsave("28 Funny Male Dogs v2.png",device = png(), path =  "C:\\Users\\emma.vitz\\Documents\\30 Day Map Challenge")


female_dogs <- ggplot(ta_dogs_alltime_female, aes(fill = breed_name, colour = breed_name)) +
  geom_sf() +
  scale_fill_manual(values = c('#F7F380', '#A4D89C', '#B24343', '#4A93CE', '#FFA954'), limits = c('LABRADOR RETRIEVER: BELLA', 'BORDER COLLIE: LUCY',
                   'FOX TERRIER SMOOTH: SIDNEY', 'COLLIE: LYLA')) +
  scale_colour_manual(values = c('#F7F380', '#A4D89C', '#B24343', '#4A93CE', '#FFA954'), guide = F, 
                      limits = c('LABRADOR RETRIEVER: BELLA', 'BORDER COLLIE: LUCY', 'FOX TERRIER SMOOTH: SIDNEY', 'COLLIE: LYLA')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white')) +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), legend.position = c(0.2, 0.8), 
        plot.title = element_text(size = 16, hjust = 0.5, vjust = 0.5, face = 'bold')) +
  annotation_custom(vs, ymin = -49, xmin = 169, xmax = 171) +
  annotation_custom(lab, ymin = -43, ymax = -46, xmin = 177) +
  annotation_custom(jack, ymin = -34, ymax = -36, xmin = 177) +
  ggtitle("Most Common Dog Breed & Name for Female Dogs by \nTerritorial Authority (2013 to 2019)") +
  labs(fill = 'Most Common Breed and Name')

ggsave("28 Funny Female Dogs v2.png",device = png(), path =  "C:\\Users\\emma.vitz\\Documents\\30 Day Map Challenge")



