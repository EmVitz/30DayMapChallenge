
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

b_consent <- fread('W:\\2019\\NZ Perils19\\Temp\\Building Consents\\Building consents by territorial authority and selected wards (Monthly).csv')

#### ---- Process data ---- ####

# Correct old TA names
b_consent[grep('2010 and earlier', Series_title_1), TA:= 'Auckland']
b_consent[grep('2011 and later', Series_title_1), TA:= 'Auckland']
b_consent[grep('2006 and earlier', Series_title_1), TA:= 'Christchurch City']

b_consent[Series_title_1 %in% unique(ta$TA2018_V_1), TA:= Series_title_1]
b_consent <- b_consent[!is.na(TA),]

new_build <- b_consent[Series_title_3 == 'New' & Units == 'Dollars', -c('Series_title_3', 'Units', 'Subject', 'Group', 'Suppressed', 
                                                                        'Status', 'Magnitude', 'Series_title_5', 'Series_title_4', 'Series_title_1')]

new_build <- new_build[!(Series_title_2 %in% c('All construction', 'All buildings', 'Residential buildings',
                                                         'Non-residential buildings', 'Commercial buildings', 'Dwelling units')), ]

new_build[, time:= as.Date(paste0(substr(Period, 1, 4), '-', substr(Period, 6, 7), '-', '01'))]

new_build[, year:= year(time)]

annual_new_build <- new_build[, .(annual_expenditure = sum(Data_value)), by = c('Series_title_2', 'TA', 'year')]

setorder(new_build, 'TA', 'Period', -'Data_value')
setorder(annual_new_build, 'TA', 'year', -'annual_expenditure')

houses <- annual_new_build[Series_title_2 == 'Houses',]
houses <- houses[year %in% c(2019, 1999)]

houses_99_19 <- dcast(houses, TA ~ year, value.var = 'annual_expenditure')
setnames(houses_99_19, c('1999', '2019'), c('exp_1999', 'exp_2019'))

houses_99_19[, comparison:= exp_2019/exp_1999]

ta_comparison <- merge(ta, houses_99_19, by.x = 'TA2018_V_1', by.y = 'TA', all.x = T)

#### ---- Plot ---- ####
ggplot(ta_comparison, aes(fill = comparison, colour = comparison)) +
  geom_sf() + 
  scale_fill_viridis(labels = scales::percent_format(accuracy = 1)) +
  scale_colour_viridis(guide = F) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white')) +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), legend.position = c(0.2, 0.8), 
        plot.title = element_text(size = 16, hjust = 0.5, vjust = 0.5, face = 'bold')) +
  ggtitle("Annual Spending on Building New Houses\n in 2019 compared to 1999") +
  labs(fill = '2019 Relative \nto 1999')

ggsave("22 Built Environment.png", device = png(), path = "C:\\Users\\emma.vitz\\Documents\\")

ggplot(ta_comparison, aes(fill = exp_2019, colour = exp_2019)) +
  geom_sf() + 
  scale_fill_viridis() +
  scale_colour_viridis(guide = F) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white')) +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), legend.position = c(0.2, 0.8), 
        plot.title = element_text(size = 16, hjust = 0.5, vjust = 0.5, face = 'bold')) +
  ggtitle("Annual Spending on Building New Houses\n in 2019 compared to 1999") +
  labs(fill = '2019 Relative \nto 1999')

ggsave("22 Built Environment.png", device = png(), path = "C:\\Users\\emma.vitz\\Documents\\")

