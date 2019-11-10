
library(sf)
library(data.table)
library(ggplot2)
library(grid)
library(maps)
library(janitor)
library(rmapshaper)
library(viridis)

folder <- "~/Documents/30 Day Map Challenge/Data/"

coast <- st_read(paste0(folder, 'lds-nz-coastlines-topo-150k-SHP/nz-coastlines-topo-150k.shp'))

reg_council <- st_read(paste0(folder, 'statsnzregional-council-2015-v1-00-clipped-SHP/regional-council-2015-v1-00-clipped.shp'))
reg_council <- reg_council[reg_council$REGC2015_1 != 'Area Outside Region',]

reg_council_simpl <- ms_simplify(reg_council, keep_shapes = T)

livestock <- fread(paste0(folder, "Livestock/livestock.csv"))
dairy <- livestock[Livestock == 'Total dairy cattle',]

dairy[, year_jun30:= as.integer(substr(Year, nchar(Year)-4, nchar(Year)))]
dairy <- dairy[!(Area %in% c('Total New Zealand', 'Chatham Islands')),]
dairy[, c('Livestock', 'Year'):= NULL]
dairy[, regional_council:= paste0(Area, " Region")]

ggplot(dairy, aes(x = year_jun30, y = Value, colour = Area)) + 
  geom_line()

dairy_change <- merge(dairy[year_jun30 == 1994, ], dairy[year_jun30 == 2017], 
      by = c('Area', 'regional_council'), all = T)

dairy_change[, c('Flags.x', 'Flags.y', 'year_jun30.x', 'year_jun30.y'):= NULL]

setnames(dairy_change, c('Value.x', 'Value.y'), c('no_dairy_cattle_94', 'no_dairy_cattle_17'))

dairy_change[, perc_change:= ((no_dairy_cattle_17 / no_dairy_cattle_94) - 1) ]
dairy_change[, log_perc_change:= log(((no_dairy_cattle_17 / no_dairy_cattle_94) - 1)) ]


reg_council_dairy <- merge(reg_council_simpl, dairy_change, by.x =  'REGC2015_1', by.y = 'regional_council', all.x = T)


ggplot(reg_council_dairy, aes(fill = perc_change, colour = perc_change)) +
  geom_sf() +
  scale_fill_gradient2(low = '#67af42', mid = '#f7f5dc', high = '#ffcc00', midpoint = 0, 
                       labels = scales::percent) + 
  scale_colour_gradient2(low = '#67af42', mid = '#f7f5dc', high = '#ffcc00', midpoint = 0,
                         guide = F) +
  theme_dark() +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), 
        legend.text = element_text(size = 11, colour = 'white'), legend.title = element_text(colour = 'white'),
        plot.background = element_rect(fill = 'black'), panel.background = element_rect(fill = 'black'),
        panel.grid.major = element_blank(), plot.title = element_text(size = 14, colour = 
        'white', face ='bold'), legend.background = element_rect(fill = 'black')) +
  labs(fill ="Change in the Number\nof Dairy Cattle") +
  ggtitle("Regional Changes in the Number of Dairy Cattle from 1994 to 2017")

ggsave("09 Yellow.png", device = png(), path = "~/Documents/30 Day Map Challenge/Maps/")


# Log scale
ggplot(reg_council_dairy, aes(fill = log_perc_change, colour = log_perc_change)) +
  geom_sf() +
  scale_fill_gradient2(low = '#67af42', mid = '#f7f5dc', high = '#ffcc00', midpoint = 0) + 
  scale_colour_gradient2(low = '#67af42', mid = '#f7f5dc', high = '#ffcc00', midpoint = 0,
                         guide = F) +
  theme_dark() +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), 
        legend.text = element_text(size = 11, colour = 'white'), legend.title = element_text(colour = 'white'),
        plot.background = element_rect(fill = 'black'), panel.background = element_rect(fill = 'black'),
        panel.grid.major = element_blank(), plot.title = element_text(size = 14, colour = 
        'white', face ='bold'), legend.background = element_rect(fill = 'black')) +
  labs(fill ="Log of Change in the \nNumber of Dairy Cattle") +
  ggtitle("Regional Changes in the Number of Dairy Cattle from 1994 to 2017")

ggsave("09 Yellow Log.png", device = png(), path = "~/Documents/30 Day Map Challenge/Maps/")




