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

coast <- st_read('W:\\2019\\NZ Perils19\\Data\\NZ Coastline Files\\nz-coastlines-and-islands-polygons-topo-150k.shp')

wind <- readRDS("D:\\PERILS\\NZ\\ReAnalysis\\RA_wind.RDS")
setDT(wind)

rain <- readRDS("D:\\PERILS\\NZ\\ReAnalysis\\RA_rain.RDS")
setDT(rain)

wind_max <- wind[, .(max_overall = max(max_gust_day)), by = c('longitude', 'latitude')]
rain_max <- rain[, .(max_overall = max(daily_rain)), by = c('longitude', 'latitude')]


ggplot(coast) + 
  geom_sf() +
  coord_sf(xlim = c(166, 179), ylim = c(-33.5, -47.5)) +
  geom_point(data = wind_max, aes(x = longitude, y = latitude, colour = max_overall), alpha = 0.75, size = 2.5) +
  scale_colour_viridis(begin = 1, end = 0) +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), panel.background = element_rect(fill = 'white'), 
        plot.title = element_text(hjust = 0.5, size = 16)) +
  ggtitle("Maximum Wind Gust from 1979 - 2019 \n(Reanalysis Data)") +
  labs(colour = "Maximum Wind \nGust (m/s)")

ggsave("21 Environment.png", device = png(), path = "C:\\Users\\emma.vitz\\Documents\\")


ggplot(coast) + 
  geom_sf() +
  coord_sf(xlim = c(166, 179), ylim = c(-33.5, -47.5)) +
  geom_point(data = rain_max, aes(x = longitude, y = latitude, colour = max_overall), alpha = 0.75, size = 2.5) +
  scale_colour_viridis(begin = 1, end = 0) +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), panel.background = element_rect(fill = 'white'), 
        plot.title = element_text(hjust = 0.5, size = 16)) +
  ggtitle("Maximum Daily Rainfall from 1979 - 2019 \n(Reanalysis Data)") +
  labs(colour = "Maximum Daily \nRainfall (mm)")

ggsave("21 Environment 2.png", device = png(), path = "C:\\Users\\emma.vitz\\Documents\\")
