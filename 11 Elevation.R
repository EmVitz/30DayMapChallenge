
library(data.table)
library(sf)
library(ggplot2)


data_folder <- "C:\\Users\\emma.vitz\\Documents\\30 Day Map Challenge\\Data\\"
perils_folder <- "W:\\2019\\NZ Perils19\\Data\\"

crs_code <- 4326
crs_code_meters <- 2193

dwellings_90mDEM <- readRDS("W:\\2019\\NZ Perils19\\FNAF\\FNAF_90MDEM_DWELLINGS.RDS")
setDT(dwellings_90mDEM)

coast <- st_read(paste0(perils_folder, 'NZ Coastline Files\\nz-coastlines-and-islands-polygons-topo-150k.shp'))


dwellings_90mDEM[, random_no:= runif(nrow(dwellings_90mDEM), 0, 1)]
sample_dwellings <- dwellings_90mDEM[random_no < 0.3,]


dwellings_sf <- st_as_sf(sample_dwellings, coords = c('X_COORDINATE', 'Y_COORDINATE'), crs = crs_code)

dwellings_sample1 <- st_sample(dwellings_sf, 1000000)


ggplot(coast) + 
  geom_sf(colour = 'white', fill = 'white') +
  coord_sf(xlim = c(166, 179), ylim = c(-33.5, -47.5)) +
  geom_point(data = sample_dwellings, aes(x = X_COORDINATE, y = Y_COORDINATE, colour = Height_90mDEM), size = 0.1, alpha = 0.8) +
  scale_colour_gradient2(low = '#442D2B', mid = '#DD7871', high = '#FFFAF9', midpoint = 250) +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), panel.background = element_rect(fill = 'white'), 
        plot.title = element_text(hjust = 0.5, size = 16)) +
  ggtitle("Elevation of Dwellings in New Zealand") +
  labs(colour = "Elevation (m)")

ggsave("11 Elevation.png", width = 6, height = 8, device = png(), path = "C:\\Users\\emma.vitz\\Documents\\")

ggsave("11 Elevation_v2.png", device = png(), path = "C:\\Users\\emma.vitz\\Documents\\")
