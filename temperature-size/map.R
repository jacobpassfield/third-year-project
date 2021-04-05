library(ggmap)
library(tidyverse)

load(file = "temperature-size/data/spatial_data.RData")

geo_data <- spatial_data %>% 
  group_by(Geogroup) %>%
  summarise(Latitude = mean(SiteLat), Longitude = mean(SiteLong))

bbox <- c(103, -45, 170, -5)

map <- get_map(location = bbox, source = "stamen", maptype = "toner-lite")

(ausMap <- ggmap(map))

LonLatAus <- geo_data %>% filter(bbox[1] <= Longitude, Longitude <= bbox[3],
                                 bbox[2] <= Latitude, Latitude <= bbox[4])

ausMap +
  geom_point(aes(x = Longitude, y = Latitude), shape = 8, size = 3, colour = "red", data = LonLatAus) +
  theme(legend.position="none")

# Saving map

# pdf to start the plot
pdf(file = "temperature-size/MapOfAustralia.pdf")
# Create the plot
ausMap +
  geom_point(aes(x = Longitude, y = Latitude), shape = 8, size = 3, colour = "red", data = LonLatAus) +
  theme(legend.position="none")
# Create the file
dev.off()

# https://bookdown.org/ndphillips/YaRrr/saving-plots-to-a-file-with-pdf-jpeg-and-png.html (11.8)
