# Comparison of basemaps available in R 

# load packages
library(ggplot2)
library(PBSmapping)
library(sf)
library(rnaturalearth)
library(rgdal)
library(raster)

# rnaturalearth
north.america <- rnaturalearth::ne_countries(scale = "large", continent = "north america",
                                             returnclass = "sf")
str(north.america)
st_crs(north.america) # WGS 84
# transform to UTMs
bcUTM <- st_transform(north.america, crs = 3157) # 3157 is the CRS for UTM zone 10

# convert UTM eastings and northings from metres into kilometres
bcUTM$geometry <- bcUTM$geometry/1000

# map
ggplot() +
  geom_sf(data = bcUTM, col = "black", fill = "grey75", lwd = 0.01) +
  scale_x_continuous(limits = c(100, 560), breaks = c(100, 200, 300, 400, 500)) +
  scale_y_continuous(limits = c(5300, 5650), breaks = c(5300, 5400, 5500, 5600)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
ggsave("figures/rnaturalearth-vancouver-island.PNG", width = 17, height = 14, units = "cm",
       dpi = 1600, background = "white")

# PBSmapping
data(nepacLLhigh)
str(nepacLLhigh)
attr(nepacLLhigh, "zone") <- 10

nepacUTM <- convUL(nepacLLhigh)

ggplot() +
  geom_polygon(data = nepacUTM, aes(x = X, y = Y, group = PID),
               col = "black", fill = "grey75", lwd = 0.01) +
  coord_equal(xlim = c(100, 560), ylim = c(5300, 5650)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_blank())
ggsave("figures/PBSmapping-vancouver-island.PNG", width = 17, height = 14, units = "cm",
       dpi = 1600, background = "white")

# compare RNaturalEarth and PBSmapping at Bamfield
ggplot() +
  geom_sf(data = bcUTM, col = "black", fill = "grey75", lwd = 0.01) +
  scale_x_continuous(limits = c(300, 365)) +
  scale_y_continuous(limits = c(5390, 5440)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave("figures/rnaturalearth-bamfield.PNG", width = 17, height = 14, units = "cm",
       dpi = 1600, background = "white")

ggplot() +
  geom_polygon(data = nepacUTM, aes(x = X, y = Y, group = PID),
               col = "black", fill = "grey75", lwd = 0.01) +
  coord_equal(xlim = c(300, 365), ylim = c(5390, 5440)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave("figures/PBSmapping-bamfield.PNG", width = 17, height = 14, units = "cm",
       dpi = 1600, background = "white")

# Hakai data # download from link on slide 17
hakai <- readOGR("data/hakai-shapefile/COAST_TEST2.shp")
str(hakai) # WGS84

# crop to Bamfield then transform to UTM zone 10
# create Bamfield lat/long extent, which is required by crop()
bamfield <- extent(-125.8, -124.7, 48.6, 49.3)
# note that the more negative value (left extent) is first

hakai.bamfield <- crop(hakai, bamfield)

hakai.bamfield.df <- fortify(hakai.bamfield)
head(hakai.bamfield.df)

ggplot() +
  geom_polygon(data = hakai.bamfield.df, aes(x = long, y = lat, group = group),
               col = "black", fill = "grey75", lwd = 0.01) +
  coord_map()

# convert Hakai shapefile to UTM zone 10
attr(hakai.bamfield.df, "projection") <- "LL"
attr(hakai.bamfield.df, "zone") <- 10
names(hakai.bamfield.df)[1:2] <- c("X", "Y")
hakai.bamfieldUTM <- convUL(hakai.bamfield.df)

ggplot() +
  geom_polygon(data = hakai.bamfieldUTM, aes(x = X, y = Y, group = group),
               col = "black", fill = "grey75", lwd = 0.01) +
  coord_equal(xlim = c(300, 365), ylim = c(5390, 5440)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave("figures/Hakai-bamfield.PNG", width = 17, height = 14, units = "cm",
       dpi = 1600, background = "white")

ggplot() +
  geom_polygon(data = hakai.bamfieldUTM, aes(x = X, y = Y, group = group),
               col = "black", fill = "grey75", lwd = 0.05) +
  coord_equal(xlim = c(335, 354), ylim = c(5404, 5424)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      axis.title = element_blank(), axis.text = element_blank(),
      axis.ticks = element_blank())
ggsave("figures/Hakai-bamfield-zoom.PNG", width = 17, height = 17, units = "cm",
       dpi = 1600, background = "white")
