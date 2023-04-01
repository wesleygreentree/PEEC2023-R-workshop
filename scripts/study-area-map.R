# Simple study area map, then extend with extra data

# load packages
library(ggplot2) # for ggplot
library(PBSmapping) # for basemap, convert projections
library(rgdal)
library(grid)

# load basemap
data(nepacLLhigh) 
# basemap from PBSmapping, northeast Pacific, lat/long, high resolution

# convert basemap to UTM zone 10
attr(nepacLLhigh, "zone") <- 10
str(nepacLLhigh) # see projection = "LL", zone = 10

nepacUTM <- convUL(nepacLLhigh) # converts lat/long to UTM zone 10
str(nepacUTM)

# look at nepacUTM
head(nepacUTM)

# make dataframe of points and convert to UTM zone 10
points <- data.frame(site = c("Race Rocks Lighthouse", "Smith Island",
                              "Protection Island"),
                     X = c(-123.5314, -122.8420, -122.9297),
                     Y = c(48.29803, 48.3190, 48.1273))
points
attr(points, "zone") <- 10
attr(points, "projection") <- "LL"
pointsUTM <- convUL(points)
str(pointsUTM)

simple.haro.strait <- ggplot() +
  geom_polygon(data = nepacUTM, aes(x = X, y = Y, group = PID), 
               fill = "grey85", col = "black", lwd = 0.1) +
  coord_equal(xlim = c(430, 540), ylim = c(5330, 5402.5)) +
  
  geom_point(data = pointsUTM, aes(x = X, y = Y), col = "black") +
  
  annotate("text", x = 460.6, y = 5346.0, label = "Race Rocks\nLighthouse", 
           size = 3, lineheight = 0.75) +
  annotate("text", x = 511.7, y = 5354.8, label = "Smith\nIsland, WA", 
           size = 3, lineheight = 0.75) +
  annotate("text", x = 504, y = 5333.7, label = "Protection\nIsland, WA", 
           size = 3, lineheight = 0.75) +
  
  annotate("text", x = 474.3, y = 5366, label = "Victoria", size = 3) +
  
  # north arrow
  geom_segment(arrow = arrow(length = unit(0.2, "cm")),
               aes(x = 542, xend = 542, y = 5398.3, yend = 5402.3), size = 0.7) +
  annotate("text", x = 542, y = 5404, label = "N",
           fontface = "bold", size= 3.5) +
  
  # scalebar
  annotate("rect", xmin = 427, xmax = 447, ymin = 5329, ymax = 5330.8, 
           col = "black", fill = "black", size = 0.2) +
  annotate("rect", xmin = 437, xmax = 447, ymin = 5329, ymax = 5330.8, 
           col = "black", fill = "white", size = 0.2) +
  
  annotate("text", x = 427, y = 5332, label = "0", size = 3) +
  annotate("text", x = 437, y = 5332, label = "10", size = 3) +
  annotate("text", x = 447, y = 5332, label = "20", size = 3) +
  annotate("text", x = 437, y = 5327.9, label = "kilometres", size = 3) +
  
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# make viewport
ne.pacific <- ggplot() +
  geom_polygon(data = nepacUTM, aes(x = X, y = Y, group = PID),
               fill = "grey85", col = "grey25", lwd = 0.0005) +
  coord_equal(xlim = c(-300, 600), ylim = c(4900, 6300)) +
  annotate("rect", xmin = 430, xmax = 540, ymin = 5330, ymax = 5402.5, 
           size = 0.6, fill = NA, col = "black") +
  annotate("text", x = 80, y = 5350, label = "Pacific Ocean", 
           size = 2.5, fontface = "italic") +
  
  annotate("text", x = 540, y = 5150, label = "USA", 
           size = 2.5, fontface = 2) +
  annotate("text", x = 460, y = 5950, label = "Canada", 
           size = 2.5, fontface = 2) +
  annotate("text", x = 420, y = 5700, label = "British\nColumbia",
           size = 2.5, lineheight = 0.75) +
  
  theme_bw() +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_blank())
ne.pacific

tiff("figures/study-area-w-viewport.tiff", res = 1600, 
     width = 17, height = 12, units = "cm", bg = "white",
     compression = "lzw")
inset.viewport <- viewport(width = 0.5, height = 0.5,
                           x = -0.1316, y = 0.9734, just = c("left", "top"))
print(simple.haro.strait)
print(ne.pacific, vp = inset.viewport)
dev.off()

# read in RCA shapefile
rca <- readOGR("data/rca-shapefile", "rockfish_102001")
str(rca) # canada albers equal area conic

# convert to lat/long
rca.wgs <- spTransform(rca, CRS(SRS_string = "EPSG:4326"))

rca.wgs.coords <- lapply(rca.wgs@polygons, 
                         function(p) data.frame(p@Polygons[[1]]@coords))

rca.wgs@data$NAME

# for loop to pull list of polygon geometry into XY format for PBSmapping

# empty dataframe to add each polygon's XY points into
df <- as.data.frame(matrix(nrow = 0, ncol = 3))

for (i in 1:length(rca.wgs@data$NAME)) {
  sub <- rca.wgs.coords[[i]] # subset by RCA
  sub$group <- paste(rca.wgs@data$NAME[i], i, sep = "-") 
  # add i to end because some RCAs with same name
  df <- rbind(df, sub)
}

sort(unique(df$group))

names(df) <- c("X", "Y", "RCA")
attr(df, "zone") <- 10
attr(df, "projection") <- "LL"

# convert RCA XY df from WGS 84 to UTM zone 10
rcaUTM <- convUL(df)

haro.strait <- ggplot() +
  geom_polygon(data = rcaUTM, aes(x = X, y = Y, group = RCA),
               fill = "firebrick1") + # put underneath basemap, as there can be slight mismatches
  geom_polygon(data = nepacUTM, aes(x = X, y = Y, group = PID), 
               fill = "grey85", col = "black", lwd = 0.1) +
  coord_equal(xlim = c(430, 540), ylim = c(5330, 5402.5)) +
  
  geom_point(data = pointsUTM, aes(x = X, y = Y), col = "black") +
  
  annotate("text", x = 460.6, y = 5346.0, label = "Race Rocks\nLighthouse", 
           size = 3, lineheight = 0.75) +
  annotate("text", x = 511.7, y = 5354.8, label = "Smith\nIsland, WA", 
           size = 3, lineheight = 0.75) +
  annotate("text", x = 504, y = 5333.7, label = "Protection\nIsland, WA", 
           size = 3, lineheight = 0.75) +
  
  annotate("text", x = 474.3, y = 5366, label = "Victoria", size = 3) +

  # should plot out a 100 km foraging ambit buffer around Smith and Protection!! (great idea)
  # label Haro Strait with a straight line
  
  # RCA legend
  annotate("rect", xmin = 427, xmax = 430.5, fill = "firebrick1",
           ymin = 5337, ymax = 5340) +
  annotate("text", x = 445, y = 5338.5, 
           label = "Rockfish Conservation Area", size = 3) +
  
  # north arrow
  geom_segment(arrow = arrow(length = unit(0.2, "cm")),
               aes(x = 542, xend = 542, y = 5398.3, yend = 5402.3), size = 0.7) +
  annotate("text", x = 542, y = 5404, label = "N",
           fontface = "bold", size= 3.5) +
  
  # scalebar
  annotate("rect", xmin = 427, xmax = 447, ymin = 5329, ymax = 5330.8, 
           col = "black", fill = "black", size = 0.2) +
  annotate("rect", xmin = 437, xmax = 447, ymin = 5329, ymax = 5330.8, 
           col = "black", fill = "white", size = 0.2) +
  
  annotate("text", x = 427, y = 5332, label = "0", size = 3) +
  annotate("text", x = 437, y = 5332, label = "10", size = 3) +
  annotate("text", x = 447, y = 5332, label = "20", size = 3) +
  annotate("text", x = 437, y = 5327.9, label = "kilometres", size = 3) +

  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
haro.strait

# save # need to save in a different way than ggsave(), to add in the viewport
tiff("figures/study-area-rca.tiff", res = 1600, 
     width = 17, height = 12, units = "cm", bg = "white",
     compression = "lzw")
inset.viewport <- viewport(width = 0.5, height = 0.5,
                           x = -0.1316, y = 0.9734, just = c("left", "top"))
print(haro.strait)
print(ne.pacific, vp = inset.viewport)
dev.off()
