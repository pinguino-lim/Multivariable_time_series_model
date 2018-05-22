library(plyr)
require(rgeos)
require(RColorBrewer)
library(rgdal)
library(maptools)
library(SpatialTools)
#library(PBSmapping)
library(geosphere)

CRSWG84 <- CRS(" +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
setwd("~/Dropbox/data/FAO_AREAS")
fao <- readShapeSpatial("FAO_AREAS.shp",
                        proj4string = CRSWG84)
fao.union <- (unionSpatialPolygons(fao, fao$F_AREA))
zone <- row.names(fao.union)
fao.union <- SpatialPolygonsDataFrame(fao.union,
                                      data = data.frame("zone" = zone,
                                                        "area" = data.frame("area" = areaPolygon(fao.union))/10^6,
                                                        row.names=row.names(fao.union)))


library(animation)
library(leaflet)
library(animation)
library(png)
library(htmlwidgets)
library(webshot)
library( RColorBrewer)

marine_areas <- marine_areas[order(marine_areas$zone, decreasing = F),-3]


fao.union@data <- merge( fao.union@data,marine_areas[,-2], by = "zone")
fao.union@data[,3:15] <- fao.union@data[,3:15]/fao.union@data[,2]
fao.union@data <- fao.union@data[,c(-2)]


pal <- (colorBin("viridis", domain = fao.union$X2014))

df <- fao.union[,c(1,4+1)]
colnames(df@data) <- c("id","value")

# Find a center point for each region
centers <- data.frame(gCentroid(fao.union, byid = TRUE))
centers[,3:4] <- marine_areas[,1:2]
colnames(centers)[3:4] = c("zone","region")
centers[19,1] <- centers[19,1]-50
centers[17,1] <- centers[17,1]-100
centers[c(13,15),1] <- centers[c(13,15),1]+30
centers[8,2] <- centers[8,2]-10


centers$variable <- c(NA,"CHL",NA,"SST",NA,NA,"SST",NA,NA,NA,
                      "CHL","CHL",NA,"CHL,PIC:POC",NA,"SST",NA,NA,"PIC:POC")
df@data$variable <- c(NA,"CHL",NA,"SST",NA,NA,"SST",NA,NA,NA,
                      "CHL","CHL",NA,"CHL,PIC:POC",NA,"SST",NA,NA,"PIC:POC")


#devtools::install_github("statnmap/HatchedPolygons")
library(HatchedPolygons)
x.hatch_1 <- hatched.SpatialPolygons(fao.union[14,], density = c(0.6, 0.3), angle = c(45, 135))
x.hatch_2 <- hatched.SpatialPolygons(fao.union[14,], density = c(0.3, 0.6), angle = c(45, 135))

plot(x.hatch)

#RColorBrewer::display.brewer.all()
factpal <- colorFactor(brewer.pal(4,"Accent"), df$variable, na.color = "#FFFFFF")
#factpal(df$variable)

m <-leaflet(df[-14,]) %>% addProviderTiles("OpenStreetMap.BlackAndWhite") %>% addPolygons(
    fillColor = ~factpal(variable),
    weight = 2,
    smoothFactor = 0.5,
    opacity = 0.5,
    color = "grey",
    dashArray = "3",
    fillOpacity = 0.8) %>%
    addPolylines(data = x.hatch_1,
                 weight = 4,
                 smoothFactor = 0.5,
                 opacity = 1,
                 #dashArray = "3",
                 fillOpacity = 1,
               color = c( "#7FC97F"))%>%
  addPolylines(data = x.hatch_2,
               weight = 4,
               smoothFactor = 0.5,
               opacity = 1,
               #dashArray = "3",
               fillOpacity = 1,
               color = c( "#FDC086"))%>%
  addPolylines(data = fao.union[14,],
               weight = 2,
               smoothFactor = 0.5,
               opacity = 0.5,
               color = "grey",
               dashArray = "3") %>%
  #addLegend(pal = pal, values = c(0,1), labFormat = labelFormat(suffix = " Tonnes/km^2"),
  #          opacity = 1, title = sprintf('Fishery Capture rate, %s', format(2006,format='%Y')))%>%
  setView(0,0, 2)%>%
  addLabelOnlyMarkers(data=centers, ~x, ~y+5, label =  ~as.character(zone),
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))%>%
  addLabelOnlyMarkers(data=centers, ~x, ~y, label =  ~as.character(region),
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))%>%
  addLabelOnlyMarkers(data=centers, ~x, ~y-5, label =  ~as.character(variable),
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,
                                                  style = list("font-family" = "serif",
                                                               "font-style" = "italic")))



library(htmlwidgets)
setwd("~/Dropbox/TESIS/R_code/Theme 1.8")

saveWidget(m, file="map.html")

