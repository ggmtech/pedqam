#
library(ggmap)  # warning changed require  google key
#https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/ggmap/ggmapCheatsheet.pdf
#- stamen: maptype = c(“terrain”, “toner”, “watercolor”)
#- google: maptype = c(“roadmap”, “terrain”, “satellite”, “hybrid”)
#- osm: open street map
#- cloudmade: 1000s of maps, but an api key from http://cloudmade.com
# zoom = integer from 3-21, 3 = continent, 10=city, 21=building (openstreetmap limit of 18)
# if fails , get map by
# get_googlemap, get_openstreetmap, get_stamenmap, get_cloudmademap

myMap <- get_map(location="New Delhi", 
                 source="google",  
                 maptype = "roadmap",
                 #color = "bw",
                 zoom = 16,
                 crop=FALSE         ) 

ggmap( myMap, extent = "normal") + theme_minimal() + labs(x="", y ="")  #  yes ok

# add layers
# ggmap(myMap)+
#  geom_point(aes(x = Longitude, y = Latitude), data = data,
#             alpha = .5, color="darkred", size = 3)

#Controlling size and color
# scale_size(range = c(3, 20))  # but by area better
# scale_area(range=c(3,20))
# Continuous variables: color gradient between n colors, e.g.:
#  scale_colour_gradientn(colours = rainbow_hcl(7))
# Discrete variables, e.g.:  color,  size
# scale_colour_manual(values=rainbow_hcl(7)) scale_colour_manual(values=c("8" = "red", "4" = "blue","6" = "green“)
#        *Use colorspace and RColorBrewer to choose color combinations


#Add polygons from shp file
#The shp file is imported into R using the rgdal package, and must be transformed to geographic coordinates (latitude/longitude) on the World Geodetic System of 1984 (WGS84) datum using the rgdal package:
# library(rgdal)
# shpData <- readOGR(dsn="C:\\Documents and Settings\\Watershed", layer="WS") proj4string(shpData) # describes data’s current coordinate reference system
# to change to correct projection:
# shpData <- spTransform(shpData,   CRS("+proj=longlat +datum=WGS84"))
#To plot the data:
#  geom_polygon(aes(x = long, y = lat, group=id), data = shpData, color ="white", fill ="orangered4", alpha = .4, size = .2)
#    color= outline color fill = polygon color , alpha = transparency size = outline size


# Annotate figure
ndelhi <- get_map('New Delhi', zoom = 14, maptype = 'satellite') 
ndelhi
mylon = 77.2
mylat = 28.615
myrange = 0.3
ggmap(ndelhi) +
  annotate('rect', xmin = mylon - 01.25*myrange, 
                   ymin = mylat - 01.25*myrange, 
                   xmax = mylon + 01.25*myrange, 
                   ymax = mylon + 01.25*myrange,  
                   col="red", fill="red", alpha = 0.5) 
 # annotate('text', x= mylon, y=mylat, label = 'Rail Bhawan',   colour = "red", size = 4 ) +  
  annotate('segment', x = mylon,  y = mylat, label = 'new point',
                   yend = mylat + 0.25*myrange, 
                   xend = mylon + 0.25*myrange,
                   arrow = arrow(length=unit(0.3,"cm")), 
                   col="red", fill="red", size = 5.5   ) +
  labs(x = 'Longitude', y = 'Latitude') + 
  ggtitle('New Delhi')

#####

library(leaflet)
library(readr)
point_data <- read_csv("http://environmentalcomputing.net/wp-content/uploads/2018/03/point_data.csv")

leaflet(point_data) %>% 
  fitBounds(lng1 = min(point_data$lon) - 0.11, 
            lat1 = min(point_data$lat) - 0.11,
            lng2 = max(point_data$lon) + 0.11, 
            lat2 = max(point_data$lat) + 0.11) %>% 
  addCircleMarkers(~lon, ~lat,
                   radius = 3,
                   opacity = 100,
                   color = "white", 
                   label = as.character(point_data$Site), 
                   labelOptions = labelOptions(noHide = TRUE, 
                                               textOnly = TRUE, 
                                               style = list(color = "white"),
                                               direction = "auto",
                                               offset = c(0, -10))) %>% 
  addScaleBar(options = list(imperial = FALSE)) %>%
  addProviderTiles(providers$Esri.WorldImagery)



###################


# wine scrap
# https://www.r-bloggers.com/visualising-italian-wine-production/


library(tidyverse)
library(ggplot2)
library(sf)
library(mapview)
library(rvest)
library(magrittr)

# webscraping

wine <- read_html("https://italianwinecentral.com/wine-production-in-italy-by-region/")

wine %<>%     html_table() %>%    extract2(1) %>%    slice(-21)

wine[,-1] %<>% map_df(parse_number)

head(wine)


# To import the spatial data I used the sf package
#  st_read function to read  shape file  includes a column geometry. 

italy <- st_read( "./Limiti_2016_ED50_g/Reg2016_ED50_g/Reg2016_ED50_g.shp")  #??

head(italy)

nc = st_read( system.file("shape/nc.shp", package="sf") )

wineLong <- gather(wine, Year, Quantity, -Region)

fullWineLong <- full_join(italy, wineLong, by = c("REGIONE" = "Region"))

ggplot(data = fullWineLong, aes(fill = Quantity)) + 
    geom_sf() + 
    facet_wrap(vars(Year)) +
    scale_fill_viridis_c()





##################

#List of Countries
ICC_WC_T20 <- c("Australia",
                  "WestIndies",
                  "India",
                  "SriLanka",
                  "Pakistan",
                  "Bangladesh",
                  "NewZealand",
                  "SouthAfrica",
                  "England",
                  "HongKong",
                  "Zimbabwe",
                  "Afghanistan",
                  "Scotland",
                  "Netherlands",
                  "Ireland",
                  "Oman")

#extract geo location of these countries
countries <- ggmap::geocode(ICC_WC_T20)
countries

#map longitude and latitude in separate variables
nation.x <- countries$lon
nation.y <- countries$lat

#using ggplot
#plot the world map
mapWorld <- borders("world", colour="grey", fill="lightblue")

#add data points to the world map
ggplot() + mapWorld + geom_point(aes(x=nation.x, y=nation.y) ,color="red", size=8, alpha=0.3)

