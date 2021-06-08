packages <- c("tidyverse", , "readxl", "lubridate",
              "broom", "coefplot", "cowplot", "drat",
              "ggfortify", "DT", "knitr",
              "ggpubr","scales", "GGally", "ggrepel", "ggridges",
              "viridis", "viridisLite", "devtools", "ggforce", "ggraph", 
              "sf",
              "summarytools",
              "graphlayouts", "gridExtra", "here", "interplot", "margins", 
              "maps", "mapproj", "mapdata", "MASS", "naniar", "prismatic", 
              #"quantreg", "rlang", "scales", "socviz", "survey", "srvyr", 
              #"ggplot2", "dplyr", "tidyr", "magrittr", "stringr","forcats","reshape2",
              #"gapminder",
              # "pwr", "psy", "car", "doBy", 
              #"imputeMissings", "RcmdrMisc", "questionr", "vcd", "multcomp", 
              #"KappaGUI", "rcompanion", "FactoMineR", "factoextra", "corrplot", 
              #"ltm", "goeveg", "corrplot", "FSA", "MASS",  "nlme", 
              #"psych", "ordinal", "lmtest",  "dslabs",  
              #"assist", "ggstatsplot",  "styler", "remedy", 
              #"snakecaser", "addinslist", "esquisse", "here",  
              #"funModeling", "pander", "cluster"
              )
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) { install.packages(packages[!installed_packages]) }
# Packages loading
lapply(packages, library, character.only = TRUE) #%>% invisible()

my_packages <- c("tidyverse", "broom", "coefplot", "cowplot", "drat",
                 "gapminder", "GGally", "ggrepel", "ggridges",  
                 "graphlayouts", "gridExtra", "here", "interplot", "margins", 
                 "maps", "mapproj", "mapdata", "MASS", "naniar", "prismatic", 
                 "quantreg", "rlang", "scales", "socviz", "survey", "srvyr", 
                 "viridis", "viridisLite", "devtools", "ggforce", "ggraph", "sf")

# install.packages(my_packages, repos = "http://cran.rstudio.com")

# spatial
install.packages(c("sf", "tmap", "mapview", "raster", "dplyr"))
install.packages(c("tidycensus", "FedData", "rnaturalearth", "osmdata", "OpenStreetMap", "shinyjs", "rmapshaper"))
# Some packages may require {rJava} which is not always straightforward to install: install.packages("rJava")

#If You want, visualize missings with naniar package
library(naniar)
gg_miss_var(task$data(), show_pct = TRUE)

#########
# Installing spatial R packages on Ubuntu Robin Lovelace 30 March 2020
# installing gis in ubuntu

# sudo add-apt-repository ppa:marutter/rrutter3.5 # add a repository that ships the latest version of R:
# sudo apt update  # update the repositories so the software can be found:
# sudo apt install libudunits2-dev libgdal-dev libgeos-dev libproj-dev libfontconfig1-dev # install system dependencies:
# sudo apt install r-base-dev r-cran-sf r-cran-raster r-cran-rjava # binary versions of key R packages:

library(sf)
#> Linking to GEOS 3.7.2, GDAL 2.4.2, PROJ 5.2.0
install.packages("tmap")
update.packages()

#sudo apt-get update # see if things have changed
# sudo apt upgrade # install changes
# sudo docker run -it robinlovelace/geocompr:ubuntu-bionic
R
library(sf)
#> Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 7.0.0

##############


# The script below checks, if the package exists, and if not, then it installs it and finally it loads it to R
mypackages<-c("readxl", "dplyr", "multcomp")
for (p in mypackages){
    if(!require(p, character.only = TRUE)){
        install.packages(p)
        library(p, character.only = TRUE)
    }
}

# better way
# Package names
packages <- c("ggplot2", "readxl", "dplyr", "tidyr", "ggfortify", "DT", "reshape2", "knitr", "lubridate", "pwr", "psy", "car", "doBy", "imputeMissings", "RcmdrMisc", "questionr", "vcd", "multcomp", "KappaGUI", "rcompanion", "FactoMineR", "factoextra", "corrplot", "ltm", "goeveg", "corrplot", "FSA", "MASS", "scales", "nlme", "psych", "ordinal", "lmtest", "ggpubr", "dslabs", "stringr", "assist", "ggstatsplot", "forcats", "styler", "remedy", "snakecaser", "addinslist", "esquisse", "here", "summarytools", "magrittr", "tidyverse", "funModeling", "pander", "cluster")
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) { install.packages(packages[!installed_packages]) }
lapply(packages, library, character.only = TRUE) %>%  invisible()

#

library(osmdata)
library(sf)
vancouver_highways <- opq(bbox = 'Vancouver Canada') %>%
                      add_osm_feature(key = 'highway') %>%
                      osmdata_sf()
vancouver_highways
plot(vancouver_highways$osm_lines$geometry)
head(vancouver_highways$osm_lines$name)
which(vancouver_highways$osm_lines$name == "Alma St") # failed
which(vancouver_highways$osm_lines$name == "Alma Street")

row_index <- agrep("Alma St", vancouver_highways$osm_lines$name)
plot(vancouver_highways$osm_lines$geometry[row_index])



#############
library(tidyverse)
library(sf)
library(osmdata)
rnaturalearth::ne_countries( scale = "medium", returnclass = "sf") %>%  
               select(name, continent, geometry) %>%  
               filter(name == 'United Kingdom') ->  
               uk  
uk
ggplot() +  ggplot2::geom_sf(data = uk,       fill = "linen") +
geom_point(    aes(x = c(0, -4), y = c(52, 57)  ),
               size = 10,
               fill = 'green',
               alpha = .6,
               color = "black",
               stroke = 1,  # larger outline width
               shape = 21  # point with outline and fill
           ) +  
    scale_size(range = c(.5,15), guide = F) +  
    scale_fill_viridis_c(begin = .2, end = 1) +  
    labs(fill = "Empire State Burger\nCost (pounds)") +  
    theme_bw()



rnaturalearth::ne_countries(  
    scale = "medium", returnclass = "sf") ->  
    ne_countries  

ggplot(data = ne_countries) +  
    ggplot2::geom_sf(data = ne_countries) +  
    aes(fill = pop_est) +  
    scale_fill_viridis_c(option = "magma") +  
    labs(title = "Defense Expenditure by Share of GDP (%)",  
         subtitle = "NATO Member States",  
         fill = "Share of GDP (%)"
         ) +  
    coord_sf(crs = "+proj=laea + y_0=10 + lon_0 = 77 +lat_0=-0 +ellps=WGS84 +no_defs") +
    theme_bw() +  
    theme(plot.title    =  element_text(hjust = 0.5)) +  
    theme(plot.subtitle =  element_text(hjust = 0.5)) 
#"+proj=moll"
# %%%%%%%%%%%%%%%%%
# https://www.r-bloggers.com/2020/10/personal-art-map-with-r/
library(osmdata)

bbx <- getbb("Boston, MA")
# or more spefic bb
min_lon <- -71.28735; max_lon <- -70.900578
min_lat <- 42.245838; max_lat <- 42.453673
bbx <- rbind(x=c(min_lon,max_lon),y=c(min_lat,max_lat))
colnames(bbx) <- c("min","max")


bbx <- getbb("Lucknow, UP")
bbx <- getbb("New Delhi, India")
bbx <- getbb("India")
bbx
highways <- bbx %>%
    opq()%>%
    add_osm_feature(key = "highway", 
                    value=c("motorway", "trunk",
                            "primary","secondary", 
                            "tertiary","motorway_link",
                            "trunk_link","primary_link",
                            "secondary_link",
                            "tertiary_link")) %>%
                            osmdata_sf()
railways <- bbx %>% opq()%>%  add_osm_feature(key = "railway", 
                    value=c("nerrow gauge")) %>%   
            osmdata_sf()

options(width = 60)

require(sf)
ggplot() +  geom_sf(data = highways$osm_lines, aes(color=highway), size = .4, alpha = .65) +
            theme_void()

ggplot() +  geom_sf(data = railways$osm_lines, aes(color=railways), size = .4, alpha = .65)+ 
            theme_void()


streets <- bbx %>%  opq() %>%
                    add_osm_feature(key = "highway", 
                    value = c("residential", "living_street",
                              "service","unclassified",
                              "pedestrian", "footway",
                              "track","path")) %>%
                    osmdata_sf()

ggplot() +  geom_sf(data = streets$osm_lines,
            aes(color=highway), size = .4, alpha = .65) + theme_void()

# let’s crop the streets to show only those within the bounding box.

color_roads <- rgb(0.42,0.449,0.488)
ggplot() +  geom_sf(data = streets$osm_lines, col = color_roads, size = .4, alpha = .65) +
           geom_sf(data = highways$osm_lines, col = color_roads, size = .6, alpha = .8 ) +
           #coord_sf(xlim = c(min_lon,max_lon), ylim = c(min_lat,max_lat), expand = FALSE)+
           theme(legend.position = F) + theme_void()





#### Read https://www.r-bloggers.com/2020/11/r-as-gis-part-1-vector/

library(tidyverse)
library(sf)
getwd()
target_dir <- "./tresh" # define a target directory in your laptop
setwd(target_dir) # put this as the working directory
download.file("https://downloadagiv.blob.core.windows.net/referentiebestand-gemeenten/VoorlopigRefBestandGemeentegrenzen_2016-01-29/Voorlopig_referentiebestand_gemeentegrenzen_toestand_29_01_2016_GewVLA_Shape.zip", destfile = "municipality.zip")
unzip(zipfile = "municipality.zip")
province <- st_read("Shapefile/Refprv.shp") # read in the Flemish province shapefile
province
?st_drop_geometry
province %>%
    rename(name = NAAM) %>%
    mutate(area = OPPERVL / 1e6) %>%
    select(name, area) %>%
    arrange(desc(area)) -> province
st_crs(province)

# re-project into the European CRS
province <- st_transform(province, 3035)

# sf objects can be, very convinently, plotted using ggplot2:
# a simple plot of the province # colored by their surface
ggplot() +
    geom_sf(data = province, aes(fill = area)) +
    geom_sf_label(data = province, aes(label = name)) +
    scale_fill_continuous(type = "viridis",  name = "Area [km²]") +
    labs(title = "Flemish provinces", x = "",  y = "") +
    theme(panel.background = element_blank(), 
          axis.text        = element_blank(),
          axis.ticks       = element_blank() )


# load the municipality shapefile
municipality <- st_read("Shapefile/Refgem.shp")

municipality <- st_transform(municipality,   st_crs(province))
# make a buffer of 5000m around the city of Antwerpen
ant_buffer <- st_buffer(municipality[municipality$NAAM == "Antwerpen",], dist = 5000)
# get the centroids of the municipalities
municipality_cent <- st_centroid(municipality)
## Warning in st_centroid.sf(municipality): st_centroid assumes attributes are
## constant over geometries of x
# plot the different objects
ggplot() +
    geom_sf(data = municipality) +
    geom_sf(data = ant_buffer, alpha = 0.5, fill = "orange") +
    geom_sf(data = municipality_cent, color = "green")

# city centroids are within the 5km buffer of the city of Antwerpen
st_within(municipality_cent, ant_buffer)
st_within(municipality_cent, ant_buffer, sparse = FALSE)[1:10,]

municipality %>%
    filter(st_within(municipality_cent, ant_buffer, sparse = FALSE))
#### Read https://www.r-bloggers.com/2020/11/r-as-gis-part-1-vector/

municipality %>%
    st_intersection(., st_geometry(ant_buffer)) %>%
    mutate(area = st_area(.)) %>%
    mutate(prop_area = as.numeric(area / OPPERVL)) %>%
    arrange(prop_area) -> municipality_inter

municipality_inter
# We can check the returned geometries:

ggplot() +
    geom_sf(data = subset(municipality, NAAM %in% municipality_inter$NAAM)) +
    geom_sf(data = municipality_inter, aes(fill = prop_area), alpha = 0.3)
# oins based on attributes adding population data to the municipality dataset:

# load population file
population <- read.csv("https://raw.githubusercontent.com/lionel68/lionel68.github.io/master/_posts_data/r_as_gis/population_flanders.csv")
# join to the municipality
municipality %>%     left_join(population) -> municipality_pop
## Joining, by = "NAAM"

# slightly adapt names in province
names(province)[1:2] <- paste(names(province)[1:2], "province", sep="_")
# slightly adapt names province
names(municipality_pop)[5] <- "name_municipality"
# joins, using municipality within provinces
municipality_province <- st_join(municipality_pop[,c(5, 10)], 
                                 province, join = st_within)
# plot the results
ggplot() +
    geom_sf(data = municipality_province, 
            aes(fill = name_province)) +
    geom_sf(data = province, color = "red",
            alpha = 0.2)

#inhabitants of Flanders are within 10km of Seveso sites, sites where dangerous (chemical) substances are stored

# load seveso data
seveso <- st_read("https://raw.githubusercontent.com/lionel68/lionel68.github.io/master/_posts_data/r_as_gis/seveso.geojson")
## Reading layer `seveso' from data source `https://raw.githubusercontent.com/lionel68/lionel68.github.io/master/_posts_data/r_as_gis/seveso.geojson' using driver `GeoJSON'
## Simple feature collection with 291 features and 2 fields
## geometry type:  MULTIPOLYGON
## dimension:      XY
## bbox:           xmin: 3809095 ymin: 3082887 xmax: 4012066 ymax: 3164945
## projected CRS:  ETRS89-extended / LAEA Europe
# create 10km buffers
seveso_buffer <- st_buffer(seveso, 10000)
# create intersection with municipality
municipality_seveso <- st_intersection(municipality_pop, seveso_buffer)create 10km buffers
## Warning: attribute variables are assumed to be spatially constant
## throughout all geometries
# compute areas and population within the intersection
# and compute sums
municipality_seveso %>%
    mutate(area_intersection = as.numeric(st_area(.))) %>%
    mutate(pop_intersection = (area_intersection /
                                   OPPERVL) * population) %>%
    st_drop_geometry() %>% # we drop the geometry column
    summarise(S_seveso = sum(pop_intersection, na.rm = TRUE),
              S_population = sum(population, na.rm = TRUE)) %>%
    mutate(prop = S_seveso / S_population)
##   S_seveso S_population      prop
## 1 70455290    136289905 0.5169516


################################### tidygeocoder

#https://www.r-bloggers.com/2021/04/tidygeocoder-1-0-3/


library(tidyverse, warn.conflicts = FALSE)
library(tidygeocoder)
library(knitr)
library(leaflet)
library(glue)
library(htmltools)

num_coords <- 25 # number of coordinates
set.seed(103) # for reproducibility

# latitude and longitude bounds
lat_limits <- c(40.40857, 40.42585)
long_limits <- c(-3.72472, -3.66983)

# randomly sample latitudes and longitude values
random_lats  <- runif( num_coords,   min = lat_limits[1],  max = lat_limits[2] )
random_longs <- runif( num_coords,   min = long_limits[1], max = long_limits[2] )

# Reverse geocode the coordinates
# the speed of the query is limited to 1 coordinate per second to comply
# with Nominatim's usage policies
madrid <- reverse_geo( 
  lat = random_lats, random_longs, 
  method = 'osm', full_results = TRUE,
  custom_query = list(extratags = 1, addressdetails = 1, namedetails = 1)
)



# Create html labels
# https://rstudio.github.io/leaflet/popups.html
madrid_labelled <- madrid %>%
  transmute(
    lat, 
    long, 
    label = str_c(
      ifelse(is.na(name), "", glue("<b>Name</b>: {name}<br>")),
      ifelse(is.na(suburb), "", glue("<b>Suburb</b>: {suburb}<br>")),
      ifelse(is.na(quarter), "", glue("<b>Quarter</b>: {quarter}")),
      sep = ''
    ) %>% lapply(htmltools::HTML)
  )

# Make the leaflet map
madrid_labelled %>% 
  leaflet(width = "100%", options = leafletOptions(attributionControl = FALSE)) %>%
  setView(lng = mean(madrid$long), lat = mean(madrid$lat), zoom = 14) %>%
  # Map Backgrounds
  # https://leaflet-extras.github.io/leaflet-providers/preview/
  addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
  addProviderTiles(providers$OpenRailwayMap, group = "Rail") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addTiles(group = "OSM") %>%
  # Add Markers
  addMarkers(
    labelOptions = labelOptions(noHide = F), lng = ~long, lat = ~lat,
    label = ~label,
    group = "Random Locations"
  ) %>%
  # Map Control Options
  addLayersControl(
    baseGroups = c("OSM", "Terrain", "Satellite", "Rail"),
    overlayGroups = c("Random Locations"),
    options = layersControlOptions(collapsed = TRUE)
  )




#limits
tie_addresses <- tibble::tribble(
  ~res_street_address, ~res_city_desc, ~state_cd, ~zip_code,
  "624 W DAVIS ST   #1D",   "BURLINGTON", "NC",  27215,
  "201 E CENTER ST   #268", "MEBANE",     "NC",  27302,
  "7833  WOLFE LN",         "SNOW CAMP",  "NC",  27349,
)

tg_batch <- tie_addresses %>%
  geocode(
    street = res_street_address,
    city = res_city_desc,
    state = state_cd,
    postalcode = zip_code,
    method = 'census', 
    full_results = TRUE
  )

tg_single <- tie_addresses %>%
  geocode(
    street = res_street_address,
    city = res_city_desc,
    state = state_cd,
    postalcode = zip_code,
    limit = 100,
    return_input = FALSE,
    method = 'census', 
    mode = 'single',
    full_results = TRUE
  )

tg_single

paris <- geo('Paris', method = 'opencage', full_results = TRUE, limit = 10)


usethis::edit_r_environ()











# %%%%%%%%%%%%%%%%%%
library(shiny)
library(RColorBrewer)
library(dplyr)
# library(rgdal)
library(leaflet)
library(sf)
library(rmapshaper)
#install.packages('rmapshaper')
??rmapshaper

dataset <- read.csv(file = "dataset_res.csv", stringsAsFactors = FALSE, encoding = 'latin1') %>%
    select(Codigo_IBGE, Sigla_Estado, cidade, longitude, latitude, Data, Total_Exames, Total_positivos, Indice_Positividade)
dataset$Data <- as.Date(dataset$Data)
dataset$Codigo_IBGE <- as.character(dataset$Codigo_IBGE)

# for one day first
dataset1 <- dataset %>% 
    filter(Data == as.Date('2020-03-10'))

data_uf <- st_read('br_unidades_da_federacao', layer = 'BR_UF_2019')
# plot(data_uf)

# Summarise by UF
dataset1 <- dataset1 %>% 
    group_by(Sigla_Estado) %>% 
    summarise(Total_positivos = sum(Total_positivos, na.rm = TRUE))

data_uf <- data_uf %>% ms_simplify(keep = 0.1)
#plot(data_uf)

casos <- data_uf %>% left_join(dataset1, by = c('SIGLA_UF' = 'Sigla_Estado')) %>% 
    mutate(popup = paste0(SIGLA_UF," -", NM_UF, " : ", Total_positivos))

# plot(casos)

pal_fun <- colorNumeric("YlOrRd", domain = as.integer(1:max(casos$Total_positivos, na.rm = TRUE)),na.color = NA)

casos <- sf::st_transform(casos,sp::CRS('+proj=longlat +datum=WGS84'))

leaflet(casos) %>% 
    addTiles() %>% 
    addPolygons(color = 'black', weight = 1, fillColor = ~pal_fun(Total_positivos), 
                fillOpacity = 0.8, smoothFactor = 0.5, label = ~ popup) %>% 
    addLegend("bottomleft", pal = pal_fun, values = ~Total_positivos, 
              labFormat = prettyNum,
              title = 'Total Positivos de Exames por UF')


##########


#install.packages("sf")
#install.packages("raster")
#install.packages("spData")
#devtools::install_github("Nowosad/spDataLarge")
library(dplyr)
library(stringr) # for working with strings (pattern matching)


library(sf)          # classes and functions for vector data
library(raster)      # classes and functions for raster data

library(sp)
library(spData)        # load geographic data
library(spDataLarge)   # load larger geographic data
world

plot(world)
#devtools::install_github("geocompr/geocompkg")

world_sp = as(world, Class = "Spatial")
world_sf = sf::st_as_sf(world_sp)

# Simple features  open standard  by the Open Geospatial Consortium (OGC),

# use the world dataset provided by the spData
names(world)
world$geom

plot(world)

world_mini = world[1:2, 1:3]
world_mini

plot(world[3:6])
plot(world["pop"])

# Plots are added as layers to existing images by setting add = TRUE
world_asia = world[world$continent == "Asia", ]
asia = st_union(world_asia)

plot(world["pop"], reset = FALSE)
plot(asia, add = TRUE, col = "red")
# easy way to verify the geographic correspondence between layers


plot(world["continent"], reset = FALSE)
cex = sqrt(world$pop) / 10000
world_cents = st_centroid(world, of_largest = TRUE)
plot(st_geometry(world_cents), add = TRUE, cex = cex)

#data()

world

st_point(c(5, 2))                 # XY point
#> POINT (5 2)
st_point(c(5, 2, 3))              # XYZ point
#> POINT Z (5 2 3)
st_point(c(5, 2, 1), dim = "XYM") # XYM point
#> POINT M (5 2 1)
st_point(c(5, 2, 3, 1))           # XYZM point
#> POINT ZM (5 2 3 1)





st_point(c(5, 2))                 # XY point
#> POINT (5 2)
st_point(c(5, 2, 3))              # XYZ point
#> POINT Z (5 2 3)
st_point(c(5, 2, 1), dim = "XYM") # XYM point
#> POINT M (5 2 1)
st_point(c(5, 2, 3, 1))           # XYZM point
#> POINT ZM (5 2 3 1)



# the rbind function simplifies the creation of matrices
## MULTIPOINT
multipoint_matrix = rbind(c(5, 2), c(1, 3), c(3, 4), c(3, 2))
st_multipoint(multipoint_matrix)
#> MULTIPOINT (5 2, 1 3, 3 4, 3 2)
## LINESTRING
linestring_matrix = rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2))
st_linestring(linestring_matrix)
#> LINESTRING (1 5, 4 4, 4 1, 2 2, 3 2)

# One sfg object contains only a single simple feature geometry. 
# A simple feature geometry column (sfc) is a list of sfg objects, which is additionally able to contain information about the coordinate reference system in use


# Craate sf object
# First, the coordinates were used to create the simple feature geometry (sfg). 
# Second, the geometry was converted into a simple feature geometry column (sfc), with a CRS. 
# Third, attributes were stored in a data.frame, which was combined with the sfc object with st_sf(). T
# his results in an sf object,
lnd_point = st_point(c(0.1, 51.5))                 # sfg object
lnd_geom = st_sfc(lnd_point, crs = 4326)           # sfc object
lnd_attrib = data.frame(                           # data.frame object
  name = "London",
  temperature = 25,
  date = as.Date("2017-06-21")
)
lnd_sf = st_sf(lnd_attrib, geometry = lnd_geom)    # sf object



# . Simple features are simply data frames (square tables), but 
# with spatial attributes stored in a list column, usually called geometry

# geographic raster data model usually consists of a raster header and 
# a matrix (with rows and columns)
# The raster header defines the coordinate reference system, the extent and the origin.

# tmap, mapview and leaflet to create interactive maps of raster and vector objects 

# ResterBrick  # single multispectral satellite file or a single multilayer object 
multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge")
r_brick = brick(multi_raster_file)

r_brick

# A RasterStack is similar to a RasterBrick. faster
# However RasterStack allows you to connect several raster objects stored in different files or multiple objects in memory. 
# More specifically, a RasterStack is a list of RasterLayer objects with the same extent and resolution
# create it is with the help of spatial objects already existing in R’s global environment

r_stack = stack(raster_in_memory, raster_on_disk)
r_stack

# crs

# Geographic coordinate systems
# ellipsoidal models with two parameters: the equatorial radius and the polar radius used.
#  suitable  Earth is compressed:  equatorial radius is around 11.5 km longer than the polar radius 
# Ellipsoids are part of a wider component of CRSs: the datum

# for precise relationship between the Cartesian coordinates and location on the Earth’s surface. 
# stored in the towgs84 argument of proj4string 
# These allow local variations in Earth’s surface, for example due to large mountain ranges,
# There are two types of datum — local and geocentric

# two types of datum — local and geocentric. 
# local datum such as NAD83 the ellipsoidal surface is shifted to align with the surface at a particular location. 
# geocentric datum such as WGS84 the center is the Earth’s center of gravity and t
# and the accuracy of projections is not optimized for a specific location. 
# Available datum definitions can be seen by executing 
st_proj_info(type = "datum")

# Projected coordinate reference systems ( vs Geographic coordinate systems )

#Projected CRSs are based on Cartesian coordinates on an implicitly flat surface.
#They have an origin, x and y axes, and a linear unit of measurement such as meters.
# All projected CRSs are based on a geographic CRS, described in the previous section, 
# and rely on map projections to convert the 3D surface of the Earth into Easting and Northing (x and y) values in a projected CRS.
# some properties area, direction, distance, and shape. 
# projected coordinate system can preserve only one or two of those properties.
# named based on a property they preserve: 
# equal-area preserves area, 
# azimuthal preserve direction, 
# equidistant preserve distance, 
# and conformal preserve local shape.

# three main groups of projection types - conic, cylindrical, and planar

#  In a conic projection, projected onto a cone along a single line of tangency or two lines of tangency.
# Distortions are minimized along the tangency lines and rise with the distance from those lines in this projection,  best suited for maps of mid-latitude areas

# A planar projection projects data onto a flat surface touching the globe at a point or along a line of tangency. 
# It is typically used in mapping polar regions. 
st_proj_info(type = "proj")  #gives a list of the available projections supported by the PROJ library.



st_proj_info(type = "datum")  # # Geographic coordinate systems
st_proj_info(type = "proj")   # # Projected coordinate reference systems

# CRSs in R mainly by  epsg code or a proj4string definition. 
# epsg code  shorter and also refers to only one, well-defined coordinate reference system. 
# Oproj4string definition allows you more flexibility 
# in specifying different parameters such as the projection type, the datum and the ellipsoid

rgdal::make_EPSG() # outputs a data frame of epsg available projections
# filter ones of interest (try filtering for the OSGB CRSs for example):

st_crs(new_vector) # get CRS # No EPSG code, #> proj4string: "+proj=utm +zone=12 +ellps=GRS80 ... +units=m +no_defs"
new_vector = st_set_crs(new_vector, 4326) # set CRS
# replacing crs does not reproject data; use st_transform

#projection() function can be used to access CRS information from a Raster* object:
projection(new_raster) # get CRS  or set a CRS for raster objects. but only accept proj4 definitions:

projection(new_raster) = "+proj=utm +zone=12 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 
                            +units=m +no_defs" # set CRS

# CRSs is that they contain information about spatial units. sf objects is that they have native support for units
luxembourg = world[world$name_long == "Luxembourg", ]
st_area(luxembourg)   #> 2.41e+09 [m^2]

# diffrent units. The solution is to set the correct units with the units package:
units::set_units(st_area(luxembourg), km^2)

# The new_raster object (see above) uses a WGS84 projection with decimal degrees as units. 
# Consequently, its resolution is also given in decimal degrees but you have to know it, since the res() function simply returns a numeric vector.

res(new_raster)
#> [1] 0.000833 0.000833

methods(class = "sf") # methods for sf objects, first 12 shown



# The geometry column of sf objects is typically called geometry but any name can be used. 
st_sf(data.frame(n = world$name_long), g = world$geom) # would have column name g
# This enables geometries imported from spatial databases names such as wkb_geometry and the_geom.

#Basic prop
# dim(world), nrow(). ncol(), ..
# Extracting the attribute data of an sf object is the same as removing its geometry:
world_df = st_drop_geometry(world)
class(world_df)

# non-spatial data operations on sf objects only change an object’s geometry when appropriate
# (e.g., by dissolving borders between adjacent polygons following aggregation). 
# This means that proficiency with attribute data in sf objects equates to proficiency with data frames in R.

# Base R subsetting functions include [, subset() and $. dplyr subsetting functions include select(), filter(), and pull(). 
# Both sets of functions preserve the spatial components of attribute data in sf objects.

small_countries = world[world$area_km2 < 10000, ]
small_countries = subset(world, area_km2 < 10000)   # base R

# To avoid this error message, and prevent ambiguity,
# use the long-form function name, prefixed by the package name and two colons
dplyr::select().

# all columns between name_long and pop (inclusive), Omit specific columns with the - operator:
world2 = dplyr::select(world, name_long:pop , -subregion, -area_km2 , population = pop2 )

# Due to the sticky geometry column, selecting a single attribute from an sf-object with the help of [() returns also a data frame. 
# Contrastingly, pull() and $ will give back a vector.

# slice() is the row-equivalent of select(). 
slice(world, 3:5)

world6 = dplyr::filter(world, lifeExp > 82)



# world_agg1 = base::aggregate(pop ~ continent, FUN = sum, data = world, na.rm = TRUE)
class(world_agg1)

# aggregared of sf activates automatically for sf data
world_agg2 = sf::aggregate(world["pop"], by = list(world$continent),
                       FUN = sum, na.rm = TRUE)
class(world_agg2)

# summarize() is the dplyr equivalent of aggregate(). It usually follows group_by()

world %>% 
  dplyr::select(pop, continent) %>% 
  group_by(continent) %>% 
  summarize(pop = sum(pop, na.rm = TRUE), n_countries = n()) %>% 
  top_n(n = 3, wt = pop) %>%
  arrange(desc(pop)) %>%
  st_drop_geometry()

# careful 
coffee_world = left_join(coffee_data, world)  # sf dat if not on left would resulto in only df
# Fortunately, non-spatial data frames with a geometry list column (like coffee_world) can be coerced into an sf object as follows: st_as_sf(coffee_world). 

setdiff(coffee_data$name_long, world$name_long)
#> [1] "Congo, Dem. Rep. of" "Others"
str_subset(world$name_long, "Dem*.+Congo")  # to get what is the name in world database
#> [1] "Democratic Republic of the Congo"

coffee_data$name_long[grepl("Congo,", coffee_data$name_long)] = 
                                str_subset(world$name_long, "Dem*.+Congo")

world_coffee_match = inner_join(world, coffee_data)
#> Joining, by = "name_long"
nrow(world_coffee_match)


# unite() pastes together existing colum
world_separate = world_unite %>% 
  separate(con_reg, c("continent", "region_un"), sep = ":")

new_names = c("i", "n", "c", "r", "s", "t", "a", "p", "l", "gP", "geom")
world %>% set_names(new_names)  # rename all columns

# to avoid accidental removel of geometry, 
# it has to be explicitly remove it because sf explicitly makes the geometry column sticky. 

world_data = world %>% st_drop_geometry()   # class(world_data)

# Manipulating raster objects #
# Because of their unique structure, subsetting and other operations on raster datasets work in a different way,

# create an example raster named elev (representing elevations).

elev = raster(nrows = 6, ncols = 6,  res = 0.5,  vals = 1:36,
              xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5)
             
elev
# raster objects can contain values of class numeric, integer, logical or factor, but not character
# convert char to factor 


grain_order = c("clay", "silt", "sand")
grain_char = sample(grain_order, 36, replace = TRUE)
grain_fact = factor(grain_char, levels = grain_order)
grain = raster(nrows = 6, ncols = 6, res = 0.5,  vals = grain_fact,
               xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5      )

grain

#The raster object stores the corresponding look-up table or “Raster Attribute Table” (RAT) as a data frame in a new slot named attributes, 
# which can be viewed with ratify(grain) (see ?ratify()

stack(elev, grain)[1] # returns a matrix with one row and two columns — one for each layer.

# summary() and cellStats() functions with a raster stack or brick object, they will summarize each layer separately, 
# summary(brick(elev, grain)).



########
seine_simp = st_simplify(seine, dTolerance = 2000)  # 2000 m
object.size(seine) #> 17304 bytes
object.size(seine_simp) #> 8320 bytes



us_states_simp1 = st_simplify(us_states2163, dTolerance = 100000)  # 100 km

seine_buff_5km = st_buffer(seine, dist = 5000)
seine_buff_50km = st_buffer(seine, dist = 50000)
#Affine transformation is any transformation that preserves lines and parallelism.
# Affine transformations include, among others, shifting (translation), scaling and rotation. 

##########

st_intersects(p, a, sparse = FALSE)
st_disjoint(p, a, sparse = FALSE)[, 1]  #> [1] FALSE FALSE  TRUE  TRUE
st_within(p, a, sparse = FALSE)[, 1]
st_touches(p, a, sparse = FALSE)[, 1]
# features that do not touch, but almost touch the selection object st_is_within_distance()
st_is_within_distance()
sel = st_is_within_distance(p, a, dist = 0.9) # can only return a sparse matrix
lengths(sel) > 0


#  Joining two non-spatial datasets relies on a shared ‘key’ variable: 
#  Spatial data joining instead relies on shared areas of geographic space ( spatial overlay). 
# both case adds a new column to the target object 

# Random points to demonstrate spatial joining are created as follows:
set.seed(2018)               # set seed for reproducibility
(bb_world = st_bbox(world))  # the world's bounds
random_df = tibble(  x = runif(n = 10, min = bb_world[1], max = bb_world[3]),
                     y = runif(n = 10, min = bb_world[2], max = bb_world[4])    )
random_points = random_df %>% 
                st_as_sf(coords = c("x", "y")) %>% # set coordinates
                st_set_crs(4326)                   # set geographic CRS


# The random_points object has no attribute data, while the world (top right) does. 
# The spatial join operation is done by st_join(), which adds the name_long variable to the points, resulting in random_joined
world_random = world[random_points, ]
nrow(world_random)
random_joined = st_join(random_points, world["name_long"])

# By default, st_join() performs a left join, but can set left = FALSE
# Like spatial subsetting, the default topological operator used by st_join() is st_intersects()
# but changed with the join argument (see ?st_join


plot(st_geometry(cycle_hire), col = "blue")
plot(st_geometry(cycle_hire_osm), add = TRUE, pch = 3, col = "red")


#We can check if any points are the same st_intersects() as shown below:
any(st_touches(cycle_hire, cycle_hire_osm, sparse = FALSE))

# non-overlapping join is needed. The simplest method is to use the topological operator st_is_within_distance() 
# firt projected coordinates
cycle_hire_P = st_transform(cycle_hire, 27700)
cycle_hire_osm_P = st_transform(cycle_hire_osm, 27700)
sel = st_is_within_distance(cycle_hire_P, cycle_hire_osm_P, dist = 20)
summary(lengths(sel) > 0)


# retrieve the values associated with the respective cycle_hire_osm_P points? The solution is again with st_join(), 
# but with an addition dist argument (set to 20 m below):

z = st_join(cycle_hire_P, cycle_hire_osm_P, st_is_within_distance, dist = 20)
nrow(cycle_hire)  ##> [1] 742
nrow(z)


nz_avheight = aggregate(x = nz_height, by = nz, FUN = mean)


##########

seine_simp = st_simplify(seine, dTolerance = 2000)  # 2000 m

us_states_simp1 = st_simplify(us_states2163, dTolerance = 100000)  # 100 km

# proportion of points to retain (0-1; default 0.05)
us_states2163$AREA = as.numeric(us_states2163$AREA)
us_states_simp2 = rmapshaper::ms_simplify(us_states2163, keep = 0.01, keep_shapes = TRUE)

# Douglas-Peucker (st_simplify) and Visvalingam (ms_simplify) algorithm outputs

# generates the geographic centroids of regions in New Zealand and tributaries to the River Seine, illustrated with black points in Figure 5.3.
nz_centroid = st_centroid(nz)
seine_centroid = st_centroid(seine)

# Sometimes the geographic centroid falls outside the boundaries of their parent objects
nz_pos = st_point_on_surface(nz)
seine_pos = st_point_on_surface(seine)

# Buffers are polygons representing the area within a given distance of a geometric feature: regardless of whether the input is a point, line or polygon, the output is a polygon.

seine_buff_5km = st_buffer(seine, dist = 5000)
seine_buff_50km = st_buffer(seine, dist = 50000)

# Affine transformation is any transformation that preserves lines and parallelism. However, angles or length are not necessarily preserved.

# The sf package implements affine transformation for objects of classes sfg and sfc.
nz_sfc = st_geometry(nz)

#Shifting moves every point by the same distance in map units
nz_shift = nz_sfc + c(0, 100000)

#Scaling enlarges or shrinks objects by a factor. either globally or locally.
nz_centroid_sfc = st_centroid(nz_sfc)
nz_scale = (nz_sfc - nz_centroid_sfc) * 0.5 + nz_centroid_sfc



# It rotates points in a clockwise direction. The rotation matrix can be implemented in R as:

rotation = function(a){    r = a * pi / 180 #degrees to radians
                          matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
}

nz_rotate = (nz_sfc - nz_centroid_sfc) * rotation(30) + nz_centroid_sfc

# Finally, the newly created geometries can replace the old ones with the st_set_geometry() function:

nz_scale_sf = st_set_geometry(nz, nz_scale)

# Clipping can only apply to features more complex than points: lines, polygons and their ‘multi’ equivalents. 

b = st_sfc(st_point(c(0, 1)), st_point(c(1, 1))) # create 2 points
b = st_buffer(b, dist = 1) # convert points to circles
plot(b)
text(x = c(-0.5, 1.5), y = 1, labels = c("x", "y")) # add text

x = b[1]
y = b[2]
x_and_y = st_intersection(x, y)
plot(b)
plot(x_and_y, col = "lightgrey", add = TRUE) # color intersecting area

bb = st_bbox(st_union(x, y))
box = st_as_sfc(bb)
set.seed(2017)
p = st_sample(x = box, size = 10)
plot(box)
plot(x, add = TRUE)
plot(y, add = TRUE)
plot(p, add = TRUE)
text(x = c(-0.5, 1.5), y = 1, labels = c("x", "y"))

# spatial aggregation can silently dissolve the geometries of touching polygons in the same group.
us_west = us_states[us_states$REGION == "West", ]
us_west_union = st_union(us_west)

texas = us_states[us_states$NAME == "Texas", ]
texas_union = st_union(us_west_union, texas)

# st_cast # Geometry casting is a powerful operation that enables transformation of the geometry type
multipoint = st_multipoint(matrix(c(1, 3, 5, 1, 3, 1), ncol = 2))
linestring = st_cast(multipoint, "LINESTRING")
polyg = st_cast(multipoint, "POLYGON")


# Conversion from multipoint to linestring is a common operation that creates a line object from ordered point observations, such as GPS measurements or geotagged media.
# Conversion from multipoint or linestring to polygon is often used to calculate an area, for example from the set of GPS measurements taken around a lake

# The transformation process can be also reversed using st_cast:

multipoint_2 = st_cast(linestring, "MULTIPOINT")
multipoint_3 = st_cast(polyg, "MULTIPOINT")
all.equal(multipoint, multipoint_2, multipoint_3)




library(geosphere)





# draw map programatically https://www.r-spatial.org//r/2018/10/25/ggplot2-sf-2.html
## install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel",  "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
# x <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap")
# install.packages("geojsonio")
# lapply(x, library, character.only = TRUE) # load the required packages

# Mollweide is an equal-area projection flsatted globe
# Orthographic projections resemble a globe, making them attractive for non-technical use
# Lambert conformal conic projection is an equal-area projection recommended by Snyder for regions of large east-west 
# stereographic projection (Snyder 1987 page 120) is conformal, used below for an Arctic view with a Canadian focus


# http://www.openstreetmap.org/?way=4639304
# http://www.openstreetmap.org/browse/way/4639304


library(tidyverse)
library(sf)                        # also loads  rgdal and worldmap
library(raster)
library( tmaptools )
library(geojsonio)
library(ggpubr) 
#library(cowplot)      # theme_set( theme_bw() )

#library(rgdal)
world
# ggplot() + geom_sf(data = world[SOV_A3="IND"],  aes() , color = "blue",  fill = "green" )     # plots world  !

# geojson to : geojson_sp() , geojson_list() , geojson_json() 
# geojson_read()/topojson_read(file path or URL)  # geojson_write()/topojson_write() 

#geom_sf( mapping = aes(),  data = NULL,  stat = "sf", position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...)
#world2 <- sf::st_transform( world1, "+proj=laea +y_0=0 +lon_0=155 +lat_0=-90 +ellps=WGS84 +no_defs"  )


######
library(raster)
#polygon_areas <- raster::shapefile("C:/yourshapefile.shp")
polygon_areas <- raster::shapefile("~/Desktop/gktex/GIS India district/IND_adm1.shp")
polygon_areas
crop_save <- function(origin_folder, pattern, destination_folder, name_sub_folder, crop_areas, name_crop_areas){
    file_list <- list.files(path = origin_folder, pattern)
    #Create folder
    dir.create(paste0(destination_folder,"/",name_sub_folder))
    how_many_areas <- nrow(crop_areas)
    #Create raster stack
    raster_stack <- stack() 
    #File paths
    paths1 <- paste0(origin_folder,file_list)
    #Load rasters to stack
    for(i in 1:length(file_list)){
        raster_stack <- stack(raster_stack, raster(paths1[i]))  
    }  
    names_list <-  eval(parse(text=name_crop_areas))
    numbers <- 1:length(names_list)
    names_list <- paste0(as.character(numbers),"_polygon_", names_list)
    polyRR_list <- list()
    for(x in 1: nrow(crop_areas)){
        pol1 <- assign(names_list[x],crop_areas[x,])
        polyRR_list[[x]] <- pol1
    }
    for(j in 1:nlayers(raster_stack)){
        dir.create(paste0(destination_folder,"/",name_sub_folder, "/", names(raster_stack)[j]))
        for(k in 1:length(polyRR_list)){
            a<-crop(raster_stack[[j]], polyRR_list[[k]])
            a<-mask(a,polyRR_list[[k]], filename = paste0(destination_folder,"/",name_sub_folder, "/", names(raster_stack)[j], "/", "RR",polyRR_list[[k]]$Id, ".tif"))
        }
    }
}


#origin_folder – Where the original rasters are saved.
# pattern – This is a character string to identify raster files: in the folder were rasters are saved there are, generally, other files. This argument allows the selection of only rasters (e.g. tif files).
# destination_folder – Folder where the otput folder will be created.
# name_sub_folder – Name of the sub-folder to be created inside the destination folder. Inside this, a folder is created for each of the original rasters where the smaller rasters for each polygon are saved.
# crop_areas – Areas to be used in the raster croping (a SpatialPolygonsDataFrame created by importing the shapefile into R).
# name_crop_areas – Column of the SpatialPolygonsDataFrame with the unique names or codes for the regions.

crop_save <- function(origin_folder, pattern, destination_folder, name_sub_folder, crop_areas, name_crop_areas){
    file_list <- list.files(path = origin_folder, pattern)
    #Create folder
    dir.create(paste0(destination_folder,"/",name_sub_folder))
    how_many_areas <- nrow(crop_areas)
    #Create raster stack
    raster_stack <- stack() 
    #File paths
    paths1 <- paste0(origin_folder,file_list)
    #Load rasters to stack
    for(i in 1:length(file_list)){
        raster_stack <- stack(raster_stack, raster(paths1[i]))  
    }  
    names_list <-  eval(parse(text=name_crop_areas))
    numbers <- 1:length(names_list)
    names_list <- paste0(as.character(numbers),"_polygon_", names_list)
    polyRR_list <- list()
    for(x in 1: nrow(crop_areas)){
        pol1 <- assign(names_list[x],crop_areas[x,])
        polyRR_list[[x]] <- pol1
    }
    for(j in 1:nlayers(raster_stack)){
        dir.create(paste0(destination_folder,"/",name_sub_folder, "/", names(raster_stack)[j]))
        for(k in 1:length(polyRR_list)){
            a<-crop(raster_stack[[j]], polyRR_list[[k]])
            a<-mask(a,polyRR_list[[k]], filename = paste0(destination_folder,"/",name_sub_folder, "/", names(raster_stack)[j], "/", "RR",polyRR_list[[k]]$Id, ".tif"))
        }
    }
}

crop_save



#######
getwd()
mypic <-  raster("myfile.tif")

setwd("/Users/gk")
mypictiff <-  raster("vann.tiff") 
mypictiff
plot(mypictiff)       # ok

mypicpng <-  raster("newmap.png") 
mypicpng
plot(mypicpng)


mypictiffdf  <-  as.data.frame(mypictiff,  xy = TRUE )
mypictiffdf

ggplot() +
    geom_raster(data = mypictiffdf,  aes(x = x, y = y)) + 
    scale_alpha(range =  c(0.25, 0.85), guide = "none") + 
    scale_fill_viridis_c() +  
    ggtitle("Elevation with hillshade") +
    coord_quickmap()
    
###################

getwd()
setwd("/Users/gk/r4ds")
list.files()       # current dir files 
ls()               # list of files in current dir

# use st_read() read shapefile dir and  GeoJSON.
indrailways1  <-  st_read("railways")           #  dir of all four shpfiles and not railways.shp"
myfootplate1  <-  st_read("footplate.geojson")
ptransport    <-  st_read("public_transport_ln")


indrailways <- indrailways1
myfootplate1
myfootplate1$gkcol <- runif( nrow(myfootplate1), min = 0, max = 5)


?l <- myfootplate1 %>% st_coordinates()  %>% ?group_by(altitude) %>% summarize(m = mean(attr_data) ) %>%  st_cast("LINESTRING")

l <- myfootplate1 %>% st_coordinates()    %>% # group_by(altitude) %>% #summarize(m = mean(gkcol)     )   %>%
                      st_linestring()      #  ?  st_cast("LINESTRING") 
l

plot(l)

plot(ptransport)



library(rgdal)
myfootplate2 <- readOGR("footplate.geojson", "OGRGeoJSON")  # with library(rgdal)

plot( myfootplate1[1] )
plot( myfootplate2[1] )
# or
ggplot( myfootplate1[1] ) + geom_sf()    # or   plot(myfootplate1[1] )





###########
# alternative way
spdata_json <- geojson_read("footplate.geojson", what = "sp") # ok, no sf
footplatesf <- st_as_sf(spdata_json)                          # Can we read sf direct from geojson?
footplatesf

myfootplate1 %>% #group_by(altitude) %>% 
                 summarize(m = mean(speed)) %>%  
                 st_cast("LINESTRING") # -> footline
footline

? 
plot(footline)
? ggplot(footline[1]) + geom_sf() 

st_crs( st_as_sf(footplatesf) )


plot(footplatesf)    # ploted facet of all atrribute
plot(indrailways1[1], add = FALSE)
plot(footplatesf, add = TRUE) 

st_crs(footplatesf)    ##EPSG: 4326   proj4string: "+proj=longlat +datum=WGS84 +no_defs"
st_crs(indrailways1)

st_geometry_type(indrailways1)  # line 
st_geometry_type(footplatesf)   # point



st_crs(indrailways1)                   # find the crs 
st_bbox(indrailways1)                  # find the extent of our map data 
# or simply all meta data  indrailways
indrailways1
plot(indrailways1) # limit fields




### dummy data ########
pts_sf <- data.frame(x = seq(47, 48, by=0.1), y = seq(147, 148, by=0.1), attr_data = rnorm(11,42,42),
    id = c(rep("fred",6), rep("wilma",5)) ) %>% 
    sf::st_as_sf(coords = c("x","y")) %>% 
    sf::st_set_crs(4326)

pts_sf
plot(pts_sf)
ggplot(pts_sf) + geom_sf()

############################
setwd("/Users/gk/Google Drive")
setwd("/Users/gk/r4ds")
# or tmaptools::read_shape(filedir, current.projection = NULL, as.sf = TRUE, ...) # wrapper of rgdal's readOGR
mtrailway <- tmaptools::read_shape("railways/railways.shp", as.sf = TRUE , current.projection = NULL )  # , ...
mtrailway
identical(mtrailway, indrailways1) # false?

indiamap <- st_read("india_ds")    #  directory of india containg shp files"
indiamap
indiamap[2-3]  %>% View()
ggplot() +  geom_sf(data = indiamap)  + theme_minimal()    # ? +   coord_sf(crs = st_crs(4326)) 

mymapdf  <- indiamap
ms <- st_as_sf( mymapdf, coords = c('x', 'y'), crs = "+init=epsg:28992" )
ggplot() +  geom_sf(data = ms)  + theme_minimal()    # ? +   coord_sf(crs = st_crs(4326)) 

##### footplate ######
data_json <- geojson_read("footplate.geojson", what = "sp") # ok
data_json
footplate <- st_as_sf(data_json)
st_crs(st_as_sf(data_json))
plot(data_json)

fpbbox <- st_bbox(data_json)
str(fpbbox)


fpbbox[]

ggplot() +   geom_sf(data = indiamap ) +  
             coord_sf( ylim = c(27.21364, 32.78969 ), xlim = c(74.70470, 78.93227 )   ) # +  geom_sf(data = footplate) 
    
indiamap
footplate
#coord_sf(xlim = NULL, ylim = NULL, expand = TRUE, crs = NULL, datum = sf::st_crs(4326), ndiscr = 100, default = FALSE)

ggplot() +  geom_sf(data = footplate)   # done ! now add map layer

# When plotting sf objects with ggplot2, you need to use the coord_sf() coordinate system.

#  Use set_projection for reprojection. 
indrailways[ indrailways$osm_id == 4774133, ]
ggplot() +  geom_sf(data = indrailways[ indrailways$osm_id %in% c(4774129:4774800), ]  )  +   coord_sf(crs = st_crs(4326)) #  wgs84  good 4326 !!


library(ggthemes)
# theme_set(theme_economist())
theme_set(theme_wsj())

ggplot() +  geom_sf(data = indrailways, col= "red" )   +  
            # geom_sf( aes(fill = rate) )
            #coord_sf(crs = "+proj=merc") +  ggtitle("Mercator projection")
            #coord_sf(crs = st_crs(4326))   +
            coord_sf( xlim = c(69, 80),  ylim = c(26, 31 ) ) + # ?Coordinate system already present
            #scale_y_continuous() +
            #?lab(title = " Northern Railway lines drawing", xlab = "Long", ylab("latitude") )
            scale_color_brewer(palette = "Dark2")  +
            theme(legend.position = "top") +
            theme_bw()


    
ggplot() +  geom_sf(data = indrailways,  size = 3, color = "black", fill = "cyan1")  +   coord_sf(crs = st_crs(3035)) # good for europ but india angle
ggplot() +  geom_sf(data =  world,       size = 3, color = "black", fill = "cyan1") +    coord_sf(crs = st_crs(3035))

getwd()


# restor library
# raster() function to import specific bands in our raster object

RGB_band2_delhi <-  raster("data/delhi_RGB_Ortho.tif", band = 2)

# We can convert this data to a data frame and plot the same way we plotted the red band:

RGB_band2_delhi_df <- as.data.frame(RGB_band2_delhi, xy = TRUE)

ggplot() +  geom_raster(data = RGB_band2_delhi_df,  aes(x = x, y = y, alpha = HARV_RGB_Ortho)) + 
            coord_equal()
# + coord_quickmap()
# plotRGB(RGB_stack_HARV, r = 1, g = 2, b = 3)  # 3 band plot
# applying a stretch to the image might improve clarity and contrast using stretch="lin" or stretch="hist".
# plotRGB(RGB_stack_HARV, r = 1, g = 2, b = 3, scale = 800, stretch = "lin")  #  stretch = "hist", "lin"




library("rnaturalearth")
library("rnaturalearthdata")
world
world <- ne_countries( scale = "medium",  returnclass = "sf")     # default is "sp"
class(world)

# Field sites (point data) with longitude and latitude, stored in a regular data.frame:
    
(sites <- data.frame(longitude = c(-80.144005, -80.109), latitude = c(26.479005, 26.83)))

# simple way to add a point on coordinate with geom_point
ggplot(data = world) +
    geom_sf() +
    geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, shape = 23, fill = "darkred") +
    coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

# A better, more flexible alternative is to use the power of sf: 
# WGS84 has CRS code 4326 is pre defined in the sf object:

(sites <- st_as_sf( sites, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))

# above ## Simple feature collection with 2 features and 0 fields

ggplot(data = world) +
    geom_sf() +
    geom_sf(data = sites, size = 4, shape = 23, fill = "darkred") +
    coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

#  coord_sf  called after all geom_sf calls, as to supersede any former input.

# maps of usa automatially installed with ggplot
library("maps")
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)

states <- cbind(states, st_coordinates(st_centroid(states)))
states



ggplot(data =   world)    +
                geom_sf() +
                geom_sf(data = states, fill = NA) + 
                geom_text(data = states, aes(X, Y, label = ID), size = 5) +
                coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

##################################
#  ggplot2 can use of simple features from the package sf as layers in a graph1. 
# https://www.r-spatial.org//r/2018/10/25/ggplot2-sf.html

# install.packages(c("cowplot", "googleway",  "ggplot2", "ggrepel",  "ggspatial", "libwgeom", "sf", "rworldmap", "rworldxtra"))

library("ggplot2")
theme_set(theme_bw())
library("sf")             
library("rworldmap")    # rworldmap provides a map of countries
library("rworldxtra")   # rworldxtra for extra resolution maps

world <- getMap(resolution = "high")    # getMap as "SpatialPolygonsDataFrame" from pkg sp; 
world <- st_as_sf(world)                # convert to sf using   st_as_sf() of pkg sf, check with #class(world)
# All data must be in sf format for ggplot.  Convert sp to sf using  st_as_sf()
# layers  added one at a time in a ggplot call, so the order of each layer is very important. 


# creating a base map of the world using ggplot2.
ggplot()   + 
                        geom_sf( data = world,  aes(fill = POP_EST)    )     #  simply adds sf geometry

#                       scale_fill_viridis_c(option = "plasma", trans = "sqrt")
#                       xlab("Longitude") + ylab("Latitude") +
#                       ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)"))
#


world <- getMap(resolution = "high")   # is SpatialPolygonsDataFrame from sp  pkg; 

world <- st_as_sf(world)               # so convert to sf:

(g1  <- qplot(0:10, 0:10))
(g1_void <- g1 + theme_void() + theme(panel.border = element_rect(colour = "black",   fill = NA)))


# Combine sub-maps: using Grobs or ggdraw
# using Grobs (graphic objects, allow plots only in plot region, based on coordinates), which directly use ggplot2
# using ggdraw (allows plots anywhere, including outer margins, based on relative position) from package cowplot

#Using grobs, and annotation_custom:
g1 +
    annotation_custom(
        grob = ggplotGrob(g1_void),
        xmin = 0,
        xmax = 3,
        ymin = 5,
        ymax = 10
                     ) +
    annotation_custom(
        grob = ggplotGrob(g1_void),
        xmin = 5,
        xmax = 10,
        ymin = 0,
        ymax = 3
                    )


# using ggdraw
library(cowplot)
ggdraw(g1) +
    draw_plot(g1_void, width = 0.25, height = 0.5, x = 0.02, y = 0.48) +
    draw_plot(g1_void, width = 0.5, height = 0.25, x = 0.75, y = 0.09)


# Several maps side by side or on a grid

# First, simplify REGION for the legend:
levels(world$REGION)[7] <- "South America"

#Prepare the subplots, #1 world map:

(gworld <- ggplot(data = world) +
     geom_sf(aes(fill = REGION)) +
     geom_rect(xmin = -102.15, xmax = -74.12, ymin = 7.65, ymax = 33.97, 
               fill = NA, colour = "black", size = 1.5) +
     scale_fill_viridis_d(option = "plasma") +
     theme(panel.background = element_rect(fill = "azure"),
           panel.border = element_rect(fill = NA)))

# And #2 Gulf map :

(ggulf <- ggplot(data = world) +
        geom_sf(aes(fill = REGION)) +
        annotate(geom = "text", x = -90, y = 26, label = "Gulf of Mexico", 
                 fontface = "italic", color = "grey22", size = 6) +
        coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE) +
        scale_fill_viridis_d(option = "plasma") +
        theme(legend.position = "none", axis.title.x = element_blank(), 
              axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
              panel.border = element_rect(fill = NA)))


# The command ggplotGrob signals to ggplot to take each created map,and how to arrange each map. 
# The argument coord_equal can specify the length, ylim, and width, xlim, for the entire plotting area.
# Where as in annotation_custom, each maps’ xmin, xmax, ymin, and ymax can be specified to allow for complete customization.

#Creating a faux empty data frame

df <- data.frame()
plot1<-ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 10)
plot2<-ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 10)


ggplot() +
    coord_equal(xlim = c(0, 3.3), ylim = c(0, 1), expand = FALSE) +
    annotation_custom(ggplotGrob(plot1), xmin = 0, xmax = 1.5, ymin = 0,   ymax = 1) +
    annotation_custom(ggplotGrob(plot2), xmin = 1.5, xmax = 3, ymin = 0,   ymax = 1) +
    theme_void()




# Using ggplot to arrange maps, allows for easy and quick plotting in one function of R code.

ggplot() +
    coord_equal(xlim = c(0, 3.3), ylim = c(0, 1), expand = FALSE) +
    annotation_custom(ggplotGrob(gworld), xmin = 0, xmax = 2.3, ymin = 0,    ymax = 1) +
    annotation_custom(ggplotGrob(ggulf), xmin = 2.3, xmax = 3.3, ymin = 0,   ymax = 1) +
    theme_void()

# In the second approach, using cowplot::plot_grid to arrange ggplot figures, is quite versatile


library("cowplot")
theme_set(theme_bw())

plot_grid(gworld, ggulf, 
          nrow = 1, 
          # adding align=v to align vertically, and align=h to align  horiztonally.
          rel_widths = c(2.3, 1)  # width of each map , first with relative width of 2.3, and the map ggulf of 1.
          )
 # also use get_legend (cowplot), and that the legend can be used as any object.
 # This map can be save using,ggsave:     ggsave("grid.pdf", width = 15, height =  5)
ggsave("grid.pdf", width = 15, height =  5)



###
usa <- subset(world, ADMIN == "United States of America") 
## US National Atlas Equal Area (2163)
## http://spatialreference.org/ref/epsg/us-national-atlas-equal-area/

(mainland <- ggplot(data = usa) +
        geom_sf(fill = "cornsilk") +
        coord_sf( crs = st_crs(2163), 
                  xlim = c(-2500000, 2500000), 
                  ylim = c(-2300000,  730000)  )  )

# Alaska map (note: datum = NA removes graticules and coordinates):
## Alaska: NAD83(NSRS2007) / Alaska Albers (3467)
## http://www.spatialreference.org/ref/epsg/3467/
(alaska <- ggplot(data = usa) +
        geom_sf(fill = "cornsilk") +
        coord_sf(crs = st_crs(3467), 
        xlim = c(-2400000, 1600000), 
        ylim = c(200000,   2500000), 
        expand = FALSE, datum = NA))


# Hawaii map:
## Hawaii: Old Hawaiian (4135)
## http://www.spatialreference.org/ref/epsg/4135/
(hawaii  <- ggplot(data = usa) +
        geom_sf(fill = "cornsilk") +
        coord_sf(crs = st_crs(4135), 
        xlim = c(-161, -154), 
        ylim = c(18, 23), 
        expand = FALSE, datum = NA))

# Using ggdraw from cowplot (tricky to define exact positions; note the use of the ratios of the inset, combined with the ratio of the    plot):
    
(ratioAlaska <- (2500000 - 200000) / (1600000 - (-2400000)))
## [1] 0.575
(ratioHawaii  <- (23 - 18) / (-154 - (-161)))
## [1] 0.7142857

ggdraw(mainland) +
    draw_plot(alaska, width = 0.26, height = 0.26 * 10/6 * ratioAlaska, 
              x = 0.05, y = 0.05) +
    draw_plot(hawaii, width = 0.15, height = 0.15 * 10/6 * ratioHawaii, 
              x = 0.3, y = 0.05)


#This plot can be saved using ggsave:
    
ggsave("map-us-ggdraw.pdf", width = 10, height = 6)
#The same kind of plot can be created using grobs, with ggplotGrob, (note the use of xdiff/ydiff and arbitrary ratios):
    
mainland +
    annotation_custom(
        grob = ggplotGrob(alaska),
        xmin = -2750000,
        xmax = -2750000 + (1600000 - (-2400000))/2.5,
        ymin = -2450000,
        ymax = -2450000 + (2500000 - 200000)/2.5
    ) +
    annotation_custom(
        grob = ggplotGrob(hawaii),
        xmin = -1250000,
        xmax = -1250000 + (-154 - (-161))*120000,
        ymin = -2450000,
        ymax = -2450000 + (23 - 18)*120000
    )


#This plot can be saved using ggsave:
    
ggsave("map-inset-grobs.pdf", width = 10, height = 6)

#The print command can also be used place multiple maps in one plotting area.
# viewport ???
vp <- viewport(width = 0.37, height = 0.10, x = 0.20, y =0.25, just = c("bottom"))   
vp1<- viewport(width = 0.37, height = 0.10, x = 0.35, y =0.25, just = c("bottom")) 

# Theprint function uses the previous specifications that were listed in each plots’ respective viewport, with vp=.

print(mainland)
print(alaska, vp=vp)
print(hawaii, vp=vp1)

################################################

#install.packages(c(“sf”,“stplanr”,“tidyverse”,“tmap”,“openair”))

#https://www.r-bloggers.com/flat-earth-mathematics-in-the-r-language/

library(tidyverse)
world <- map_data("world")
worldmap <- ggplot(world) +
            geom_path(aes(x = long, y = lat, group = group)) +
            theme(axis.text  = element_blank(),
                  axis.ticks = element_blank()             ) +
            labs( x = "" ,  y = "" )

worldmap  + 
            coord_map(  "azequidistant" , orientation = c( 90,  0,  260)  )    # lat lon  , rot



###############################################################

#https://www.r-bloggers.com/beyond-basic-r-mapping/
# researchers at the United States Geological Survey (USGS) 
# Idaho National Laboratory (INL) Project Office.
# not in CRAN ! install.packages(sbtools)
library(sbtools)  # install
library(dataRetrieval)
?dataRetrieval
library(sf)

item_file_download(sb_id = "5a83025ce4b00f54eb32956b", 
                   names = "huc8_05010007_example.zip", 
                   destinations = "huc8_05010007_example.zip", 
                   overwrite_file = TRUE)
## [1] "huc8_05010007_example.zip"
unzip('huc8_05010007_example.zip', overwrite = TRUE)
#note st_read takes the folder, not a particular file
huc_poly <- st_read('huc8_05010007_example')


## Reading layer `wbdhu8_alb_simp' from data source `D:\LADData\RCode\owi-blog\content\huc8_05010007_example' using driver `ESRI Shapefile'
## Simple feature collection with 1 feature and 9 fields

class(huc_poly)
str(huc_poly)
st_geometry(huc_poly)

st_bbox(huc_poly)
st_crs(huc_poly)

huc_gages <- whatNWISdata(huc = "05010007", parameterCd = "00060", service="uv")
head(huc_gages)




############
# Run these commands one by one to see the map take shape — 
# first we create a blank state map, 
# then add county lines in white, then the HUC boundary, 
# then the gage points, and then the legend. 
# Note that we use the st_geometry function inside of the plot command so that we only plot the huc_poly and huc_gages_sf geometry, and not the other information in their data frames.

library(maps)

map(database = 'state',  regions = 'Pennsylvania', col = "tan",   fill = TRUE, border = NA)
#this draws all PA counties since the regions argument uses partial matching
map(database = 'county', regions = 'Pennsylvania', col = "green", fill = FALSE,       add = TRUE )
# map(database = 'county', regions = 'Pennsylvania', col = "green", fill = FALSE,       add = FALSE )

plot(st_geometry(huc_poly), col = NA, add = TRUE    )

plot(st_geometry(huc_gages_sf),       add = TRUE, col = "red", pch = 19, cex = 0.7)


legend( "bottomright", 
        title = "Legend",
        legend = c("Gage", "Subbasin boundary"), 
        pch = c(19,NA), lty = c(NA, 1),
        col = c("red", "black") )
title("Conemaugh Subbasin")


# Similarly, we can create a map zoomed in to the HUC polygon. Note that we set the x and y limits of the map by extracting the limits of the bbox object we created earlier. We can use the names left, right, etc. because bbox is a named vector.

map(database = 'county', regions = 'Pennsylvania', col = "lightgray", 
    xlim = bbox[c('left', 'right')], ylim = bbox[c('bottom', 'top')])

plot(st_geometry(huc_poly), col = "dodgerblue", border = NA, add = TRUE)
box()
plot(st_geometry(huc_gages_sf), add = TRUE, col = "red", pch = 19, cex = 0.7)

legend("bottomleft", legend = c("Gage", "Conemaugh subbasin"), pch = c(19,NA), lty = c(NA, 1),
       col = c("red", "dodgerblue"), title = "Legend", lwd = c(1,15), bg = "wheat")
text(x = huc_gages$dec_long_va, y = huc_gages$dec_lat_va, labels = huc_gages$site_no,
     pos = 4, cex = 0.7)







################################
## https://www.r-bloggers.com/drawing-beautiful-maps-programmatically-with-r-sf-and-ggplot2-part-1-basics/

install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", "ggspatial", "libwgeom", "sf", "rworldmap", "rworldxtra"))

library(sf)
demo(nc, ask = FALSE, echo = FALSE)
plot(st_geometry(nc))
plot(st_geometry(nc), col = sf.colors(12, categorical = TRUE), border = 'grey',  axes = TRUE)

plot(st_geometry(st_centroid(nc)), pch = 3, col = 'red', add = TRUE)
plot(nc)

plot(nc, max.plot = 14)

options(sf_max.plot=1)
plot(nc)
plot(nc["AREA"])


plot(nc["AREA"], key.pos = 4)

plot(nc["AREA"], key.pos = 1, axes = TRUE, key.width = lcm(1.3), key.length = 1.0)


nc$f = cut(nc$AREA, 10)
plot(nc["f"], axes = TRUE, key.pos = 4, pal = sf.colors(10), key.width = lcm(4.5))

plot(nc["AREA"], breaks = c(0,.05,.1,.15,.2,.25))

plot(nc["AREA"], breaks = "jenks")

plot(st_geometry(nc), axes = TRUE)

lat_ts = mean(st_bbox(nc)[c(2,4)]) # latitude of true scale
eqc = st_transform(nc, paste0("+proj=eqc +lat_ts=", lat_ts))
plot(st_geometry(eqc), axes = TRUE)


library(maps)
usa = st_as_sf(map('usa', plot = FALSE, fill = TRUE))
laea = st_crs("+proj=laea +lat_0=30 +lon_0=-95") # Lambert equal area
usa <- st_transform(usa, laea)
g = st_graticule(usa)
plot(st_geometry(g), axes = TRUE)

plot(usa, graticule = TRUE, key.pos = NULL, axes = TRUE)

g = st_graticule(usa, lon = seq(-130,-65,5))
plot(usa, graticule = g, key.pos = NULL, axes = TRUE,
     xlim = st_bbox(usa)[c(1,3)], ylim = st_bbox(usa)[c(2,4)],
     xaxs = "i", yaxs = "i")
#Package sf provides a number of methods for st_as_grob:

methods(st_as_grob)


# contains a geom specially for simple feature objects,
# with support for graticule white lines in the background using sf::st_graticule. 

## devtools::install_github("tidyverse/ggplot2")
## library(ggplot2)

if (utils::packageVersion("ggplot2") > "2.2.1")
    ggplot() + geom_sf(data = usa)


# polygons can be colored using aes:

if (utils::packageVersion("ggplot2") > "2.2.1")
    ggplot() + geom_sf(data = nc, aes(fill = BIR74)) + scale_y_continuous(breaks = 34:36)


#and sets of maps can be plotted as facet plots after rearranging the sf object, e.g. by

library(dplyr)
library(tidyr)
nc2 <- nc %>% select(SID74, SID79, geom) %>% gather(VAR, SID, -geom)
if (utils::packageVersion("ggplot2") > "2.2.1")
    ggplot() + geom_sf(data = nc2, aes(fill = SID)) + facet_wrap(~VAR, ncol = 1) +
    scale_y_continuous(breaks = 34:36)


# mapview
#Package mapview creates interactive maps in html pages, using package leaflet as a work horse. Extensive examples are found here.

library(mapview)
# if (Sys.getenv("USER") != "CRAN")
mapview(nc["BIR74"], col.regions = sf.colors(10))


##############

install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rworldmap", "rworldxtra"))


library("ggplot2")
theme_set(theme_bw())

library("sf")
library("rworldmap")
library("rworldxtra")

world <- getMap(resolution = "high")
class(world)


world <- st_as_sf(world)
class(world)

ggplot(data = world)         +        #geom_sf()  
            geom_sf(color =  "black", fill = "lightgreen") +
            #geom_sf(aes(fill = POP_EST)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
            xlab("Longitude") + ylab("Latitude") +
             coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ") +
            # or coord_sf(crs = "+init=epsg:3035") +
            # coord_sf(crs = st_crs(3035)) +
            # coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE)
            ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)"))


names(world)
plot(world)
###############linl#####################

# Scale bar and North arrow (package ggspatial)
library("ggspatial")

ggplot(data = world) +
    geom_sf(fill = "antiquewhite1") +
    geom_text(aes(LON, LAT, label = NAME), size = 4, hjust = "left",  color = "darkblue", fontface = "bold", check_overlap = TRUE) +
    annotate(geom = "text", x = -90, y = 26, label = "Gulf of Mexico", fontface = "italic", color = "grey22", size = 6) +
    annotation_scale(location = "bl", width_hint = 0.5) +
    annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),  style = north_arrow_fancy_orienteering) +
    coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE) +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("Gulf of Mexico and Caribbean Sea") +
    theme(panel.grid.major = element_line(color = gray(.5),  linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue")  )


#ggsave("map.pdf")
ggsave("map_web.png", width = 6, height = 6, dpi = "screen")


###########################################

library(caret)
library(dplyr)
library(ggmap)

data(Sacramento)
df <- Sacramento %>% group_by(city) %>%
      summarize(median_price = median(price), transactions = n(),
                latitude = mean(latitude), longitude = mean(longitude) )

ggplot() + 
      geom_point(data = df, mapping = aes(x = longitude, y = latitude, 
                                        col = median_price, size = transactions)) +
      scale_color_distiller(palette = "YlOrRd", direction = 1)

ggplot(data = df, mapping = aes(x = longitude, y = latitude)) + 
    geom_point(aes(col = median_price, size = transactions)) +
    geom_text(aes(label = city), size = 2, nudge_y = 0.01) +
    scale_color_distiller(palette = "YlOrRd", direction = 1)



height <- max(Sacramento$latitude) - min(Sacramento$latitude)
width <- max(Sacramento$longitude) - min(Sacramento$longitude)
sac_borders <- c(bottom  = min(Sacramento$latitude)  - 0.1 * height, 
                 top     = max(Sacramento$latitude)  + 0.1 * height,
                 left    = min(Sacramento$longitude) - 0.1 * width,
                 right   = max(Sacramento$longitude) + 0.1 * width)

map <- get_stamenmap(sac_borders, zoom = 10, maptype = "toner-lite")

ggmap(map)


ggmap(map) +
    geom_point(data = df, mapping = aes(x = longitude, y = latitude, 
                                        col = median_price, size = transactions)) +
    scale_color_distiller(palette = "YlOrRd", direction = 1)

#alt
qmplot(x = longitude, y = latitude, data = df, maptype = "watercolor", 
       geom = "point", color = median_price, size = transactions) +
    scale_color_gradient(low = "blue", high = "red")




qmplot(x = longitude, y = latitude, data = df, maptype = "terrain", 
       geom = "point", color = median_price, size = transactions) +
    scale_color_gradient(low = "blue", high = "red")


qmplot(x = longitude, y = latitude, data = df, maptype = "toner", 
       geom = "point", color = median_price, size = transactions) +
    scale_color_gradient(low = "blue", high = "red")







#=======================================
# INSTALL DEVELOPMENT VERSION OF ggplot2
#=======================================

library(devtools)

# dev_mode(on = T)
# install_github("hadley/ggplot2")

#==============
# LOAD PACKAGES
#==============
library(sf)
library(tidyverse)
library(stringr)
library(scales)

# Get the data
# Next, we’ll get the data from a URL at Sharp Sight and load it into R:

#=========
# GET DATA
#=========

url.france_pop <- url("http://sharpsightlabs.com/wp-content/datasets/france_population_data_2016.RData")

load(url.france_pop)

## https://www.r-bloggers.com/mapping-france-at-night-with-the-new-sf-package/

# INSPECT
glimpse(df.france)

#Change column names to lower case
#We won’t have to make too many adjustments to the data, but we will transform the variable names to lower case.

#======================================
# CHANGE COLUMN NAMES: lowercase
# - in the original dataset the column
#   names were capitalized
# - we'll transform the names to 
#   lower case
#======================================
colnames(df.france) <- colnames(df.france) %>% str_to_lower()


# INSPECT
colnames(df.france)


#=========================
# CREATE DENSITY VARIABLE
#=========================
df.france$population %>% summary()


#-------------------------
# CREATE VARIABLE: density
#-------------------------
df.france <- df.france %>% mutate(density = population/superficie*100)


# INSPECT  
colnames(df.france)
head(df.france)


#==========================================
# PLOT TEST CHART
# - here, we'll just plot a test chart
#   to see if the data are roughly correct
#   and to make sure that the data are
#   roughly in working order
#==========================================

#ggplot(df.france) + geom_sf()
ggplot(df.france) + geom_sf(aes(fill = density))

#==========================================
# PLOT SECOND TEST CHART
# - here, we'll plot another test with
#   smaller borders
# - we'll set size = .1
#==========================================

ggplot(df.france) + geom_sf(aes(fill = density), color = NA)


#=======================================
# INSTALL DEVELOPMENT VERSION OF ggplot2
#=======================================

library(devtools)
dev_mode(on = T)
install_github("hadley/ggplot2")

#==============
# LOAD PACKAGES
#==============
library(sf)
library(tidyverse)
library(stringr)
library(scales)

#=========
# GET DATA
#=========
url.france_pop <- url("http://sharpsightlabs.com/wp-content/datasets/france_population_data_2016.RData")

load(url.france_pop)

# INSPECT
glimpse(df.france)

#======================================
# CHANGE COLUMN NAMES: lowercase
# - in the original dataset the column
#   names were capitalized
# - we'll transform the names to 
#   lower case
#======================================
colnames(df.france) <- colnames(df.france) %>% str_to_lower()


# INSPECT
colnames(df.france)

#=========================
# CREATE DENSITY VARIABLE
#=========================
df.france$population %>% summary()


#-------------------------
# CREATE VARIABLE: density
#-------------------------
df.france <- df.france %>% mutate(density = population/superficie*100)


# INSPECT  
colnames(df.france)
head(df.france)

#==========================================
# PLOT TEST CHART
# - here, we'll just plot a test chart
#   to see if the data are roughly correct
#   and to make sure that the data are
#   roughly in working order
#==========================================

#ggplot(df.france) + geom_sf()
ggplot(df.france) + geom_sf(aes(fill = density))



#==========================================
# PLOT SECOND TEST CHART
# - here, we'll plot another test with
#   smaller borders
# - we'll set size = .1
#==========================================

ggplot(df.france) + geom_sf(aes(fill = density), color = NA)




#=======================================================
# IDENTIFY QUANTILES
# - the hardest part of creating the finalized
#   data visualization is getting the color scale
#   just right
# - on final analysis, the best way to adjust the color
#   scale was by capping the population density
# - we'll use these quantiles to find a good cap
#   via trial and error
#=======================================================


#---------
# VENTILES
#---------

quantile(df.france$density, seq(from = 0, to = 1, by = .05))


#----------------------
# PERCENTILES: 90 - 100
#----------------------

quantile(df.france$density, seq(from = .9, to = 1, by = .01))

# MEDIAN
median(df.france$density) #40.08812


#==================
# DETAILED VERSION
# - add theme
# - add colors
#==================

df.france %>%
    mutate(fill_var = density) %>%
    ggplot() + 
    geom_sf(aes(fill = fill_var, color = fill_var)) +
    scale_fill_gradientn(colours = c("#000233","#f9cf86","#fceccf","white") 
                         ,values = rescale(c(0,500,3000,40000))
                         ,breaks = c(0,500,1500)
                         ,guide = FALSE) + 
    scale_color_gradientn(colours = c("#000233","#f9cf86","#fceccf","white") 
                          ,values = rescale(c(0,500,3000,40000))
                          ,guide = FALSE) +
    labs(title = "Population density in France\nvisualized to imitate satellite images of France at night" 
         ,subtitle = "(people/sq km)" ) +
    theme(text = element_text(family = "Gill Sans", color = "#E1E1E1")
          ,plot.title = element_text(size = 18, color = "#E1E1E1")
          ,plot.subtitle = element_text(size = 10)
          ,plot.background = element_rect(fill = "#000223")
          ,panel.background = element_rect(fill = "#000223")
          ,panel.grid.major = element_line(color = "#000223")
          ,axis.text = element_blank()
          ,axis.ticks = element_blank()
          ,legend.background = element_blank()
    )

#==================
# TURN OFF DEV MODE
#==================
dev_mode(on = F)



scale_fill_gradientn(colours = c("#000233","#f9cf86","#fceccf","white") 
                       , values = rescale(c(0,500,3000,40000) ) )
                     
                     
                     
########################################
                     
                     
install.packages("sabre")  
#In this blogpost, we will use only two R packages:
                     
# attach packages
library(sf)
library(sabre)
                     
                     
                     # data download
                     url = "http://sil.uc.edu/cms/data/uploads/software_data/vmeasure/regions.zip"
                     download.file(url, destfile = "regions.zip")
                     unzip("regions.zip", exdir = "data")
                     
                     
                     # Importantly, sabre requires that two input datasets have the same coordinate reference system (CRS).
                     # If your datasets have different CRS’s, you can transform one of the datasets to have the same CRS as the second one.
                     
                     
                     # read data
                     europe_borders = st_read("data/europe_borders.gpkg", quiet = TRUE)
                     biogeo_regions = st_read("data/biogeo_regions.gpkg", quiet = TRUE)
                     
                     #Note: the sabre package expects data with valid geometries only.
                     #You can fix invalid geometries with lwgeom::st_make_valid().
                     
                     
                     #The main function in sabre is vmeasure_calc().
                     #It calculates a degree of spatial association between two regionalizations 
                     #using an information-theoretical measure called the V-measure.
                     
                     
#Let’s apply v-measure to compare our two maps:
                     
sabre_output = vmeasure_calc(europe_borders, name, biogeo_regions, code)
                     
#The output gives five elements – values of v-measure, homogeneity, and completeness and two maps:
                     
sabre_output
                     
                     
regions_mc = mapcurves_calc(regions1, z, regions2, z)
regions_mc
                     
#################
library("ggplot2")
theme_set(theme_bw())
library("sf")
                     
library("rworldmap")
library("rworldxtra")
world <- getMap(resolution = "high")
class(world)
                     
world <- st_as_sf(world)
class(world)
                     
ggplot(data = world) +    geom_sf()
                     
ggplot(data = world) +
                         geom_sf() +
                         xlab("Longitude") + ylab("Latitude") +
                         ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)"))
                     
                     
ggplot(data = world) +   geom_sf(color = "black", fill = "lightgreen")
                     
ggplot(data = world) +   geom_sf(aes(fill = POP_EST)) +
                         scale_fill_viridis_c(option = "plasma", trans = "sqrt")
                     
                     
 
######################
####################

library(ggplot2)
library(sf)
library(rnaturalearth)
# ne_countries() from the package worldmap, we can get a spatial dataset with the borders of all countries 
worldmap <- ne_countries(scale = 'medium', type = 'map_units',  returnclass = 'sf')
worldmap


ggplot() + geom_sf(data = worldmap) 
# Selecting certain areas of interest from a spatial dataset

france <- worldmap[ worldmap$name == 'France' ,  ]
ggplot() + geom_sf(data = france) 

europe <- worldmap[ worldmap$continent == 'Europe',  ]
ggplot() + geom_sf(data = europe) 

# crop
europe_cropped <- st_crop(worldmap, xmin = -20, xmax = 45,  ymin = 30, ymax = 73)
ggplot() + geom_sf(data = europe_cropped)   
ggplot() + geom_sf(data = europe_cropped)   + coord_sf(expand = FALSE) # expend to fill

#  coord_sf() for (xlim vector with minimum and maximum) and latitudes (ylim vector) like st_crop():
ggplot() + geom_sf(data = worldmap) +
          coord_sf(xlim = c(-20, 45), ylim = c(30, 73), expand = FALSE) 


ggplot() + geom_sf(data = worldmap)  +   
          coord_sf(crs = st_crs('+proj=moll'),  xlim = c(-20, 45), ylim = c(30, 73), expand = FALSE)  ## Mollweide projection 
          # ???  bcuse  xlim and ylim as WGS84 coordinates but  same CRS as the target projection

# At first, we set the target CRS and transform the whole worldmap dataset to that CRS.
target_crs <- '+proj=moll'
worldmap_trans <- st_transform(worldmap, crs = target_crs)

# two points POINT (-20 30), POINT (45 73)
disp_win_wgs84 <- st_sfc(st_point(c(-20, 30)), st_point(c(45, 73)), crs = 4326)
disp_win_wgs84

# similarly, these coordinates can be transformed to our target CRS via st_transform.
disp_win_trans <- st_transform( disp_win_wgs84,  crs = target_crs )
disp_win_trans

# pass them as limits for the x and y scale respectively.
# Note also that I set datum = target_crs which makes sure that the graticule is displayed in the target CRS. Otherwise ggplot2 by default displays a WGS84 graticule.

disp_win_coord <- st_coordinates(disp_win_trans)
disp_win_coord

ggplot() + geom_sf(data = worldmap_trans) +
          coord_sf( xlim = disp_win_coord[,'X'],  ylim = disp_win_coord[,'Y'],
                    datum = target_crs,   expand = FALSE  ) 

# with this transformation, display window is slightly different than the one specified in WGS84 coordinates

# Instead of specifying a rectangular region of interest, I found it more comfortable to set a center point of interest and specify a “zoom level”
#  zoom level 1/4 of earth, every increase devide earth by factor 4 ( = magnify by 4) 360 / 4 to poer zoom level

####  set a center point of interest and specify a “zoom level” 
zoom_to <- c(13.38, 52.52)  # Berlin
zoom_level <- 3

lon_span <- 360 / 2^zoom_level  #  lon_span <- 360 / 2^zoom_level , for zoom level 3, 2^3 = 8
lat_span <- 180 / 2^zoom_level  #  lat_span <- 180 / 2^zoom_level

# Now we can calculate the longitude and latitude ranges for the the display window
lon_bounds <- c( zoom_to[1] - lon_span / 2,    zoom_to[1] + lon_span / 2 )
lat_bounds <- c( zoom_to[2] - lat_span / 2,    zoom_to[2] + lat_span / 2 )

# now plot
ggplot()    +   geom_sf(data = worldmap) +
                geom_sf(data = st_sfc(st_point(zoom_to), crs = 4326),  color = 'red') +
                coord_sf(xlim = lon_bounds, ylim = lat_bounds) 

# another projection?  use a Lambert azimuthal equal-area projection (LAEA) 
# Kamchatka at the center of our map, set a zoom level

zoom_to <- c(159.21, 56.41)  # ~ center of Kamchatka
zoom_level <- 2.5

target_crs <- sprintf('+proj=laea +lon_0=%f +lat_0=%f',    zoom_to[1], zoom_to[2])
# LAEA projection uses meters as unit instead of degrees. 
# so instead of dividing 360°, full circumference in meters C = 40,075,016m is divided by 2^z  
# so for x above, and for y-axis, so we get C / 2^(z+1).
C <- 40075016.686   # ~ circumference of Earth in meters
x_span <- C / 2^zoom_level
y_span <- C / 2^(zoom_level+1)

# calculate the lower left and upper right corner  by subtracting/adding half of x_span and y_span from/to the zoom center coordinate as before
# Also transform the centre point also in new projection

zoom_to_xy <- st_transform( st_sfc( st_point(zoom_to), crs = 4326 ),  crs = target_crs)
zoom_to_xy

disp_window <- st_sfc(  st_point(st_coordinates(zoom_to_xy - c(x_span / 2, y_span / 2))),
                        st_point(st_coordinates(zoom_to_xy + c(x_span / 2, y_span / 2))),
                        crs = target_crs              )
# plot
ggplot() +  geom_sf(data = worldmap) +
            geom_sf(data = zoom_to_xy, color = 'red') +
            coord_sf( xlim = st_coordinates(disp_window)[,'X'],
                      ylim = st_coordinates(disp_window)[,'Y'],
                      crs = target_crs, datum = target_crs       ) 

# Expriment. some combinations of zoom levels and projections will cause  errors, since some projection are only specified within certain bounds.
# source code https://gist.github.com/internaut/d6ede8d11a9078bbe1704083e196354d


#################
# Plot a network graph of nodes with geographic coordinates on a map.
# This script shows three ways of plotting a network graph on a map.
library(assertthat)
library(dplyr)
library(purrr)
library(igraph)
library(ggplot2)
library(ggraph)
library(ggmap)
set.seed(123)

N_EDGES_PER_NODE_MIN <- 1
N_EDGES_PER_NODE_MAX <- 4
N_CATEGORIES <- 4

country_coords_txt <- "
1     3.00000  28.00000       Algeria
2    54.00000  24.00000           UAE
3   139.75309  35.68536         Japan
4    45.00000  25.00000 'Saudi Arabia'
5    9.00000   34.00000       Tunisia
6     5.75000  52.50000   Netherlands
7   103.80000   1.36667     Singapore
8   124.10000  -8.36667         Korea
9    -2.69531  54.75844            UK
10    34.91155  39.05901        Turkey
11  -113.64258  60.10867        Canada
12    77.00000  20.00000         India
13    25.00000  46.00000       Romania
14   135.00000 -25.00000     Australia
15    10.00000  62.00000        Norway "

country_coords_txt  # nodes come from the above table and contain geo-coordinates for some randomly countries
nodes <- read.delim(text = country_coords_txt,  quote = "'", sep = "", header = FALSE,
                    col.names = c('id', 'lon', 'lat', 'name'))
nodes
# edges: create random connections between countries (nodes)
edges <- map_dfr(nodes$id, function(id) {
                              n <- floor(runif(1, N_EDGES_PER_NODE_MIN, N_EDGES_PER_NODE_MAX+1))
                             to <- sample(1:max(nodes$id), n, replace = FALSE)
                             to <- to[to != id]
                     categories <- sample(1:N_CATEGORIES, length(to), replace = TRUE)
                        weights <- runif( length(to) )
                   data_frame(from = id, to = to, weight = weights, category = categories)
                                        }
                 )

edges <- edges %>% mutate(category = as.factor(category))
edges
# create the igraph graph object

##????????? which fn from data

g <- graph_from_data_frame(edges, directed = F, vertices = nodes)

# create a data frame for plotting the edges, join with nodes to get start and end positions for each
# edge (x, y and xend, yend)

edges_for_plot <- edges %>%
    inner_join(nodes %>% select(id, lon, lat), by = c('from' = 'id')) %>%
    rename(x = lon, y = lat) %>%
    inner_join(nodes %>% select(id, lon, lat), by = c('to' = 'id')) %>%
    rename(xend = lon, yend = lat)

assert_that(nrow(edges_for_plot) == nrow(edges))

# use the node degree for scaling the node sizes
nodes$weight = degree(g)

# common plot theme
maptheme <- theme(panel.grid = element_blank()) +
    theme(axis.text = element_blank()) +
    theme(axis.ticks = element_blank()) +
    theme(axis.title = element_blank()) +
    theme(legend.position = "bottom") +
    theme(panel.grid = element_blank()) +
    theme(panel.background = element_rect(fill = "#596673")) +
    theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))

# common polygon geom for plotting the country shapes
country_shapes <- geom_polygon(data = map_data('world'), aes(x = long, y = lat, group = group),
                               fill = "#CECECE", color = "#515151", size = 0.15)
# common coordinate system for all the following plots
mapcoords <- coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))

# ------------------------------- #
# Solution 1: ggplot + ggmap only #
# ------------------------------- #

# try to plot with scaled edge widths and node sizes
# this will fail because we can only use the "size" aesthetic twice

ggplot(nodes) + country_shapes +
    geom_curve(aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                   color = category, size = weight),
               data = edges_for_plot, curvature = 0.33, alpha = 0.5) +
    scale_size_continuous(guide = FALSE, range = c(0.25, 2)) + # scale for edge widths
    geom_point(aes(x = lon, y = lat, size = weight),           # draw nodes
               shape = 21,   
               fill = 'white', color = 'black', stroke = 0.5) +
    scale_size_continuous(guide = FALSE, range = c(1, 6)) +    # scale for node size
    geom_text(aes(x = lon, y = lat, label = name),             # draw text labels
              hjust = 0, nudge_x = 1, nudge_y = 4,
              size = 3, color = "white", fontface = "bold") +
    mapcoords + maptheme

# Results in warning: "Scale for 'size' is already present. Adding another scale for
# 'size', which will replace the existing scale."

# now a plot with static node size:

ggplot(nodes) + country_shapes +
    geom_curve(aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                   color = category, size = weight),
               data = edges_for_plot, curvature = 0.33, alpha = 0.5) +
    scale_size_continuous(guide = FALSE, range = c(0.25, 2)) + # scale for edge widths
    geom_point(aes(x = lon, y = lat),                          # draw nodes
               shape = 21, size = 3,
               fill = 'white', color = 'black', stroke = 0.5) +
    geom_text(aes(x = lon, y = lat, label = name),             # draw text labels
              hjust = 0, nudge_x = 1, nudge_y = 4,
              size = 3, color = "white", fontface = "bold") +
    mapcoords + maptheme


# ------------------------------------ #
# Solution 2: ggplot2 + ggmap + ggraph #
# ------------------------------------ #

# prepare layout: use "manual" layout with geo-coordinates
node_pos <- nodes %>% select(lon, lat) %>% rename(x = lon, y = lat)
lay <- create_layout(g, 'manual', node.positions = node_pos)
assert_that(nrow(lay) == nrow(nodes))

# use the node degree for scaling the node sizes
lay$weight <- degree(g)

ggraph(lay) + country_shapes +
    geom_edge_arc(aes(color = category, edge_width = weight,   # draw edges as arcs
                      circular = FALSE),
                  data = edges_for_plot, curvature = 0.33, alpha = 0.5) +
    scale_edge_width_continuous(range = c(0.5, 2),             # scale for edge widths
                                guide = FALSE) +
    geom_node_point(aes(size = weight), shape = 21,            # draw nodes
                    fill = "white", color = "black",
                    stroke = 0.5) +
    scale_size_continuous(range = c(1, 6), guide = FALSE) +    # scale for node widths
    geom_node_text(aes(label = name), repel = TRUE, size = 3,
                   color = "white", fontface = "bold") +
    mapcoords + maptheme


# --------------------------------------------------------------- #
# Solution 3: the hacky way (overlay several ggplot "plot grobs")
# --------------------------------------------------------------- #

theme_transp_overlay <- theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
)

# the base plot showing only the world map
p_base <- ggplot() + country_shapes + mapcoords + maptheme

# first overlay: edges as arcs
p_edges <- ggplot(edges_for_plot) +
    geom_curve(aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                   color = category, size = weight),
               curvature = 0.33, alpha = 0.5) +
    scale_size_continuous(guide = FALSE, range = c(0.5, 2)) +  # scale for edge widths
    mapcoords + maptheme + theme_transp_overlay +
    theme(legend.position = c(0.5, -0.1), legend.direction = "horizontal")

# second overlay: nodes as points
p_nodes <- ggplot(nodes) +
    geom_point(aes(x = lon, y = lat, size = weight),
               shape = 21, fill = "white", color = "black",    # draw nodes
               stroke = 0.5) +
    scale_size_continuous(guide = FALSE, range = c(1, 6)) +    # scale for node size
    geom_text(aes(x = lon, y = lat, label = name),             # draw text labels
              hjust = 0, nudge_x = 1, nudge_y = 4,
              size = 3, color = "white", fontface = "bold") +
    mapcoords + maptheme + theme_transp_overlay

# combine the overlays to a full plot

# proper positioning of the grobs can be tedious... I found that
# using `ymin` works quite well but manual tweeking of the
# parameter seems necessary

p <- p_base +
    annotation_custom(ggplotGrob(p_edges), ymin = -74) +
    annotation_custom(ggplotGrob(p_nodes), ymin = -74)

print(p)
# from source https://gist.githubusercontent.com/internaut/a9a274c72181eaa7f5c3ab3a5f54b996/raw/13de5e0a1a3f578d530bb80a430fdbca7884a85a/networkmap.R
#################################



