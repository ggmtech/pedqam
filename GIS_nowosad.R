# GIS book
# geo raster data model has raster header and a matrix (rows/columns) of equally spaced cells / pixels;
# raster header defines crs, extent and origin (or starting point frequently lower-left corner.
# matrix and map algebra makes raster processing efficient and faster than vector data processing.
# terra package create, read, export, manipulate and process raster datasets
# Simple features widely used data model structures including in QGIS and PostGIS. cross-transferable
# sf objects can be treated as data frames in most operations and fn starts with st_**
# Simple features is data frames with a spatial extension: class(lnd_sf) # "sf"   "data.frame"
# sf enables the full power of R’s data analysis capabilities to be unleashed on geographic data
# install.packages("sf")
# install.packages("terra")
# install.packages("spData")
# spDataLarge has few raster and one vector of Zion National Park (Utah, USA)  srtm.tif is a digital elevation model 
# install.packages("spDataLarge", repos = "https://nowosad.r-universe.dev")

# All the packages needed for  book can be installed with the following command: 
# remotes::install_github("geocompr/geocompkg"). 

# spatial element "geom or geometry" added as ‘list columns’ of class sfc "simple feature column"
#  In turn, sfc objects  composed of one or more objects of class sfg: "simple feature geometries"
library(stringr) # for working with strings (pattern matching)
library(tidyr)   # for unite() and separate()
library(sf) # classes and functions for vector data #> Linking to GEOS 3.9.0, GDAL 3.2.1, PROJ 7.2.1
library(terra)      # classes and functions for raster data

library(spData)        # load geographic data
library(spDataLarge)   # load larger geographic data

library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package


# sf - unified interface to GEOS lib for geometry operations, GDAL lib for read/write geo data files, and the PROJ lib for representing and transforming projected CRS.  Details
vignette(package = "sf") # see which vignettes are available
vignette("sf1")          # an introduction to the package
methods(class = "sf") # methods for sf objects, first 12 shown


# sf objects stored in df, with geo data occupying a special column, usually named ‘geom’ or ‘geometry’. 
# use world dataset provided by  spData. see nowosad.github.io/spData for list of datasets 
#  world is a spatial object containing spatial and attribute columns, the names returned ny names() 
world %>% View()
names(world)

# The geom column give sf objects their spatial powers: world$geom is ‘list column’ that contains all the coordinates of the country polygons. # The sf has plot() for visualizing 
plot(world)   # multiple maps, one for each variable in the world datasets, non interactive

# GIS in dataframe by sf has advantage eg summary(world["lifeExp"]). geometry retained unless removed
summary(world["lifeExp"])  # result provides a quick summary of both the non-spatial and spatial datas

# MULTIPOLYGON  geometry type of features (countries) necessary for countries with islands etc
# sf easy to subset. 
# two major diff from regular df is geometry column and geographic data (geometry type, dimension, bbox and CRS information - epsg (SRID), proj4string)

world_mini = world[1:2, 1:3]
world_mini


# many poackge tmap, mapview and tidycensus started adding support for sf. Others used by converting
library(sp) # sp functions ...
world_sp = as(world, Class = "Spatial")
# converted back to sf in the same way or with st_as_sf():
world_sf = st_as_sf(world_sp)

# Basic maps with plot() as multi-panel plot (like sp’s spplot()) for each variable of the object
# legend or ‘key’ with a continuous color is produced if the object to be plotted has a single variable
# Colors set with col =, although this will not create a continuous palette or a legend.
# Plots are added as layers to existing images by setting add = TRUE
# 

plot(world[3:6])
plot(world["pop"])

world_asia = world[world$continent == "Asia", ]
asia = st_union(world_asia)

# plot the Asian continent over a map of the world using add = TRUE. first plot must only have one facet.
# If the first plot has a key, reset = FALSE must be used (result not shown):
  
plot(world["pop"], reset = FALSE)
plot(asia, add = TRUE, col = "red")  # Adding layers 

# plot() fast but does not create interactive maps with options. For advanced use  tmap
# Base plot arguments
plot(world["continent"], reset = FALSE,  main = "my map title")

cex = sqrt(world$pop) / 10000
world_cents = st_centroid(world, of_largest = TRUE)
plot(st_geometry(world_cents), add = TRUE, cex = cex,)

# Projected version
#library(sf)
library(spData)
world_proj  = st_transform(world,  "+proj=eck4" )

world_cents = st_centroid(world_proj, of_largest_polygon = TRUE)
par(mar = c(0, 0, 0, 0))
# plot(st_geometry(world), graticule = TRUE, reset = FALSE)
plot(world_proj["continent"], reset = FALSE, main = "", key.pos = NULL)

g = st_graticule()
g = st_transform(g, crs = "+proj=eck4")
plot(g$geometry, add = TRUE, col = "lightgrey")

cex = sqrt(world$pop) / 10000
plot(st_geometry(world_cents), add = TRUE, cex = cex, lwd = 2, graticule = T)
# #  st_centroid() convert one geometry type (polygons) to another (points),   aesthetics of which are varied with the cex argument

# sf plot argument expandBB to plot an sf object in context: bounding box - bottom, left, top, right. 
# eg plot India in the context of its giant Asian neighbors, with an emphasis on China to the east,

india = world[world$name_long == "India", ]

plot(st_geometry(india), expandBB = c(0, 0.2, 0.1, 1), col = "blue", lwd = 3)
plot(world_asia[0], add = TRUE) # Note use of [0] to keep only the geometry col and lwd to emphasize India.


# sf 17 geometry types, common 7 : POINT, LINESTRING, POLYGON, 3 MULTI versions and GEOMETRYCOLLECTION
# well-known binary (WKB) or well-known text (WKT) are standard encoding for sf geometries.
# POINT (5 2), LINESTRING (1 5, 4 4, 4 1, 2 2, 3 2), POLYGON ((1 5, 2 2, 4 1, 4 4, 1 5)) ( without hole)
# MULTIPOINT (5 2, 1 3, 3 4, 3 2)  , 
# MULTILINESTRING ((1 5, 4 4, 4 1, 2 2, 3 2), (1 2, 2 4))
# MULTIPOLYGON (((1 5, 2 2, 4 1, 4 4, 1 5), (0 2, 1 2, 1 3, 0 3, 0 2)))
# GEOMETRYCOLLECTION (MULTIPOINT (5 2, 1 3, 3 4, 3 2), LINESTRING (1 5, 4 4, 4 1, 2 2, 3 2))

# sfg class:  point, linestring, polygon, 3‘multi’ equivalents, geometry collection.
# if reqd sfg from scratch using functions as st_point(), st_linestring(), st_polygon()
# st_multipoint(),  st_multilinestring(), st_multipolygon(), st_geometrycollection()
# sfg objects can be created from three base R data types:
# A numeric vector: a single point
# A matrix: a set of points, where each row represents a point, a multipoint or linestring
# A list: a collection of objects such as matrices, multilinestrings or geometry collections
st_point(c(5, 2)) 
st_point(c(5, 2, 3))  # XYZ point

st_point(c(5, 2, 1), dim = "XYM") # XYM point # dimension
st_point(c(5, 2, 3, 1))           # XYZM point
#  XY (2D), XYZ (3D ) and XYZM (3D with an additional variable, typically measurement accuracy) 

# Use matrices in the case of multipoint (st_multipoint()) and linestring (st_linestring()) objects:
# the rbind function simplifies the creation of matrices
## MULTIPOINT
multipoint_matrix = rbind(c(5, 2), c(1, 3), c(3, 4), c(3, 2))
st_multipoint(multipoint_matrix)
## LINESTRING
linestring_matrix = rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2))
st_linestring(linestring_matrix)   #> LINESTRING (1 5, 4 4, 4 1, 2 2, 3 2)
# Finally, use lists for the creation of multilinestrings, (multi-)polygons and geometry collections:
## POLYGON
polygon_list = list( rbind( c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5)  )  )
st_polygon(polygon_list)  #> POLYGON ((1 5, 2 2, 4 1, 4 4, 1 5))
## POLYGON with a hole
polygon_border = rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5))
polygon_hole = rbind(c(2, 4), c(3, 4), c(3, 3), c(2, 3), c(2, 4))
polygon_with_hole_list = list(polygon_border, polygon_hole)
st_polygon(polygon_with_hole_list)
#> POLYGON ((1 5, 2 2, 4 1, 4 4, 1 5), (2 4, 3 4, 3 3, 2 3, 2 4))
## MULTILINESTRING
multilinestring_list = list(rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2)), 
                            rbind(c(1, 2), c(2, 4)) 
                            )
st_multilinestring((multilinestring_list))
#> MULTILINESTRING ((1 5, 4 4, 4 1, 2 2, 3 2), (1 2, 2 4))
## MULTIPOLYGON
multipolygon_list = list(list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5))),
                         list(rbind(c(0, 2), c(1, 2), c(1, 3), c(0, 3), c(0, 2))))
st_multipolygon(multipolygon_list)
#> MULTIPOLYGON (((1 5, 2 2, 4 1, 4 4, 1 5)), ((0 2, 1 2, 1 3, 0 3, 0 2)))
## GEOMETRYCOLLECTION
gemetrycollection_list = list(st_multipoint(multipoint_matrix),
                              st_linestring(linestring_matrix))
st_geometrycollection(gemetrycollection_list) 
#> GEOMETRYCOLLECTION (MULTIPOINT (5 2, 1 3, 3 4, 3 2),
#>   LINESTRING (1 5, 4 4, 4 1, 2 2, 3 2))

# sfc   : One sfg object contains only a single simple feature geometry. 
# sfc is a list of sfg objects. This additionally contains info about crs in use.
# st_sfc() combine two sf into one object with two features, important as sfc is geometry col in sf df.
  
# sfc POINT
point1 = st_point(c(5, 2))
point2 = st_point(c(1, 3))
points_sfc = st_sfc(point1, point2)
points_sfc
# mostly sfc object contains objects of the same geometry type. Therefore, when we convert sfg objects of type polygon into a simple feature geometry column, we would also end up with an sfc object of type polygon, which can be verified with st_geometry_type(). Equally, a geometry column of multilinestrings would result in an sfc object of type multilinestring:
  
# sfc POLYGON
polygon_list1 = list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5)))
polygon1 = st_polygon(polygon_list1)
polygon_list2 = list(rbind(c(0, 2), c(1, 2), c(1, 3), c(0, 3), c(0, 2)))
polygon2 = st_polygon(polygon_list2)
polygon_sfc = st_sfc(polygon1, polygon2)
st_geometry_type(polygon_sfc)

multilinestring_list1 = list(rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2)), 
                             rbind(c(1, 2), c(2, 4)))
multilinestring1 = st_multilinestring((multilinestring_list1))
multilinestring_list2 = list(rbind(c(2, 9), c(7, 9), c(5, 6), c(4, 7), c(2, 7)), 
                             rbind(c(1, 7), c(3, 8)))
multilinestring2 = st_multilinestring((multilinestring_list2))
multilinestring_sfc = st_sfc(multilinestring1, multilinestring2)
st_geometry_type(multilinestring_sfc)

# Also possible to create an sfc object from sfg objects with different geometry types:
# sfc GEOMETRY
point_multilinestring_sfc = st_sfc(point1, multilinestring1)
st_geometry_type(point_multilinestring_sfc)
# note sfc objects can additionally store information on CRS. 
# To specify a certain CRS, use the epsg (SRID) or proj4string attributes of an sfc object. 
# The default value of epsg (SRID) and proj4string is NA (Not Available), se st_crs():
st_crs(points_sfc)

# All geometries in an sfc object must have the same CRS. 
# We can add crs as a crs argument of st_sfc(). accepts an integer epsg code like 4326, which automatically adds the ‘proj4string’ (see Section 2.4):
  
# EPSG definition
points_sfc_wgs = st_sfc(point1, point2, crs = 4326)
st_crs(points_sfc_wgs)

# It also accepts a raw proj4string (result not shown):
# PROJ4STRING definition
st_sfc(point1, point2, crs = "+proj=longlat +datum=WGS84 +no_defs")

# when st_crs() only return a proj4string but not an epsg code as no method from proj4string to epsg 


# SF with other non-geog attributes, representing name or other attributes as measured values, groups
# 
# eg temperature of 25°C in London on June 21st, 2017 has geometry (coordinates), and three attributes with three different classes (place name, temperature and date).
# The 11 Objects of class sf represent such data by combining the attributes (data.frame) with the simple feature geometry column (sfc). They are created with st_sf() 
lnd_point = st_point(c(0.1, 51.5))                 # sfg object
lnd_point
lnd_geom = st_sfc(lnd_point, crs = 4326)           # sfc object
lnd_geom

lnd_attrib = data.frame(                           # data.frame object
                        name = "London",
                        temperature = 25,
                        date = as.Date("2017-06-21")
                        )
lnd_sf = st_sf(lnd_attrib, geometry = lnd_geom)    # sf object
lnd_sf
class(lnd_sf)   # "sf"         "data.frame"



# TERRA
# Rester datasets from the spDataLarge. few raster objects and one vector object of  Zion National Park # srtm.tif is a digital elevation model 

# SpatRaster  my_rast:
raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
my_rast = rast(raster_filepath)
my_rast   # Dedicated functions: 
dim(my_rast) # returns the number of rows, columns and layers; 
ncell(my_rast)      # the number of cells (pixels); res() the spatial resolution; 
ext(my_rast)        # its spatial extent; and 
crs(my_rast)        # crs 
help("terra-package") # returns a full list of all available terra functions.

# terra also provides plot() methods for its own classes.
plot(my_rast) # The plot was big size so check par("mar") # To change that write:
par(mar=c(1,1,1,1))
dev.off()  # To restart R graphics # graphics.off() par("mar") par(mar=c(1,1,1,1))

# Other way to plot: 
# tmap to create static and interactive maps of raster and vector objects 
# rasterVis::levelplot() #from the rasterVis package, to create facets ef for visualizing change over time
# 
# SpatRaster class for rasters object in terra. 
# read-in a raster file from disk or from a server 
single_raster_file = system.file("raster/srtm.tif", package = "spDataLarge")
single_rast = rast(raster_filepath)

# Terra reads file header and a pointer to the file itself and only part Rasters into RAM
# rast() # create rester from scratch 
new_raster = rast(nrows = 6, ncols = 6, resolution = 0.5, 
                  xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
                  vals = 1:36)

new_raster
# The resulting raster consists of 36 cells (6 columns and 6 rows specified by nrows and ncols) 
# centered around the Prime Meridian and the Equator (see xmin, xmax, ymin and ymax parameters). 
# CRS default is WGS84,change with crs argument. resolution is in degrees set to 0.5 (resolution). 
# Values (vals) are assigned to each cell: 1 to cell 1, 2 to cell 2, and so on. 
# Remember: rast() fills cells row-wise (unlike matrix()) starting at the upper left corner, meaning the top row contains the values 1 to 6, the second 7 to 12, etc
# 
# SpatRaster class also handles multiple layers, typically correspond to a multispectral satellite file or a ts.

multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge")
multi_rast = rast(multi_raster_file)
multi_rast                  

nlyr(multi_rast)  # nlyr() retrieves the number of layers stored in a SpatRaster object:             
                  


# Coordinate Reference System (CRS) # spatial data relationship to Earth surface: geographic / projected
# Geographic crs identify location on  Earth’s surface with longitude and latitude , degrees
# Earth geocrs as spherical or ellipsoidal surface as Earth equatorial radius +11.5 km +polar radius

# DATUM : Ellipsoids subset of CRSs: the datum. 
# DATUM : what ellipsoid to use and precise relationship between Cartesian coordinates and Earth’s surface. 
#
# geocentric datum eg WGS84 : Earth’s center of gravity is centre and proj accuracy not optimized for a location
# local datum      eg NAD83 : ellipsoidal surface shifted to align with surface at a particular location

# Projected CRS
# Projected CRS: based on Cartesian coord on implicitly flat surface: origin, x and y axes, and linear measure
# All projected CRSs are based on a geographic CRS and rely on map projections to convert the 3D surface of Earth into Easting and Northing (x and y) values in a projected CRS.

# This transition adds deformations in area, direction, distance, and shape.  Projection preservs one or two.
# Projections often named on a property preserved: 
# equal-area  # preserves area, 
# azimuthal   # preserve direction, 
# equidistant # preserve distance,
# conformal   # preserve local shape confirmance.
#
# 3 main projection types - conic, cylindrical, and planar. 
# conic projection  # the Earth’s surface is projected onto a cone along a one or two lines of tangency. Distortions minimized along tangency lines and increase with distance. suits  maps of mid-latitude areas. 
# cylindrical projection # maps the surface onto a cylinder. This projection could also be created by touching the Earth’s surface along a single line of tangency or two lines of tangency. Cylindrical projections are used most often when mapping the entire world. 
# planar projection projects data onto a flat surface touching the globe at a point or along a line of tangency. It is typically used in mapping polar regions. 

sf_proj_info(type = "proj") #gives a list of the available projections supported by the PROJ library.

# CRS Spatial Reference System Identifier (SRID) 
# WKT2 
# epsg code shorter single well-defined coordinate reference system.
st_crs()
vector_filepath = system.file("vector/zion.gpkg", package = "spDataLarge")
new_vector = st_read(vector_filepath)
?zion

st_crs(new_vector) # get CRS
# st_set_crs() to set crs when a CRS is missing or the wrong CRS 
new_vector = st_set_crs(new_vector, "EPSG:26912") # set CRS
crs(my_rast) # get CRS
# The same function, crs(), is used to set a CRS for raster objects.
my_rast2 = my_rast
crs(my_rast2) = "EPSG:26912" # set CRS

# st_crs() and crs() functions do not alter coordinates’ values or geometries. only  set  metadata information
# 
# 
# CRSs is that they contain information about spatial units.
# sf objects is that they have native support for units.
# distance, area and other geometric calculations in sf return values with units attribute, as in units package 
luxembourg = world[world$name_long == "Luxembourg", ]
st_area(luxembourg) ##> 2.41e+09 [m^2]
# information, stored as an attribute (which interested readers can discover with 
attributes(st_area(luxembourg))
# take care when changing per km2 as wrongly reports 2408.817 [m^2] instead of km2
st_area(luxembourg) / 1000000
units::set_units(st_area(luxembourg), km^2)  # set right the units now


res(my_rast)  #> [1] 0.000833 0.000833 cf they are in degrees !
# If used the UTM projection, the units would change.
repr = project(my_rast, "EPSG:26912")
res(repr) # command gives back a numeric vector without any unit, forcing us to know unit of UTM proj is meters.

methods(class = "sf") # methods for sf objects, first 12 shown


#############################
#############################
dim(world) # it is a 2 dimensional object, with rows and columns   [1] 177  11
nrow(world) # how many rows?
ncol(world) # how many columns?
# ten non-geographic columns (and one geometry list column) with almost 200 rows representing the world’s countries
# Extracting the attribute data of an sf object is the same as removing its geometry:
world_df = st_drop_geometry(world)
class(world_df)

#Base R subsetting functions
#  [, subset() and $. dplyr subsetting functions include select(), filter(), and pull()
world[, 1:3] # subset columns by position
world[, c("name_long", "lifeExp")] # subset columns by name


#small_countries, containing nations whose surface area is smaller than 10,000 km2:
  
sel_area = world$area_km2 < 10000
summary(sel_area) # a logical vector
small_countries = world[sel_area,     ]
# or  crisper
small_countries =        world[world$area_km2 < 10000, ]
small_countries = subset(world, area_km2 < 10000)

small_countries
# also dplyr
world1 = dplyr::select(world, name_long, pop)
names(world1)

# all columns between name_long and pop (inclusive)
world2 = dplyr::select(world, name_long:pop)

# all columns except subregion and area_km2 (inclusive)
world3 = dplyr::select(world, -subregion, -area_km2)
# use select() lets you subset and rename columns at the same time, for example:
world4 = dplyr::select(world, name_long, population = pop)
names(world4)

world5 = world[, c("name_long", "pop")] # subset columns by name
names(world5)[names(world5) == "pop"] = "population" # rename column manually




