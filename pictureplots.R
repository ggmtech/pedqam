
####
#remotes::install_github("rstudio/webshot2")
library(webshot2)

# Single page
webshot("https://www.r-project.org")
webshot("https://www.rdso.indianrailways.gov.in")
# Multiple pages (in parallel!)
webshot(c("https://www.r-project.org", "https://www.rstudio.com"))

# Specific height and width
webshot("https://www.r-project.org", vwidth = 1600, vheight = 900, cliprect = "viewport")

#####
# Tools for creating, manipulating, and writing HTML from R.
install.packages("htmltools")  # or remotes::install_github("rstudio/htmltools")


#######
remotes::install_github('rstudio/rmarkdown')


# bubble charts
library(ggplot2)
theme_set( theme_bw()  )
#data("mtcars")
df <- mtcars
df$cyl <- as.factor(df$cyl)

df  %>% 
    ggplot(     aes(x = wt, y = disp)   ) +
    geom_point( aes(color = cyl, size = qsec), alpha = 0.5 ) +
    scale_color_manual(values = c("#AA4371", "#E7B800", "#FC4E07")) +
    scale_size(range = c( 1,  13 )   ) + # Adjust the range of points size
    theme_set(theme_bw() +theme(legend.position = "bottom"))

#Method 2:-
library(plotly)
df %>% 
              plot_ly(x       = ~wt,  
                      y       = ~disp,
                      text    = ~cyl, size = ~qsec,
                      color   = ~cyl, sizes = c(10, 50),
                      marker  = list(opacity = 0.7, 
                      sizemode = "diameter") 
                    )    # %>%   layout

# 'Highcharts' <http://www.highcharts.com/> is a charting library
# Highcharter is a R wrapper for Highcharts javascript library and its modules. 
# Highcharts is very flexible and customizable javascript charting library and 
# it has a great and powerful API.
# Chart various R objects with one function: 
# hchart(x)  can chart data.frames, numeric or character vectors, ts, xts, forecast, survfit objects.
# support Highstock , candlestick charts in 2 lines of code. 
# Support Highmaps Create choropleth charts or add information in geojson format.
# Themes: you configure your chart in multiples ways. There are implemented themes like economist, financial times, google, 538 among others.
# A lot of features and plugins: motion, draggable points, font-awesome, tooltips, annotations.
#remotes::install_github("jbkunst/highcharter")
library(highcharter)
# see https://jkunst.com/highcharter/articles/highcharts.html
vignette("highcharter") #explore the basics of the package.
vignette("showcase") #to see how much highcharts is flexible in terms of customization and design.
vignette("highcharts-api") #show the main functions to configure charts with the implemented Highcharts API.
vignette("highchartsjs-api-basics") #explain the relationship between the highchartsJS API and highcharter.

# This 3 functions are inspired in ggplot2 package. So:
# hchart() works like ggplot2’s qplot.
# hc_add_series() works like ggplot2’s geom_s.
# hcaes() works like ggplot2’s aes.

data("mpg", "diamonds", "economics_long", package = "ggplot2")
head(mpg)



mpg %>% hchart(  "point",  hcaes(x = displ, y = cty, group = year)   )

x <- diamonds$price   # Numeric & Histograms
x %>% hchart()      #  Numeric & Histograms

# Densities
hchart(density(x), type = "area", color = "#B71C1C", name = "Price")

# Character & Factor
x <- diamonds$cut
hchart(x, type = "column")

# Time Series
hchart(LakeHuron, name = "Level") %>%   hc_title(text = "Level of Lake Huron 1875–1972")

# Seasonal Decomposition of Time Series by Loess
x <- stl(log(AirPassengers), "per")
hchart(x)

# Forecast package
library(forecast)
x <- forecast(ets(USAccDeaths), h = 48, level = 95)
hchart(x)

# Survival analysis
library(survival)
data(cancer, package = "survival")
lung <- dplyr::mutate(cancer, sex = ifelse(sex == 1, "Male", "Female"))

fit <- survfit(Surv(time, status) ~ sex, data = cancer)

hchart(fit, ranges = TRUE)

library(quantmod)
x <- getSymbols("GOOG", auto.assign = FALSE)
hchart(x)

# Multivariate Time series
x <- cbind(mdeaths, fdeaths)
hchart(x)


# Autocovariance & Autocorrelation
x <- acf(diff(AirPassengers), plot = FALSE)
hchart(x)

# Principal Components
hchart(princomp(USArrests, cor = TRUE))

# Matrix
data(volcano)
hchart(volcano)  %>%    
    hc_colorAxis(stops = color_stops(   # changing default color from bluish to valcano redish)
        colors = c("#000004FF", "#56106EFF", "#BB3754FF", "#F98C0AFF", "#FCFFA4FF"))  )

# Distance matrix
mtcars2 <- mtcars[1:20, ]
x       <- dist(mtcars2)
hchart(x)

#Correlation matrix
hchart(cor(mtcars))

# 
library(highcharter)
library(dplyr)
# remotes::install_github("allisonhorst/palmerpenguins")
data(penguins,                 package = "palmerpenguins")
data(diamonds, economics_long, package = "ggplot2")

hchart(penguins, "scatter", hcaes(x = body_mass_g, y = flipper_length_mm , group = species))
penguins2 <- penguins %>% count(species, island) %>%  glimpse()
hchart(penguins2, "column", hcaes(x = island, y = n, group = species))

economics_long2 <- economics_long %>%  filter(variable %in% c("pop", "uempmed", "unemploy"))
hchart(economics_long2, "line", hcaes(x = date, y = value01, group = variable))

#####
economics_long2 <- dplyr::filter(economics_long, variable %in% c("pop", "uempmed", "unemploy")) # data( "economics_long", package = "ggplot2")
hchart(economics_long2, "line", hcaes(x = date, y = value01, group = variable))

# Heatmap
dfdiam <- diamonds %>%   
          group_by(cut, clarity) %>%
          summarize(price = median(price))

hchart(dfdiam, "heatmap", hcaes(x = cut, y = clarity, value = price), name = "Median Price") 

mpgman <- mpg %>% 
         group_by(manufacturer) %>% 
         summarise(n = n(),  unique = length(unique(model))  ) %>% 
         arrange(-n, -unique)
# heat treemap
mpgman %>% hchart( "treemap", hcaes(x = manufacturer, value = n, color = unique)   )

# add prameters
mpgman2 <- count(mpg, manufacturer, year)

mpgman2 %>% hchart(  "bar",
    hcaes(x = manufacturer, y = n, group = year),
    color = c("#7CB5EC", "#F7A35C"),
    name = c("Year 1999", "Year 2008"),
    showInLegend = c(TRUE, FALSE) # only show the first one in the legend
   )


#Check automatically if the x column is date class:
economics_long2 <- economics_long %>% filter(variable %in% c("pop", "uempmed", "unemploy"))
hchart(economics_long2, "line", hcaes(x = date, y = value01, group = variable)   )


# library(highcharter)
# The highcharter package include highmaps libraries from highchartsJS to chart maps, choropleths and geojson.

hcmap("countries/nz/nz-all")
hcmap("countries/in/in-all")  # India with cut kashmir
hcmap("countries/in/in-all")  # India with cut kashmir

hcmap("countries/us/us-ca-all") %>%
    hc_title(text = "California") %>% 
    hc_subtitle(text = "You can use the same functions to modify your map!")

# Choropleths
download_map_data()# : Download the geojson data from the highchartsJS collection.
get_data_from_map()#: Get the properties for each region in the map, as the keys from the map data.

mapdata <- get_data_from_map(download_map_data("custom/usa-and-canada"))
glimpse(mapdata)
data_fake <- mapdata %>%  select(code = `hc-a2`) %>% mutate(value = 1e5 * abs(rt(nrow(.), df = 10)))
glimpse(data_fake)

hcmap(
   # "custom/usa-and-canada",
   "countries/in/in-all",  # wow good !
    data = data_fake,
    value = "value",
    joinBy = c("hc-a2", "code"),
    name = "Fake data",
    dataLabels = list(enabled = TRUE, format = "{point.name}"),
    borderColor = "#FAFAFA",
    borderWidth = 0.1,
    tooltip = list( valueDecimals = 2, valuePrefix = "$", valueSuffix = "USD" )
)

# Adding Points

cities <- data.frame(
    name = c("London", "Birmingham", "Glasgow", "Liverpool"),
    lat = c(51.507222, 52.483056, 55.858, 53.4),
    lon = c(-0.1275, -1.893611, -4.259, -3),
    z = c(1, 2, 3, 2)
)

hcmap("countries/gb/gb-all", showInLegend = FALSE) %>%
    hc_add_series(
        data = cities, 
        type = "mappoint",
        name = "Cities", 
        minSize = "1%",
        maxSize = "5%"  ) %>%
    hc_mapNavigation(enabled = TRUE)

hcmap("countries/gb/gb-all", showInLegend = FALSE) %>%
    hc_add_series(
        data = cities, 
        type = "mapbubble", # "mapbubble", "mappoint",
        name = "Cities", 
        minSize = "1%",
        maxSize = "5%" ) %>%
    hc_mapNavigation(enabled = TRUE)

##

airports <- read.csv( "https://raw.githubusercontent.com/ajdapretnar/datasets/master/data/global_airports.csv",
                      stringsAsFactors = FALSE )

south_america_countries <- c( "Brazil", "Ecuador", "Venezuela", "Chile", "Argentina", "Peru",
                              "Uruguay", "Paraguay", "Bolivia",  "Suriname", "Guyana", "Colombia" ) 
library(dplyr)


airports <- airports %>%
    filter(country %in% south_america_countries) %>%
    rename(lat = latitude, lon = longitude) %>%   # error
    filter(lon < -30)

hcmap(  "custom/south-america",    name = "South America",   showInLegend = FALSE ) %>%
    hc_add_series(
        data = airports,
        type = "mappoint",
        name = "Airports",
        color = hex_to_rgba("darkred", alpha = 0.3),
        maxSize = "10",
        tooltip = list( pointFormat = "{point.name}: {point.altitude:,.0f} feets <br>
                                      ({point.lat:,.2f}, {point.lon:,.2f})"  )   ) %>%
    hc_chart(zoomType = "xy")

# highchartsJS support geo_json classes from the geojsonio package.

library(httr)
library(jsonlite)
library(geojsonio)

ausgeojson <- GET("https://raw.githubusercontent.com/johan/world.geo.json/master/countries/AUS.geo.json") %>%
              content() %>%
              fromJSON(simplifyVector = FALSE) %>%
              as.json()

ausmap <-     highchart(type = "map") %>%
              hc_add_series(mapData = ausgeojson, showInLegend = FALSE)
ausmap
airports <- read.csv("https://raw.githubusercontent.com/ajdapretnar/datasets/master/data/global_airports.csv")
airports <- filter(airports, country == "Australia", name != "Roma Street Railway Station")

airp_geojson <- geojson_json(airports, lat = "latitude", lon = "longitude")
class(airp_geojson)

ausmap %>%   hc_add_series( data = airp_geojson,
                            type = "mappoint",
                            dataLabels = list(enabled = FALSE),
                            name = "Airports",
                            tooltip = list(pointFormat = "{point.name}")
                           )

# Advanced Example # Let’s download some geojson files and make a map.
getContent <- function(url) {  
                                library(httr)  
                                content( GET(url)  )   
                                }

world <- getContent("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json")
world <- jsonlite::fromJSON(world, simplifyVector = FALSE)  # is text

# http://cedeusdata.geosteiniger.cl/layers/geonode:mundo_corrientes_maritimas
marine <- getContent("http://cedeusdata.geosteiniger.cl/geoserver/wfs?srsName=EPSG%3A4326&typename=geonode%3Amundo_corrientes_maritimas&outputFormat=json&version=1.0.0&service=WFS&request=GetFeature")
# marine <- geojsonio::as.json(marine)

# http://cedeusdata.geosteiniger.cl/layers/geonode:mundo_limites_placas
plates <- getContent("http://cedeusdata.geosteiniger.cl/geoserver/wfs?srsName=EPSG%3A4326&typename=geonode%3Amundo_limites_placas&outputFormat=json&version=1.0.0&service=WFS&request=GetFeature")
# plates <- geojsonio::as.json(plates)

# http://cedeusdata.geosteiniger.cl/layers/geonode:mundo_volcanes
volcano <- getContent("http://cedeusdata.geosteiniger.cl/geoserver/wfs?srsName=EPSG%3A4326&typename=geonode%3Amundo_volcanes&outputFormat=json&version=1.0.0&service=WFS&request=GetFeature")
# volcano <- geojsonio::as.json(volcano)

# The data is ready. can keep using the rest of the API to customize your map.

highchart(type = "map") %>%
    hc_chart(backgroundColor = "#161C20") %>%
    hc_add_series(
        mapData = world,
        showInLegend = FALSE,
        nullColor = "#424242",
        borderWidth = 0
    ) %>%
    hc_add_series(
        data = marine,
        type = "mapline",
        geojson = TRUE,
        color = "#2980b9",
        name = "Marine currents",
        tooltip = list(pointFormat = "{point.properties.NOMBRE}")
    ) %>%
    hc_add_series(
        data = plates,
        type = "mapline",
        lineWidth = 2,
        zIndex = -1,
        geojson = TRUE,
        color = "#d35400",
        name = "Plates",
        tooltip = list(pointFormat = "{point.properties.TIPO}")
    ) %>%
    hc_add_series(
        data = volcano,
        type = "mappoint",
        color = hex_to_rgba("#f1c40f", 0.4),
        geojson = TRUE,
        name = "Volcanos",
        tooltip = list(pointFormat = "{point.properties.NOMBRE}"),
        marker = list(lineWidth = 0, radius = 2)
    )

###
library(quantmod)
x <- getSymbols("GOOG", auto.assign = FALSE)
hchart(x)
#Obviously you can use the implemented API functions to edit the chart:
y <- getSymbols("AMZN", auto.assign = FALSE)

hchart(y, type = "ohlc") %>% 
    hc_title(text = "This is a Open-high-low-close chart with a custom theme") %>% 
    hc_add_theme(hc_theme_db())


highchart(type = "stock") %>% 
    hc_add_series(x, id = 1) %>% 
    hc_add_series(y, type = "ohlc", id = 2)

# waffle 
library(dplyr) # to wokr with list columns
library(purrr) # to wokr with list columns

df2 <- tibble(
    type = c("fruit", "samminch", "pizza"),
    amount = c(46, 54, 80),
    faico = c("apple-alt", "bread-slice", "pizza-slice"),
    col = c("#d35400", "#907E4D", "#F89101")       )

df2
# install.packages("rsvg")
library(rsvg) # for fontawesome::fa_png
library(fontawesome)

fa_to_png_to_datauri <- function(name, ...) {
    tmpfl <- tempfile(fileext = ".png")
    fontawesome::fa_png(name, file = tmpfl, ...)
    knitr::image_uri(tmpfl) }
df2 <- df2 %>% 
      mutate(  uri = map2_chr(faico, col, ~fa_to_png_to_datauri(.x, fill = .y)),
            marker = map(uri, ~ list(symbol = str_glue("url({data_uri})", data_uri = .x))) )

hchart( df2, "item", hcaes(name = type, y = amount),  name = "What I eat",  showInLegend = TRUE ) %>% 
    hc_plotOptions( series = list(point = list(events = list(legendItemClick = JS("function(e) {e.preventDefault() }")))) ) %>% 
    hc_legend( labelFormat =  '{name} <span style="opacity: 0.4">{y}</span>') %>% 
    hc_colors(pull(df2, col))


#####

library(purrr)
library(dplyr)

library(idbr)
idbr::idb_api_key("35f116582d5a89d11a47c7ffbfc2ba309133f09d")
yrs <-  seq(1980, 2030, by = 5)

df <- map_df(  c("male", "female"), 
               function(sex){   mutate(  idbr::idb1("US", yrs,  sex = sex), 
                                         sex_label = sex)  }
               )
names(df) <- tolower(names(df))

df <- df %>%   mutate(population = pop*ifelse(sex_label == "male", -1, 1))

series <- df %>% 
    group_by(sex_label, age) %>% 
    do(data = list(sequence = .$population)) %>% 
    ungroup() %>% 
    group_by(sex_label) %>% 
    do(data = .$data) %>%
    mutate(name = sex_label) %>% 
    list_parse()

maxpop <- max(abs(df$population))

xaxis <- list(
    categories = sort(unique(df$age)),
    reversed = FALSE, tickInterval = 5,
    labels = list(step = 5)
)

highchart() %>%
    hc_chart(type = "bar") %>%
    hc_motion(
        enabled = TRUE, 
        labels = yrs, 
        series = c(0,1),
        autoplay = TRUE, 
        updateInterval = 10
    ) %>% 
    hc_add_series_list(series) %>% 
    hc_plotOptions(
        series = list(stacking = "normal"),
        bar = list(groupPadding = 0, pointPadding =  0, borderWidth = 0)
    ) %>% 
    hc_tooltip(
        shared = FALSE,
        formatter = JS("function () { return '<b>' + this.series.name + ', age ' + this.point.category + '</b><br/>' + 'Population: ' + Highcharts.numberFormat(Math.abs(this.point.y), 0);}")
    ) %>% 
    hc_yAxis(
        labels = list(
            formatter = JS("function(){ return Math.abs(this.value) / 1000000 + 'M'; }") 
        ),
        tickInterval = 0.5e6,
        min = -maxpop,
        max = maxpop) %>% 
    hc_xAxis(
        xaxis,
        rlist::list.merge(xaxis, list(opposite = TRUE, linkedTo = 0))
    )

# the above moyion chart plotted


#####
data(favorite_bars)
data(favorite_pies)

highchart() %>% 
    hc_add_series(  favorite_pies,    "column",  hcaes(  x = pie, y = percent  ), name = "Pie" ) %>%
    hc_add_series(  favorite_bars,    "pie",     hcaes(name = bar,  y = percent ), name = "Bars" ) %>%
    # Options for each type of series
    hc_plotOptions( series = list( showInLegend = FALSE, pointFormat = "{point.y}%", colorByPoint = TRUE ),
                    pie    = list( center = c('30%', '10%'), size = 120, dataLabels = list(enabled = FALSE)  )  ) %>%
    hc_yAxis( title = list(text = "percentage of tastiness"), 
              labels = list(format = "{value}%"),   max = 100  ) %>% 
    hc_xAxis( categories = favorite_pies$pie   ) %>%
    hc_title( text = "How I Met Your Mother: Pie Chart Bar Graph" ) %>% 
    hc_subtitle( text = "This is a bar graph describing my favorite pies 
                 including a pie chart describing my favorite bars" ) %>%
    hc_caption( text = "The values represented are in percentage of tastiness and awesomeness."  ) %>% 
    hc_credits( enabled = TRUE, text = "Source: HIMYM",  href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
                 style = list(fontSize = "12px") ) %>% 
    hc_size( height = 600 )

# Weathers Radials  Example inspired by http://weather-radials.com/.
data(weather)

x <- c("Min", "Mean", "Max")
y <- sprintf( "{point.%s}°" ,
              c("min_temperaturec",  "mean_temperaturec", "max_temperaturec")   ) 

tltip <- tooltip_table(x, y)

weather %>% hchart( type = "columnrange",
          hcaes(x = date, low = min_temperaturec,  high = max_temperaturec,color = mean_temperaturec ) ) %>%
          hc_chart(polar = TRUE ) %>%
          hc_yAxis(   max = 30,  min = -10,  labels = list(format = "{value} C"), showFirstLabel = FALSE ) %>%
          hc_xAxis(  title = list(text = ""),  gridLineWidth = 0.5, labels = list(format = "{value: %b}")  ) %>%
          hc_tooltip( useHTML = TRUE, pointFormat = tltip, headerFormat = as.character(tags$small("{point.x:%d %B, %Y}")) ) %>% 
          hc_title(  text = "Climatical characteristics of San Francisco" ) %>% 
          hc_size( height = 600 )

# generic 
x <- c( rnorm(10000), rnorm(1000, 4, 0.5))
hchart(x, name = "data") 

# One of the nicest class which hchart can plot is theforecast` class from the {forecast} package.
library(forecast)
airforecast <- forecast( auto.arima(AirPassengers), level = 95)
hchart(airforecast)

# scatter
data(penguins, package = "palmerpenguins")
hchart(penguins, "scatter", hcaes(x = flipper_length_mm, y = bill_length_mm, group = species))

# highstock
library(quantmod)
x <- getSymbols("GOOG", auto.assign = FALSE)
y <- getSymbols("AMZN", auto.assign = FALSE)

highchart(type = "stock") %>% 
    hc_add_series(x) %>% 
    hc_add_series(y, type = "ohlc")

# Highmap
data(GNI2014, package = "treemap")

hcmap( "custom/world-robinson-lowres", 
    data = GNI2014,
    name = "Gross national income per capita", 
    value = "GNI",
    borderWidth = 0,
    nullColor = "#d3d3d3",
    joinBy = c("iso-a3", "iso3")
   ) %>%
    hc_colorAxis( stops = color_stops( colors = viridisLite::inferno(10, begin = 0.1) ), type = "logarithmic" ) 


# funnel chart
library(dplyr)
library(highcharter)
options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))

df <- data.frame(
    x = c(0, 1, 2, 3, 4),
    y = c(975, 779, 584, 390, 200),
    name = as.factor(c("Leads", "Sales Call", "Follow Up", "Conversion", "Sale"))
     ) %>%
    arrange(-y)
df

df %>%     #  x   y    name
      hchart( "funnel",   # area
              hcaes(x = name, y = y),
             name = "Sales Conversions"
            )

## [{"area",... }: "arearange",... } "areaspline",... }"areasplinerange",... }
# "bar",... } "bellcurve",... } "boxplot",... }"bubble",... } "bullet",... }"column",... } "columnpyramid",... }
#"columnrange",... }"cylinder",... } "dependencywheel",... }"dumbbell",... }"errorbar",... }
#"funnel",... }"funnel3d",... } "gauge",... }"heatmap",... }
# "histogram",... } "item",... } "line",... } "lollipop",... }
# "networkgraph",... } "organization",... }"packedbubble",... }  "pareto",... } "pie",... } "polygon",... }
# "pyramid",... } "pyramid3d",... } "sankey",... }
# "scatter",... } "scatter3d",... }
#"solidgauge",... } "spline",... }"streamgraph",... } "sunburst",... }"tilemap",... }
# "timeline",... } "treemap",... }
# "variablepie",... } "variwide",... } "vector",... }
#"venn",... }"waterfall",... } "windbarb",... }"wordcloud",... } "xrange",... }

df %>%     #  x   y    name
    hchart( "area",   # area
            hcaes(x = name, y = y),
            name = "Sales Conversions"
    )

data("citytemp")

hc <- highchart() %>% 
    hc_xAxis(categories = citytemp$month) %>% 
    hc_add_series( name = "Tokyo", data = citytemp$tokyo ) %>% 
    hc_add_series( name = "London", data = citytemp$london ) %>% 
    hc_add_series(  name = "Other city", data = (citytemp$tokyo + citytemp$london)/2   )

# With hc_chart you can define general chart options.

hc %>% 
    hc_chart( borderColor     = '#EBBA95', borderRadius = 10, borderWidth = 2,
              backgroundColor = list( linearGradient = c(0, 0, 500, 500),
              stops           = list( list(0, 'rgb(255, 255, 255)'), 
                                      list(1, 'rgb(200, 200, 255)')  )  )
           )
# back to the line!
hc <- hc_chart(hc, type = "line", options3d = list(enabled = FALSE))
hc

# Title and subtitle
hc %>% 
    hc_title( text = "This is the title of the chart" ) %>% 
    hc_subtitle(text = "This is an intereseting subtitle to give
                 context for the chart or some interesting fact") %>% 
    hc_caption( text = "This is a long text to give some 
           subtle details of the data which can be relevant to the reader. 
          This is usually a long text that's why I'm trying to put a  <i>loooooong</i> text.",  useHTML = TRUE ) %>% 
    hc_credits( text = "Chart created using R and highcharter", href = "http://jkunst.com/highcharter", enabled = TRUE  )


# Axis: Usually is desirable get control or modify the behavior of the axis:
    
hc %>% 
    hc_xAxis(title = list(text = "Month in x Axis"), opposite = TRUE ) %>% 
    hc_yAxis(title = list(text = "Temperature <b>in y Axis</b>", useHTML = TRUE),
             opposite = TRUE,
             minorTickInterval = "auto",
             minorGridLineDashStyle = "LongDashDotDot",
             showFirstLabel = FALSE,
             showLastLabel = FALSE       )

# A good feature is plotLines and plotBand which you can use in both axis. For example:

hc %>% 
    hc_xAxis( plotLines = list(  list( label = list(text = "This is a plotLine"),
                                        color = "#FF0000",
                                        width = 2,
                                        value = 4,
                                        zIndex = 1 # the zIndex is used to put the label text over the grid lines 
                                           )
                                )    ) %>% 
    hc_yAxis( plotBands = list( list(  from = 20, to = 50, color = hex_to_rgba("red", 0.1),
                                       label = list(text = "This is a plotBand"),
                                        zIndex = 1    )
                               )
    ) 
## Legend & tooltip
hc %>%
    hc_legend(  align = "left",
                verticalAlign = "top",
                layout = "vertical",
                x = 0, y = 100 ) %>%
    hc_tooltip( crosshairs = TRUE,
                backgroundColor = "#F0F0F0",
                shared = TRUE, 
                borderWidth = 5  )



##############
conflicts(where = search(), detail = FALSE)

## bee haney  graph
# https://buzzrbeeline.blog

library(ggplot2)
library(png)
library(grid)
library(ggimage)

setwd("/Users/gk/Google Drive")

bees <- data.frame(distance = c(0.5, 0.5, 1.5, 2, 2.5, 3),
                     number = c(40,  34,  32,  22, 18, 10)    )

img <- readPNG("honeycomb.png")

ggplot(data = bees, aes(x = distance, y = number)) +
    annotation_custom(rasterGrob(img, 
                                 width = unit(1,"npc"),
                                 height = unit(1,"npc")), 
                      -Inf, Inf, -Inf, Inf) +
    geom_point() +
    xlab("Distance (km)") +
    ylab("Number of Bees") +
    ylim(0, 45)

# To use bees as markers we need to add the bee image file name to every row of the data frame:
# Now edit your graph code again by removing the geom_point() and adding a geom_image() i  


bees$beeimage <- "honeybee.png"
    
ggplot(data = bees, aes(x = distance, y = number)) +
    annotation_custom(rasterGrob(img, 
                                 width = unit(1,"npc"),
                                 height = unit(1,"npc") ), 
                                 -Inf, Inf, -Inf, Inf) +
    geom_image( aes(image = beeimage),  size = 0.15 ) +
    xlab("Distance (km)" ) +
    ylab("Number of Bees") +
    ylim(0, 45)  +
    theme_bw()

bees$beeimage <- "WDP4.png"
ggplot(data = bees, aes(x = distance, y = number)) +
    annotation_custom(rasterGrob(img, width = unit(1,"npc"), height = unit(1,"npc") ), -Inf, Inf, -Inf, Inf) +
    geom_image( data = NULL, inherit.aes = TRUE, na.rm = FALSE,
                aes(image = beeimage),  
                size = 0.25 , by =  "width", # "height", 
                color = "red", nudge_x = 0.1, angle = 30, position = "identity"
                ) +
    xlab("Distance (km)" ) +
    ylab("Number of Bees") +
    ylim(0, 45)  +
    theme_bw()  + coord_flip()

############## funnel chart
library(dplyr)
library(highcharter)
options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))
df <- data.frame( x = c(0, 1, 2, 3, 4),
                  y = c(975, 779, 584, 390, 200),
                 name = as.factor(c("Leads", "Sales Call", "Follow Up", "Conversion", "Sale"))
               ) %>%  arrange(-y)

library(tidyverse)
hc <- df %>%   hchart( "funnel",  hcaes(x = name, y = y),   name = "Sales Conversions" )
hc





##################

detach(googlevis)
library(ggplot2)
library(cowplot)  

plot.mpg <- ggplot(mpg,  aes(x = cty, y = hwy, colour = factor(cyl)  ) ) +  geom_point(size = 2.5)
plot.mpg    # ~ theme_classic for grey  theme_set(theme_gray())

# use save_plot(fname, plot, base_aspect_ration= 4:9) instead of ggsave()
# save_plot(filename, plot, ncol = 1, nrow = 1, base_height = 4, base_aspect_ratio = 1.1, base_width = NULL, ..., cols = NULL, rows = NULL)
save_plot( "mpg.png",  plot.mpg,  base_aspect_ratio = 1.3 )  # make room for figure legend

plot.mpg + background_grid(major = "xy", minor = "none")   # gridlines with background_grid(): 
plot.mpg

draw( plot.mpg ) 
ggdraw( plot.mpg ) +   draw_label( "DRAFT!" ,  size = 40, angle = 30,  alpha = .2) # adding draw layers !!

plot.diamonds <- ggplot(diamonds, aes(clarity, fill = cut)) + geom_bar() +  theme(axis.text.x = element_text(angle=70, vjust=0.5))

plot_grid(  plot.mpg,   plot.diamonds,    labels = c("A", "B") , nrow = 1, align = "h"   )  # combine graphs with plot_grid(), h align:

plot2by2  <- plot_grid(  plot.mpg,   NULL,   NULL,   plot.diamonds,   labels = c("A", "B", "C", "D"),  ncol = 2 )

# save_plot(), with grid plot_grid(), eg 2-by-2 figure:
save_plot( "plot2by2.png",  plot2by2,   ncol = 2,  nrow = 2,  base_aspect_ratio = 1.3  )  # each  subplot  aspect 1.3

ggdraw(plot2by2)
ggdraw(plot.mpg)

#  ggdraw() sets up the drawing layer, and functions on this drawing layer all start with draw_. 
#  ggdraw() produces a standard ggplot2 object, and you can do whatever like ggplot2
ggdraw(plot.mpg) +   draw_label( "DRAFT!"        ,  size = 40, angle =  45,  alpha = .2) +
                     draw_label( "add anothor !" ,  size = 20, angle = -45,  alpha = .2) +
                     draw_plot_label("A mix drawing", size = 24,angle = -5,  alpha = .4 )

t <- (0:1000)/1000   # good simple power examiple
spiral <- data.frame( x = .45 + .55*t*cos(t*15),  y = .55-.55*t*sin(t*15),  t)
spiral
ggdraw(plot.mpg) +  geom_path( data = spiral, aes(x = x, y = y, colour = t), size = 6, alpha = .4) 
                
# Ordering layers, you can initialize an empty drawing canvas by calling ggdraw() without any parameters. 

# draw_plot() function also allows us to place graphs at arbitrary locations 

# image or plot  inset plot.mpg and plot.diamonds were defined earlier
library(viridis)
ggdraw() +
            draw_plot(plot.diamonds  + theme(legend.justification = "bottom"), 0, 0, 1, 1 )   +  # The main chart
            draw_plot(plot.mpg + scale_color_viridis(discrete = TRUE) + 
                                   theme(legend.justification = "top"), 0.6, 0.6, 0.4, 0.4 )  +  # the inset chart
            draw_plot_label(c("A", "B"), c(0, 0.3), c(1, 0.8), size = 5)

# with image
p    <-    ggplot(iris, aes(x=Sepal.Length, fill=Species)) + geom_density(alpha = 0.7)

ggdraw()  +    
            # draw_image("http://jeroen.github.io/images/tiger.svg") +
            draw_image( "plot2by2.png"  , scale = 0.80  ) +     # used image !!
            draw_plot(plot.mpg)   +
            draw_plot(p)

# side by side with image
p <- ggplot(iris, aes(x = Sepal.Length, fill = Species)) + geom_density(alpha = 0.7)
p2 <- ggdraw() + draw_image("http://jeroen.github.io/images/tiger.svg", scale = 0.9)  # or
p2 <- ggdraw() + draw_image("plot2by2.png", scale = 0.9)
plot_grid( p,  p2,  p2, p , labels = "AUTO")

############################################################
###########################################################
# Venn
set.seed(20190708)
genes <- paste("gene", 1:1000,  sep="" )
x <- list(
    A = sample(genes,300), 
    B = sample(genes,525), 
    C = sample(genes,440),
    D = sample(genes,350)
)

if (!require(devtools)) install.packages("devtools")
devtools::install_github("yanlinlin82/ggvenn")
library(ggvenn)
ggvenn(x)  # Graphique par défaut
ggvenn(
    x, 
    fill_color = c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF"),
    stroke_size = 0.5, set_name_size = 4
)



names(x) <- c("Stage 1","Stage 2","Stage 3", "Stage4")  # Changer les noms de catégories
ggvenn(
    x, 
    fill_color = c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF"), # Changer la couleur de remplissage
    stroke_size = 0.5, set_name_size = 4
)

ggvenn(  x, 
         columns = c("Stage 1", "Stage 2", "Stage 3"),
         stroke_size = 0.5
     )

# Diagramme de Venn en deux dimensions
ggvenn( x, 
        columns = c("Stage 1", "Stage 2"),
        stroke_size = 0.5
     )





######################## R ggVennDiagram
if (!require(devtools)) install.packages("devtools")
#devtools::install_github("gaospecial/ggVennDiagram")
library("ggVennDiagram")
ggVennDiagram(x)   # Graphique par défaut
ggVennDiagram(x, label_alpha = 0) #+ theme_bw()
# Changer la couleur de remplissage du dégradé
ggVennDiagram( x, 
               label_alpha = 0, 
               category.names = c("Stage 1","Stage 2","Stage 3", "Stage4") ) +
    ggplot2::scale_fill_gradient(low="blue",high = "yellow")


ggVennDiagram(x[1:3], label_alpha = 0.7)
ggVennDiagram(x[1:2], label_alpha = 0)







#Utilisation du package R VennDiagram
install.packages("VennDiagram")
library(VennDiagram)
venn.diagram(x, filename = "dahlia.png")
# Afficher le graphique directement dans R:
# Fonction d'aide pour afficher le diagramme de Venn
display_venn <- function(x, ...){
        library(VennDiagram)
        grid.newpage()
        venn_object <- venn.diagram(x, filename = NULL, ...)
        grid.draw(venn_object)
    }

display_venn(x)       # Diagramme de Venn en quatre dimensions
display_venn(x[1:3])  # Diagramme de Venn en trois dimensions
display_venn( x,
    category.names = c("Set 1" , "Set 2 " , "Set 3", "Set 4"),  # Changer les noms de catégories
    fill = c("#999999", "#E69F00", "#56B4E9", "#009E73")        # Changer la couleur de remplissage
)


# Personnalisation supplémentaire
display_venn(
    x,
    category.names = c("Set 1" , "Set 2 " , "Set 3", "Set 4"),
    # Cercles
    lwd = 2,
    lty = 'blank',
    fill = c("#999999", "#E69F00", "#56B4E9", "#009E73"),
    # Nombres
    cex = .9,
    fontface = "italic",
    # Noms des groupes
    cat.cex = 1,
    cat.fontface = "bold",
    cat.default.pos = "outer",
    cat.dist = c(0.055, 0.055, 0.1, 0.1)
)


#Utilisation du package R gplots
#install.packages("gplots")
library(gplots)
v.table <- gplots::venn(x)
print(v.table)


################################################
#  RIDGELINE
library(ggplot2)
theme_set(theme_minimal())

library(ggridges)  # geom_density_ridges()

# Polygones ouverts
iris %>% ggplot( aes(x = Sepal.Length, y = Species, group = Species)) + 
    geom_density_ridges(fill = "#00AFBB")

# Polygones fermés lines
ggplot(iris, aes(x = Sepal.Length, y = Species, group = Species)) + 
    geom_density_ridges2(fill = "#00AFBB")

# Couper les queues. 
# Précisez `rel_min_height` : un pourcentage de seuil de coupure
ggplot(iris, aes(x = Sepal.Length, y = Species)) + 
    geom_density_ridges(fill = "#00AFBB", rel_min_height = 0.01)

# scale = 0,6, courbes séparées
ggplot(iris, aes(x = Sepal.Length, y = Species)) + 
    geom_density_ridges(scale = 0.6)

# scale = 1, courbes se touchant exactement
ggplot(iris, aes(x = Sepal.Length, y = Species)) + 
    geom_density_ridges(scale = 1)

# scale = 5, chevauchement important
ggplot(iris, aes(x = Sepal.Length, y = Species)) + 
    geom_density_ridges(scale = 5, alpha = 0.7)

# Modifier les couleurs de remplissage de la zone de densité par groupes
ggplot(iris, aes(x = Sepal.Length, y = Species)) +
    geom_density_ridges(aes(fill = Species)) +
    scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
    theme(legend.position = "none")


lincoln_weather %>% ggplot(    aes(x = `Mean Temperature [F]`, y = `Month`, fill = stat(x)) ) +
    geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
    scale_fill_viridis_c( name = "Temp. [F]",    option = "C"   ) +
    labs(title = 'Temperatures in Lincoln NE') 

#######################################################

# ggrepel: repel overlapping text labels: geom_text_repel() and geom_label_repel()
library(ggrepel)
theme_set( theme_bw()  ) # Configurer le thème par défaut de ggplot2 à theme_bw()

# Données de démonstration
df <- tibble::tribble(
    ~Species, ~Petal.Length, ~Petal.Width, ~Sepal.Length, ~Sepal.Width,
    "setosa",         1.462,        0.246,         5.006,        3.428,
    "versicolor",          4.26,        1.326,         5.936,         2.77,
    "virginica",         5.552,        2.026,         6.588,        2.974
)
df
# Transformer les données en format long
# Mettez de Petal.length à Sepal.Width dans la même colonne
df_long <- df %>%
    pivot_longer(
        Petal.Length:Sepal.Width,
        names_to = "variable", 
        values_to = "value"
    )
df_long


# Line plot basique
lp <- ggplot(df_long, aes(x = Species, y = value, group = variable)) +
    geom_line(aes(color = variable)) +
    geom_point() +
    theme(legend.position = "top")
lp
# Filtrez les dernières valeurs et ajoutez-les sur le graphique
# Correspond à l'espèce `virginica`
data_ends <- df_long %>% filter(Species == "virginica")
lp +    geom_text_repel( data = data_ends,  
                         aes( label = value ), 
                         fontface ="plain", color = "black", size = 3 )

# Utiliser des noms de variables comme étiquettes
lp2 <- ggplot(df_long, aes(x = Species, y = value, group = variable)) +
    geom_line(aes(color = variable)) +
    geom_point() 
lp2

lp2 +  geom_text_repel( data = data_ends,
                        aes(label = variable), 
                                           color = "black", size = 3 ) +
         theme(legend.position = "top")


# secondary axis
# Extraire le vecteur des dernières valeurs

df2 <- Orange  # Données de démonstration
head(df2)
data_ends <- df2 %>% 
    group_by(Tree) %>% 
    top_n(1, age) %>%
    pull(circumference)
data_ends


ggplot(df2, aes(x = age, y = circumference)) +
    geom_line(aes(color = Tree)) +
    scale_y_continuous(sec.axis = sec_axis(~ ., breaks = data_ends))  # Créer le line plot avec des étiquettes


#######################################################
#######################################################
#######################################################



#######################################################
#######################################################




#######################################################
#######################################################


#######################################################
#######################################################



#######################################################
#######################################################















######## autoplot  with  ggfortify  ######
#install.packages("ggfortify")
library("ggfortify")

# Plotting matrix using autoplot.matrix()   
# autoplot(object, geom = "tile")  object (of class matrix) and geom are “tile” (for heatmap) or “point” (scatter plot)

df <- mtcars[, c("mpg", "disp", "hp", "drat", "wt")]
df <- as.matrix(df)
df

scale(df)   # applied to matrix objects
autoplot(  scale( df )   )      # Heatmap, pre sort would be better?

# Plot a scatter plot: The data  be a matrix with 2 columns named V1 and V2. 

df2  <-  df[  ,  c("wt", "mpg")  ]  #  plots mpg by wt
colnames(df2) <- c("V1", "V2")
autoplot(df2, geom = 'point') +     labs(x = "mpg", y = "wt")     # Scatter plot

# autoplot.lm() draw diagnostic plots for LM and GLM [in stats package].
# autoplot(object, which = c(1:3, 5))  #  object: stats::lm instance
#    which: subset of the plots if  required, specify  subset 1:6.
# ncol and nrow allows you to specify the number of subplot columns and rows.

m <- lm(Petal.Width ~ Petal.Length, data = iris)     # Compute a linear model

autoplot(m, which = 1:6, ncol = 2, label.size = 3)   # Create the plot
autoplot(m, which = 1:6,           label.size = 3, data = iris, colour = 'Species') # Change the color by groups (species)

# Diagnostic plots with Generalized Linear Models (GLM)  # USArrests data

m <- glm(Murder ~ Assault + UrbanPop + Rape, family = gaussian, data = USArrests)  # Compute a generalized linear model

autoplot(m, which = 1:6, ncol = 2, label.size = 3, colour = "steelblue") + theme_bw() # Create the plot # Change the theme and colour

# Plotting time series
# R Function: autoplot.ts()
autoplot(AirPassengers)

# autoplot() handles time-series-likes packages, 
#  zoo::zooreg(), xts::xts(), timeSeries::timSeries(), tseries::irts(), forecast::forecast(), vars:vars()


library(changepoint)    # for identifying shifts in mean and/or variance in a time series.
autoplot(  cpt.meanvar(AirPassengers)  )

library(strucchange)    # struc change detecting jumps in data.  
autoplot(  breakpoints(Nile ~ 1) )  #Data set: Nile

# Plotting PCA (Principal Component Analysis)  : Data set: iris, Function: autoplot.prcomp()
df <- iris[, -5]
df
pca <- prcomp(df, scale. = TRUE)    # Principal component analysis
autoplot(pca, loadings = TRUE, loadings.label = TRUE, data = iris, colour = 'Species')   # Plot

# Plotting K-means   Data set: USArrests,  Function: autoplot.kmeans() , original data  required as kmeans object doesn’t store original data. 
# Samples will be colored by groups (clusters).

autoplot(  kmeans(USArrests, 5),  data = USArrests,  label = TRUE, label.size = 3, frame = TRUE)  # pca 5

# Plotting cluster package
# ggfortify supports cluster::clara, cluster::fanny and cluster::pam classes, aand return object containing original data, so not reqd explicitly.
library(cluster)
autoplot(  pam(iris[-5], 4),  frame = TRUE,  frame.type = 'norm' )  # clusters 4


# Plotting Local Fisher Discriminant Analysis
library(lfda)       # Local Fisher Discriminant Analysis (LFDA)
model <- lfda(iris[,-5], iris[, 5], 4, metric="plain")
autoplot(model, data = iris, frame = TRUE, frame.colour = 'Species')


# Plotting survival curves
library(survival)
lung
fit <- survfit(  Surv(time, status)  ~  sex ,   data = lung   )
fit
autoplot(fit)

################
library(tidyverse)
library(hrbrthemes)
library(ggalt)

df <- data.frame(   trt=LETTERS[1:5] , 
                    l=c(20, 40, 10, 30, 50), 
                    r=c(70, 50, 30, 60, 80)  )

# ???  
x <- read.table(file = "clipboard" )



ggplot(df, aes(y=trt, x=l, xend=r)) + 
    geom_dumbbell(size     =3,   color="#e3e2e1", 
                  colour_x  = "#5b8124", colour_xend = "#bad744",
                  dot_guide = TRUE, dot_guide_size=0.25 ) +
    labs(x=NULL, y=NULL, title="ggplot2 geom_dumbbell with dot guide") +
    #theme_ipsum_rc(grid="X") +
    theme(panel.grid.major.x=element_line(size=0.05)) +
    labs(x=NULL, y=NULL, 
         title="SUNY Cortland Multicultural Alumni survey results",
         subtitle="Ranked by race, ethnicity, home land and orientation\namong the top areas of concern",
         caption="Data from http://stephanieevergreen.com/lollipop/") +
         theme_minimal(base_family="Arial Narrow") +
         theme(panel.grid.major.y=element_blank()) +
         theme(panel.grid.minor=element_blank()) +
         theme(axis.line.y=element_line(color="#2b2b2b", size=0.15)) +
         theme(axis.text.y=element_text(margin=margin(r=0, l=0))) +
         theme(plot.margin=unit(rep(30, 4), "pt")) +
         theme(plot.title=element_text(face="bold")) + 
         theme(plot.subtitle=element_text(margin=margin(b=10))) +
         theme(plot.caption=element_text(size=8, margin=margin(t=10)))  +# or +    
         # theme_ipsum()    #library(hrbrthemes)
         geom_label(aes(x = 3.5, y = 2.5, label = "I'm an annotation!"), 
             hjust = 0,  vjust = 0.5,    colour = "#555555",   fill = "white",  label.size = NA, family="Helvetica", 
             size = 3)


