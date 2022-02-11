# tidydeocoder Tidygeocoder 1.0.4  also osmextract
# Combining Multiple Queries from multiple servers using geocode_combine() and geo_combine()
# install.packages('tidygeocoder')  # or #devtools::install_github("jessecambon/tidygeocoder")



library(tidyverse)
library(tidygeocoder)

mixed_addresses <- tribble(
  ~street_address, ~city, ~state_cd, ~zip_cd,
  "624 W DAVIS ST #1D",   "BURLINGTON", "NC",  27215,
  "201 E CENTER ST #268", "MEBANE",     "NC",  27302,
  "7833  WOLFE LN",       "SNOW CAMP",  "NC",  27349,
  "202 C St",             "San Diego",  "CA",  92101,
  "121 N Rouse Ave",      "Bozeman",    "MT",  59715
            ) %>%
  bind_rows(tibble(city = c('Taipei', 'Moscow', 'Buenos Aires'))  )

mixed_addresses

results <- mixed_addresses %>%
  geocode_combine(
    queries = list(
      list(method = 'census', mode = 'batch',
           street = 'street_address', city = 'city', state = 'state_cd', postalcode = 'zip_cd'),
      list(method = 'census', mode = 'single',
           street = 'street_address', city = 'city', state = 'state_cd', postalcode = 'zip_cd'),
      list(method = 'arcgis', address = 'city')   ),
    query_names = c('census - batch', 'census - single', 'arcgis')
  )

results



library(dplyr, warn.conflicts = FALSE)
library(tibble)
library(tidygeocoder)

# create a dataframe with addresses
some_addresses <- tribble(
  ~name,                  ~addr,
  "White House",          "1600 Pennsylvania Ave NW, Washington, DC",
  "Transamerica Pyramid", "600 Montgomery St, San Francisco, CA 94111",
  "Willis Tower",         "233 S Wacker Dr, Chicago, IL 60606"
)

some_addresses
# geocode the addresses  not worked
lat_longs <- some_addresses %>%  geocode(addr, method = 'osm', lat = latitude , long = longitude)
lat_longs

# Plot
library(ggplot2)
library(maps)
library(ggrepel)

ggplot(lat_longs, aes(longitude, latitude), color = "grey99") +
  borders("state") + geom_point() +
  geom_label_repel(aes(label = name)) +
  theme_void()


# Reverse geocode
reverse <- lat_longs %>%
  reverse_geocode(lat = latitude, long = longitude, method = 'osm',
                  address = address_found, full_results = TRUE) %>%
  select(-addr, -licence)


#############################
#############################
#############################
library(osmextract)
library(sf)

Delhi <- oe_get("Lucknow")
oe_match("Lucknow, India") # ? No exact match found for place = Lucknow, India and provider = geofabrik.
oe_match_pattern("Delhi")
oe_match_pattern("Lucknow") # Null

