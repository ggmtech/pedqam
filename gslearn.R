#learn  gs  google sheets in R
#https://cran.r-project.org/web/packages/googlesheets/vignettes/basic-usage.html
####################################################
library(googlesheets4)
library(tidyverse)
library(lubridate)
library(forcats)

#############################################
library(rvest)
## Read the web page and store into faa_mapping_html object.
faa_mapping_html <- read_html("http://aspmhelp.faa.gov/index.php/ASQP:_Carrier_Codes_and_Names")

faa_mapping_html  # got "table"  nade by selector gadget in chrome
faa_mapping <- faa_mapping_html %>%   html_node("table") %>%   html_table()


search <- html_form(read_html("http://www.google.com"))[[1]]
set_values(search, q = "My little pony")
set_values(search, hl = "fr")



###################################################

devtools::install_github('emitanaka/anicon')

###############################################
#########
#basemaps is a lightweight R package to download and cache spatial basemaps from open sources such as OpenStreetMap, Carto, Mapbox and others.
# install.packages("basemaps")    #
devtools::install_github("16EAGLE/basemaps")
library(basemaps)
# similar is "ceramic"  is to obtain web map tiles. library(ceramic)

data(ext)  # or use draw_ext() to interactively draw an extent yourself
draw_ext()
get_maptypes()  # view all available maps

# set defaults for the basemap
set_defaults(map_service = "mapbox", map_type = "satellite", map_token = "YOUR_MAPTOKEN_IF_NEEDED")

# load and return basemap map as many different classes:
basemap_plot(ext)    #> Loading basemap 'satellite' from map service 'mapbox'...

basemap_mapview(ext)  #> Loading basemap 'satellite' from map service 'mapbox'...

basemap_ggplot(ext)

basemap_magick(ext)

basemap_raster(ext)

basemap_stars(ext)

basemap_png(ext)

basemap_geotif(ext)

library(ggplot2)
ggplot() +
  basemap_gglayer(ext) +
  coord_sf() +
  scale_fill_identity()


basemap_magick(ext, map_service = "osm", map_type = "topographic")  #> Loading basemap 'topographic' from map service 'osm'...
basemap_magick(ext, map_service = "osm_stamen", map_type = "toner")
basemap_magick(ext, map_service = "osm", map_type = "hike")
basemap_magick(ext, map_service = "carto", map_type = "dark")
basemap_magick(ext, map_service = "mapbox", map_type = "hybrid")
basemap_magick(ext, map_service = "mapbox", map_type = "streets")
basemap_magick(ext, map_service = "mapbox", map_type = "terrain")
# get_maptypes() returns every supported map service and map type that can be used as input
# draw_ext() lets you draw an extent on an interactive map.
# set_defaults(), get_defaults() and reset_defaults() set, get or reset the defaults of all map arguments passed to basemap() or associated functions.
# basemap() and aliases basemap_raster(), basemap_stars(), basemap_mapview(), basemap_plot(), basemap_ggplot(), basemap_gglayer(), basemap_magick(), basemap_png() and basemap_geotif()






#############################
# moveVis  tools to visualize movement data (e.g. from GPS tracking) and temporal changes of environmental data (e.g. from remote sensing) by creating video animations.


## add first six rows of iris data (and var names) into a blank sheet
foo <- foo %>%
  gs_edit_cells(ws = "edit_cells", input = head(iris), trim = TRUE)

## initialize sheet with column headers and one row of data
## the list feed is picky about this
foo <- foo %>%
  gs_edit_cells(ws = "add_row", input = head(iris, 1), trim = TRUE)

## add the next 5 rows of data ... careful not to go too fast, add Sys.sleep(0.3)
for (i in 2:6) {
  foo <- foo %>% gs_add_row(ws = "add_row", input = iris[i, ])
  Sys.sleep(0.3)
}

## gs_add_row() can atually handle multiple rows at once
foo <- foo %>%
  gs_add_row(ws = "add_row", input = tail(iris))

# delete
gs_delete(foo)


#Make new Sheets from local delimited files or Excel workbooks
# Use gs_upload() to create a new Sheet de novo from a suitable local file.

iris %>%  head(5) %>%   write.csv("iris.csv", row.names = FALSE)  # create csv file

iris_ss <- gs_upload("iris.csv")


# Now we’ll upload a multi-sheet Excel workbook. Slowly.
# Now we’ll upload a multi-sheet Excel workbook. Slowly.

gap_xlsx <- gs_upload( system.file( "mini-gap",  "mini-gap.xlsx",  package = "googlesheets"))
gap_xlsx <- gs_upload( system.file( "mini-gap",  "mini-gap.xlsx",   package = "googlesheets"))

# delete vector of files  vecdel
gs_vecdel(c("iris", "mini-gap"))

#Use gs_download() to download a Google Sheet as a csv, pdf, or xlsx file

gs_title("Gapminder") %>%   gs_download(ws = "Africa", to = "gapminder-africa.csv")

#Download the entire spreadsheet as an Excel workbook.

gs_title("Gapminder") %>%   gs_download(to = "gapminder.xlsx")



# file remove
file.remove(c("gapminder.xlsx", "gapminder-africa.csv"))


#Read data, but with more control
#Read data, but with more control
#Read data, but with more control

# Get the data for worksheet "Oceania": the super-fast csv way
oceania_csv <- gap %>% gs_read_csv(ws = "Oceania")

# Get the data for worksheet "Oceania": the less-fast tabular way ("list feed")
oceania_list_feed <- gap %>% gs_read_listfeed(ws = "Oceania")


# Get the data for worksheet "Oceania": the slow cell-by-cell way ("cell feed")
oceania_cell_feed <- gap %>% gs_read_cellfeed(ws = "Oceania")

# gs_reshape_cellfeed(), makes a 2D thing, i.e. a data frame
# gs_simplify_cellfeed() makes a 1D thing, i.e. a vector

## reshape into 2D data frame
gap_3rows <- gap %>% gs_read_cellfeed("Europe", range = cell_rows(1:3))

gap_3rows %>% gs_reshape_cellfeed()

# Example: first row only
gap_1row <- gap %>% gs_read_cellfeed("Europe", range = cell_rows(1))

# convert to a named (character) vector  -- becomes useful data table
gap_1row %>% gs_simplify_cellfeed()


# Example: single column
gap_1col <- gap %>% gs_read_cellfeed("Europe", range = cell_cols(3))


df <- data_frame(thing1 = paste0("A", 2:5),
                 thing2 = paste0("B", 2:5),
                 thing3 = paste0("C", 2:5))
df$thing1[2] <- paste0("#", df$thing1[2])
df$thing2[1] <- "*"

#new sheet for data ingest practice, with input df

ss <- gs_new("data-ingest-practice", ws_title = "simple",   input = df, trim = TRUE) %>%
  gs_ws_new("one-blank-row", input = df, trim = TRUE, anchor = "A2") %>%
  gs_ws_new("two-blank-rows", input = df, trim = TRUE, anchor = "A3")

# ss is google handle

# the above made the new sheet with 3 ws
# check to read with assigned col names
ss %>% gs_read(col_names = letters[1:3], skip = 1)
ss     ## ss is google handle
## explicitly use gs_read_listfeed
ss %>% gs_read_listfeed( col_names = FALSE, skip = 1)      # ss is google handle




## use range to force use of gs_read_cellfeed
ss %>% gs_read_listfeed(col_names = FALSE, skip = 1, range = cell_cols("A:Z"))


ss %>% gs_read(ws = "one-blank-row")  ## blank row causes variable names to show up in the data frame :(
ss %>% gs_read(ws = "one-blank-row", skip = 1)   ## skip = 1 fixes it :)

## more arguments, more better
ss %>% gs_read(ws = "one-blank-row", skip = 2,
               col_names = paste0("yo ?!*", 1:3), check.names = TRUE,
               na = "*", comment = "#", n_max = 2)


## also works on list feed
ss %>% gs_read_listfeed(ws = "one-blank-row", skip = 2,
                        col_names = paste0("yo ?!*", 1:3), check.names = TRUE,
                        na = "*", comment = "#", n_max = 2)


## also works on the cell feed
ss %>% gs_read_listfeed(ws = "one-blank-row", range = cell_cols("A:Z"), skip = 2,
                        col_names = paste0("yo ?!*", 1:3), check.names = TRUE,
                        na = "*", comment = "#", n_max = 2)


## use skip to get correct result via gs_read() --> gs_read_csv()
ss %>% gs_read(ws = "two-blank-rows", skip = 2)

## or use range in gs_read() --> gs_read_cellfeed() + gs_reshape_cellfeed()
ss %>% gs_read(ws = "two-blank-rows", range = cell_limits(c(3, NA), c(NA, NA)))


ss %>% gs_read(ws = "two-blank-rows", range = cell_cols("A:C"))


## list feed can't cope because the 1st data row is empty
ss %>% gs_read_listfeed(ws = "two-blank-rows")

ss %>% gs_read_listfeed(ws = "two-blank-rows", skip = 2)


## from https://cran.r-project.org/web/packages/googlesheets/vignettes/basic-usage.html

####################################################

# dplyr joins
inner_join(superheroes, publishers)    # All colums of x, y   but of rows of x having matchings in Y
inner_join(publishers, superheroes)

semi_join(superheroes, publishers)     # Only columns from x,  and filtred x  also in y, and unrepeated
semi_join(publishers, superheroes)

anti_join(superheroes, publishers)     # Only columns from x,  and rows data which are not in y
anti_join(publishers, superheroes)

left_join(superheroes, publishers)     # also right_join
left_join(publishers, superheroes)

full_join(superheroes, publishers)     # all combinaitons



#Its common to want to know if one data set is the same as another dataset
# dplyr’s setequal ;
# base R’s identical true if the datasets  exact same rows in the exact same order

#  Mutating Joins:   left_join, right_join, inner_join, full_join
# Filtering Joins:   semi_join, anti_join

#Set Operations:
union()
intersect()
setdiff()   # miinus sets
setequal()  #Comparisions:

#If the datasets have the same columns and all are used in the key, then intersect is analagous to semi_join
#In the same scenario setdiff is similar to anti_join


# Check if any order: definitive and union of complete and soundtrack
union(complete, soundtrack) %>%    setequal(definitive)   # true

#Assembling data  : # Binds
# Base R binds:  rbind, cbind
# dplyr  binds:  bind_rows, bind_cols       # faster, tibble , handle lists of dataframes



#####################################################
# Read existing ss files
lf16link <- gs_title("locofail1617")      # register it and store gs as locofail1718
lf17link <- gs_title("locofail1718")      # register it and store gs as locofail1718

# registration  gs_title(), gs_key(), gs_url(), and gs_gs()  registers
# extract_key_from_url(URL), gs_key(URL),  gs_url(),  gs_url(GAP_URL)

gs_ws_ls(lf16link)      # ls  worksheets names with handle
gs_ws_ls(lf17link)      # ls  worksheets names with handle
# Visit a Google Sheet in the browser
gs_browse(ss = lf16link, ws = "setout1617" )  # or  gs_browse(ws = 1)
#browse the worksheet as
lf17  %>% gs_browse(ws = "locodata")    # or locos %>% gs_browse(ws = 1)
lf17  %>% gs_browse(ws =     2     )    # default is ws = 1

data() # preexisting datas in the various packages
# gs_download gs to xlsx, csv or pdf to your computer folder
# gs_download(from, ws = NULL, to = NULL, overwrite = FALSE, verbose = TRUE)
# from = gs object, ws = int or wsname, to = path to write with ext = xlsx[default], csv, pdf

gs_title("locofail1617") %>%
  gs_download(ws = "setout1617", to = "testupload.csv",  overwrite = FALSE, verbose = TRUE)

read.csv("testupload.csv") %>% glimpse()   # check from your folder

# download entire worksheet by not specifying ws
gs_download( locofail1617handle,     to = "testupload.xlsx", overwrite = TRUE, verbose = TRUE )

ls()              # check then remove
file.remove(c("gapminder.xlsx", "gapminder-africa.csv"))

##############################################

# Read existing ss files, first register the specific ss file
lf16link <- gs_title("locofail1617")      # register it and store gs as locofail1718
lf17link <- gs_title("locofail1718")      # register it and store gs as locofail1718

#gs_read() reads actual data of worksheet and returns a data frame
lf16 <-  gs_read(ss = lf16link,  ws = "setout1617" , skip = 1, col_names = TRUE)
lf17 <-  gs_read(ss = lf17link, ws = "Setouts 2017-18",  skip = 1, col_names = TRUE)
# range =  "A1:D8" or range = cell_rows(1:1000), or cell_cols(1:4) or cell_limits(  c(1, 4),  c(5, NA)  )


lf16
lf16link %>% gs_copy(to = "locobak")    # copies locoboak to gdrive, check gs_ls("loc")

# joining both tables after finding mismatched colomns
a <- as.data.frame(names(lf16) )
b <-  as.data.frame(  names(lf17)  )
names(a) <- "myrows"
names(b) <- "myrows"

anti_join(a,b)
anti_join(b,a)

lf17 %>% select(`Brief RESP cause`) %>% filter(!is.na(`Brief RESP cause`))
lf17$`Brief RESP cause`  <- NULL
lf17$`Specific reason(Axle lock/fire/ /)`  <- NULL
lf16$`Specific reason(Axle lock/fire/ /)`  <- NULL

lfall <- bind_rows(lf16, lf17)
lfall
lfall %>% distinct() %>% View()




locosselect1 <- locos %>% gs_read(ws = "locodata", range = cell_rows(1:2))
locosselect1




locosselect2 <- locos %>% gs_read(ws = "locodata", range = cell_rows(100:103), col_names = FALSE)
locosselect2


## googlesheets aims to match the interface of the readr package.
##
?gs_new()
boring_ss <- gs_new("boring", ws_title = "iris-gs_new", input = head(iris), trim = TRUE, verbose = FALSE)
boring_ss
boring_ss %>% gs_read()

#Add a new worksheet to an existing Google Sheet with  gs_ws_new()

#Use gs_ws_new() to add some mtcars data as a second worksheet to boring_ss.
boring_ss <- boring_ss %>% gs_ws_new(ws_title = "mtcars-gs_ws_new", input = head(mtcars), trim = TRUE, verbose = FALSE)
boring_ss %>% gs_read(ws = 2)

#Rename or delete worksheets We use gs_ws_delete() and gs_ws_rename()
boring_ss <- boring_ss %>%  gs_ws_delete(ws = 2) %>%  gs_ws_rename(to = "iris")

#Edit cells : There are two ways to edit cells within an existing worksheet of an existing spreadsheet:
gs_edit_cells()     #can write into an arbitrary cell rectangle
gs_add_row()        #can add a new row to the bottom of an existing cell rectangle

# gs_upload()
gs_add_row()    #is faster,


#We create a new Sheet, foo, and set up some well-named empty worksheets to practice with.
foo <- gs_new("foo") %>%          # new blank spreadsheet
  gs_ws_rename(from = "Sheet1", to = "edit_cells") %>%   # rename the default worksheet name
  gs_ws_new("add_row")                # add a new worksheet blank

## add first six rows of iris data (and var names) into a blank sheet
foo <- foo %>%  gs_edit_cells(ws = "edit_cells", input = head(iris), trim = TRUE)
gs_browse(foo, ws = "edit_cells")

## add selective data  in ws add_row, initialize sheet with column headers and one row of data
foo <- foo %>% gs_edit_cells(ws = "add_row", input = head(iris, 1), trim = TRUE)
foo

## add the next 5 rows of data ... careful not to go too fast
for (i in 2:5) {
                  foo <- foo %>% gs_add_row(ws = "add_row", input = iris[i, ])
                  Sys.sleep(0.3)
}

gs_read(foo, ws = "add_row")    # foo is an object w/o "

## gs_add_row() can actually handle multiple rows at once, see
foo <- foo %>% gs_add_row(ws = "add_row", input = tail(iris))

## check
foo %>% gs_read(ws = "edit_cells")

foo %>% gs_read(ws = "add_row")


#Go to your Google Sheets home screen

gs_browse(foo, ws = "edit_cells")
gs_browse(foo, ws = "add_row")


#Read the function documentation for gs_edit_cells() for how to specify where the data goes,
# via an anchor cell, and in which direction, via the shape of the input or the byrow = argument.

gs_delete(foo)             # gs_grepdel() and gs_vecdel() deletion of multiple sheets.
gs_delete(boring_ss)
gs_ls()

# make gs from local excel file
#Use gs_upload() to create a new Sheet de novo from a suitable local file.
#First, we’ll write then upload a comma-delimited excerpt from the iris data.

iris %>%  head(5) %>%  write.csv("iris.csv", row.names = FALSE)
iris_ss <- gs_upload("iris.csv")   # make a goodle sheet uploaded
iris_ss %>% gs_read()

# Upload a multi-sheet Excel workbook to googlesheet drive. Slowly.
gap_xlsx <- gs_upload(system.file("mini-gap", "mini-gap.xlsx", package = "googlesheets"))
# check
gap_xlsx %>% gs_read(ws = "Asia")

#Delete files in Google Drive.
gs_vecdel(c("iris", "mini-gap"))   # Vector for deleting file
foo
#Use gs_download() to download a GS as a csv, pdf, or xlsx file to your local directory.
gs_title(foo) %>% gs_download(ws = "add_row", to = "testfoo.csv")  #??????


## is it there? yes!
read.csv("gapminder-africa.csv") %>% head()

#Download the entire spreadsheet as an Excel workbook.

gs_title("Gapminder") %>% gs_download(to = "gapminder.xlsx")


file.remove(c("gapminder.xlsx", "gapminder-africa.csv"))

# using data from google sheets
# gs_read_csv(), gs_read_listfee(), gs_read_cellfeed() all works , fast to slow, less to more versitile
gs_read_csv()
gs_read_listfeed()  #Gets data  “list feed” row-by-row, slow but supports filtering and sorting.
gs_read_cellfeed()  #filter arbitrary cells, rows, columns, and regions of the sheet
#  gs_read() invokes gs_read_cellfeed() whenever the range = non-NULL or literal = FALSE
# all are same but speed gets slower
oceania_list_feed <- gap %>% gs_read_csv(ws = "Oceania")  # tabular
oceania_list_feed <- gap %>% gs_read_listfeed(ws = "Oceania")  # rowwise data
oceania_cell_feed <- gap %>% gs_read_cellfeed(ws = "Oceania")  # the slow cell-by-cell way ("cell feed")

gs_read_cellfeed() %>% gs_simplify_cellfeed()

#with gs_read_cellfeed(), gives a data.frame with one row per cell.
# The package offers two functions
gs_reshape_cellfeed()   # i.e. a data frame
gs_simplify_cellfeed()  # makes a 1D thing, i.e. a vector

### reshape into 2D data frame
gap_3rows <- gap %>% gs_read_cellfeed("Europe", range = cell_rows(1:3))
gap_3rows %>% gs_reshape_cellfeed()


# Example: first row only
gap_1row <- gap %>% gs_read_cellfeed("Europe", range = cell_rows(1))

# convert to a named (character) vector
gap_1row %>% gs_simplify_cellfeed()

# Example: single column
gap_1col <- gap %>% gs_read_cellfeed("Europe", range = cell_cols(3))


# drop the `year` variable name, convert to integer, return un-named vector
yr <- gap_1col %>% gs_simplify_cellfeed(notation = "none")

#googlesheets provides control of data ingest in the style of readr.
#Some arguments are passed straight through to readr::read_csv() or readr::type_convert()
# and others are used internally by googlesheets
#Which cells?  gs_read(), which calls gs_read_cellfeed(), which can also be called directly.
#skip skips rows, from the top only -  Available in all read functions.
# comment can be used to skip rows inside the data rectangle if the comment string occurs at the start of the first cell. Available in all read functions.
# n_max can be used to limit the number of rows. Available in all read functions.


#Controlling data ingest, practice
df <- data_frame(thing1 = paste0("A", 2:5),
                 thing2 = paste0("B", 2:5),
                 thing3 = paste0("C", 2:5))
df$thing1[2] <- paste0("#", df$thing1[2])
df$thing2[1] <- "*"
df


## more arguments, more better
ss %>% gs_read(ws = "one-blank-row", skip = 2,
               col_names = paste0("yo ?!*", 1:3), check.names = TRUE,
               na = "*", comment = "#", n_max = 2)

## also works on list feed
ss %>% gs_read_listfeed(ws = "one-blank-row", skip = 2,
                        col_names = paste0("yo ?!*", 1:3), check.names = TRUE,
                        na = "*", comment = "#", n_max = 2)
## also works on the cell feed
ss %>% gs_read_listfeed(ws = "one-blank-row", range = cell_cols("A:Z"), skip = 2,
                        col_names = paste0("yo ?!*", 1:3), check.names = TRUE,
                        na = "*", comment = "#", n_max = 2)

## use skip to get correct result via gs_read() --> gs_read_csv()
ss %>% gs_read(ws = "two-blank-rows", skip = 2)
## or use range in gs_read() --> gs_read_cellfeed() + gs_reshape_cellfeed()
ss %>% gs_read(ws = "two-blank-rows", range = cell_limits(c(3, NA), c(NA, NA)))

ss %>% gs_read(ws = "two-blank-rows", range = cell_cols("A:C"))
ss %>% gs_read_listfeed(ws = "two-blank-rows", skip = 2)
#> Accessing worksheet titled 'two-blank-rows'. cannot cope as ws 'two-blank-rows' is empty.




ss <- gs_new("data-ingest-practice", ws_title = "simple",
             input = df, trim = TRUE) %>%
gs_ws_new("one-blank-row", input = df, trim = TRUE, anchor = "A2") %>%
gs_ws_new("two-blank-rows", input = df, trim = TRUE, anchor = "A3")

## will use gs_read_csv
ss %>% gs_read(col_names = FALSE, skip = 1)

ss %>% gs_read(col_names = letters[1:3], skip = 1)

## explicitly use gs_read_listfeed
ss %>% gs_read_listfeed(col_names = FALSE, skip = 1)

## use range to force use of gs_read_cellfeed
ss %>% gs_read_listfeed(col_names = FALSE, skip = 1, range = cell_cols("A:Z"))

## blank row causes variable names to show up in the data frame :(
ss %>% gs_read(ws = "one-blank-row")

## skip = 1 fixes it :)
ss %>% gs_read(ws = "one-blank-row", skip = 1)


## more arguments, more better
ss %>% gs_read(ws = "one-blank-row", skip = 2,
               col_names = paste0("yo ?!*", 1:3), check.names = TRUE,
               na = "*", comment = "#", n_max = 2)

## also works on list feed
ss %>% gs_read_listfeed(ws = "one-blank-row", skip = 2,
                        col_names = paste0("yo ?!*", 1:3), check.names = TRUE,
                        na = "*", comment = "#", n_max = 2)

## also works on the cell feed
ss %>% gs_read_listfeed(ws = "one-blank-row", range = cell_cols("A:Z"), skip = 2,
                        col_names = paste0("yo ?!*", 1:3), check.names = TRUE,
                        na = "*", comment = "#", n_max = 2)

## use skip to get correct result via gs_read() --> gs_read_csv()
ss %>% gs_read(ws = "two-blank-rows", skip = 2)

## or use range in gs_read() --> gs_read_cellfeed() + gs_reshape_cellfeed()
ss %>% gs_read(ws = "two-blank-rows", range = cell_limits(c(3, NA), c(NA, NA)))

ss %>% gs_read(ws = "two-blank-rows", range = cell_cols("A:C"))

## list feed can't cope because the 1st data row is empty
ss %>% gs_read_listfeed(ws = "two-blank-rows")

ss %>% gs_read_listfeed(ws = "two-blank-rows", skip = 2)

gs_delete(ss)

gs_auth()
gs_auth(new_user = TRUE)  # to force the process to begin anew

user_session_info <- gs_user()
user_session_info

################################################

# Examples summary functions dplyr
metadata %>%
group_by(cit, clade) %>%
  summarize(mean_size = mean(genome_size, na.rm = TRUE),
            min_generation = min(generation))

ARXWtTab <- anorexia %>%
  group_by(Treat) %>%
  mutate(wtDelta = Postwt - Prewt) %>%
  summarize(count = n(),
            avgWtChng = mean(wtDelta, na.rm=TRUE),
            StDevWtChng = sd(wtDelta, na.rm=TRUE))

#alt keep data frame  using sapply
library(data.table)
dt <- data.table(sapply(df, summary), keep.rownames = TRUE)


summarise(group_by(melted, sex, treatment, variable),
          mean=mean(value), sd=sd(value))
# or equivalant
melted %.% group_by(sex, treatment, variable) %.%
  summarise(mean=mean(value), sd=sd(value))



msleep %>%   count(order, vore, sort = TRUE)


msleep %>%    tally()

msleep %>%
  select(1:3) %>%
  add_tally()

msleep %>%
  select(name:vore) %>%
  add_count(vore)
#If you just want to know the number of observations count() does the job,
# but to produce summaries of the average, sum, standard deviation, minimum, maximum of the data, we need summarise()

#To use the function you just add your new column name, and after the equal sign the mathematics
# of what needs to happen: column_name = function(variable).
#You can add multiple summary functions behind each other.

msleep %>%
  group_by(vore) %>%
  summarise(n = n(), average = mean(sleep_total), maximum = max(sleep_total))


#The summarise() call works with nearly any aggregate function, and allows for additional arithmetics:
n() #- gives the number of observations
n_distinct(var) #- gives the numbers of unique values of var
# sum(var), max(var), min(var), mean(var), median(var), sd(var), IQR(var), …


msleep %>%  group_by(vore) %>%   summarise(avg_sleep_day = mean(sleep_total)/24)

#like filter, select and mutate functions, summarise() comes with three additional functions
#for doing things to multiple columns in one go:
#  summarise_all() will summarise all columns based on your further instructions
# summarise_if() requires a function that returns a boolean. If that is true, the summary instructions will be followed
# sumarise_at() requires you to specify columns inside a vars() argument for which the summary will be done.

#summarise_all() requires a function as argument, which apply to all columns.
msleep %>%
  group_by(vore) %>%
  summarise_all(mean, na.rm=TRUE)


#you can either make a function upfront, or make a function on the fly.
#The sample code will add 5 to the mean of each column.
#The function on the fly can be made by either using funs(mean(., na.rm = TRUE) + 5),
#or via a tilde: ~mean(., na.rm = TRUE) + 5.

msleep %>%
  group_by(vore) %>%
  summarise_all(~mean(., na.rm = TRUE) + 5)


# summarise_if() and summarise at needs two argument , the colomns and how

msleep %>%
  group_by(vore) %>%
  summarise_if(is.numeric, mean, na.rm=TRUE)

# rename colomns for clarity that now they repersent average_
msleep %>%
  group_by(vore) %>%
  summarise_if(is.numeric, mean, na.rm=TRUE) %>%
  rename_if(is.numeric, ~paste0("avg_", .))


#The sample code below will return the average of all columns which contain the word ‘sleep’,
#and also rename them to “avg_var” for clarity.
msleep %>%
  group_by(vore) %>%
  summarise_at(vars(contains("sleep")), mean, na.rm=TRUE) %>%
  rename_at(vars(contains("sleep")), ~paste0("avg_", .))

  arrange(desc(avg_sleep))

# for grouped
msleep %>%
    select(order, name, sleep_total) %>%
    group_by(order) %>%
    arrange(desc(sleep_total), .by_group = TRUE)    %>%
    top_n(5)   #top_n(-5) or five lowest


msleep %>%
  group_by(order) %>%
  summarise(average_sleep = mean(sleep_total), max_sleep = max(sleep_total)) %>%
  top_n(5, average_sleep)  # in multiple columns cases

msleep %>%
  slice(50:55)







# googlesheets Basic Usage  from https://cran.r-project.org/web/packages/googlesheets/vignettes/basic-usage.html






# copy an entire Sheet with gs_copy() and rename one with gs_rename()
# create new sheet with gs_new()

boring_ss <- gs_new("boring", ws_title = "iris-gs_new", input = head(iris), trim = TRUE, verbose = FALSE)
boring_ss

boring_ss   %>%   gs_read()


# Add a new worksheet to an existing Google Sheet : gs_ws_new() to add some mtcars data as a second worksheet to boring_ss.

boring_ss <- boring_ss %>%
  gs_ws_new(ws_title = "mtcars-gs_ws_new", input = head(mtcars), trim = TRUE, verbose = FALSE)

boring_ss %>% gs_read(ws = 2)

# Rename  gs_ws_rename()   or delete worksheets  gs_ws_delete()

boring_ss <- boring_ss %>%  gs_ws_delete(ws = 2)  %>%  gs_ws_rename(to = "iris")

boring_ss

# gs_edit_cells() can write into an arbitrary cell rectangle
# gs_add_row() can add a new row to the bottom of an existing cell rectangle
# both are slow  better use gs_upload()

#We create a new Sheet, foo, and set up some well-named empty worksheets to practice with.

foo <- gs_new("foo") %>%
  gs_ws_rename(from = "Sheet1", to = "edit_cells") %>%
  gs_ws_new("add_row")

foo


## add first six rows of iris data (and var names) into a blank sheet
foo <-  gs_edit_cells(ss = foo, ws = "edit_cells", input = head(iris), trim = TRUE)


## initialize sheet with column headers and one row of data
## the list feed is picky about this
foo <- foo %>%
  gs_edit_cells(ws = "add_row", input = head(iris, 1), trim = TRUE)


## add the next 5 rows of data ... careful not to go too fast
for (i in 2:6) {   foo <-  foo  %>%   gs_add_row(ws = "add_row", input = iris[i, ])
Sys.sleep(0.3)
}


## gs_add_row() will actually handle multiple rows at once
foo <- foo %>%   gs_add_row(ws = "add_row", input = tail(iris))

## let's inspect our work
foo %>% gs_read(ws = "edit_cells")


foo %>% gs_read(ws = "add_row")

# you will find in google sheet
# You could also use gs_browse() to take you directly to those worksheets.

gs_browse(foo, ws = "edit_cells")
gs_browse(foo, ws = "add_row")

#Read documentation for gs_edit_cells() to specify where the data goes, via an anchor cell,
# and in which direction, via the shape of the input or the byrow = argument.

# delete sheet

gs_delete(foo)

# also gs_grepdel() and gs_vecdel()  for multi del

# Use gs_upload() to create a new Sheet de novo from a suitable local file.

iris %>%   head(5) %>%  write.csv("iris.csv", row.names = FALSE)

iris_ss <- gs_upload("iris.csv")

gs_ls()


iris_ss

iris_ss %>% gs_read()

file.remove("iris.csv")


# Now we’ll upload a multi-sheet Excel workbook. Slowly.

gap_xlsx <- gs_upload(system.file("mini-gap", "mini-gap.xlsx", package = "googlesheets"))

gap_xlsx


gap_xlsx %>% gs_read(ws = "Asia")



#And we clean up after ourselves on Google Drive.

gs_vecdel(c("iris", "mini-gap"))

## achieves same as:
## gs_delete(iris_ss)
## gs_delete(gap_xlsx)

#Use gs_download() to download a Google Sheet as a csv, pdf, or xlsx file.
# gs_download() to download a Google Sheet as a csv, pdf, or xlsx file.
# Downloading the spreadsheet as a csv file has default first worksheet  unless another worksheet is specified.

gs_ws_ls(locofail1718)

gs_title("locofail1718") %>%  gs_download(ws = "HHP system wise", to = "HHPsystems.csv")
#### successfully downloaded: as /Users/gk/r4ds/HHPsystems.csv

## Check , yes !
read.csv("HHPsystems.csv") %>% head()

# To Download the entire spreadsheet as an Excel workbook.

gs_title("locofail1718") %>%   gs_download(to = "locofail1718.xlsx")
ls()   #list all files including the just downloaded

file.remove(c("gapminder.xlsx", "gapminder-africa.csv"))   # remove files from your computer dir

######

# Read data, but with more control     Specify the consumption method


gs_read_csv()          # faster, when data ractanguler and gets tbl_df / data.frame. Also gs_read_listfeed()
gs_read_listfeed()     # Gets data via the “list feed”, when you work data row-by-row.
# for ractanguler data but supports some query parameters for sorting and filtering
gs_read_cellfeed()     # Get data via “cell feed” when you work data cell-by-cell.
# you need arbitrary cells, rows, columns, and regions
# or when you want to get formulas or cell contents without numeric formatting applied, e.g. rounding.
# invoked by gs_read() whenever range = argument is non-NULL or literal = FALSE.
# returns tbl_df with one row per cell.
# You can target specific cells via the range argument.
# see demos for gs_reshape_cellfeed() and gs_simplify_cellfeed() which help with post-processing.



HHPsyscsv <- gs_title("locofail1718") %>% gs_read_csv(ws = "HHP system wise")
HHPsyscsv


# using list feed
HHPsyscsv2 <- gs_title("locofail1718") %>% gs_read_listfeed(ws = "HHP system wise")

lf17gslink <- gs_title("locofail1718")    # better

HHPsyscsv2 <- lf17gslink %>% gs_read_listfeed(ws = "HHP system wise")
HHPsyscsv2

# using cell feed

HHPsys_cell_feed <-lf17gslink %>% gs_read_cellfeed(ws = "HHP system wise")

HHPsys_cell_feed        # A tibble: 1,665 x 7

#  after cell feed with  gs_read_cellfeed(), you get a data.frame back with one row per cell.
# The package offers two functions to post-process this into something more useful:

gs_reshape_cellfeed()     # makes a 2D thing, i.e. a data frame
gs_simplify_cellfeed()    # makes a 1D thing, i.e. a vector


## reshape into 2D data frame
lf2_3rows <- lf17gslink %>% gs_read_cellfeed("HHP system wise", range = cell_rows(1:3))

lf2_3rows   # interesting struc

lf2_3rows %>% gs_reshape_cellfeed()     # converts in 2x10 tibble


lf2_3rows %>% gs_simplify_cellfeed()  # ???

lf2_1col <- lf17gslink %>% gs_read_cellfeed("HHP system wise", range = cell_cols(3))
lf2_1col

lf2simply <- lf2_1col %>% gs_simplify_cellfeed(notation = "none")
lf2simply



#googlesheets provides control of data ingest in the style of readr






# learning from http://www.rpubs.com/williamsurles/293454

#inner_join(x, y):
#Return all rows from x where there are matching values in y, and all columns from x and y.
#If there are multiple matches between x and y, all combination of the matches are returned. This is a mutating join.

#semi_join(x, y): Return all rows from x where there are matching values in y, keeping just columns from x.
#A semi join differs from an inner join because an inner join will return one row of x for each matching row of y,
# where a semi join will never duplicate rows of x. This is a filtering join.


#left_join(x, y): Return all rows from x, and all columns from x and y.
# If there are multiple matches between x and y, all combination of the matches are returned. This is a mutating join.


#anti_join(x, y): Return all rows from x where there are not matching values in y, keeping just columns from x. This is a filtering join.

#inner_join(x, y): Return all rows from x where there are matching values in y, and all columns from x and y.
# If there are multiple matches between x and y, all combination of the matches are returned. This is a mutating join.


# semi_join(x, y): Return all rows from x where there are matching values in y, keeping just columns from x.
#A semi join differs from an inner join because an inner join will return one row of x for each matching row of y,
#where a semi join will never duplicate rows of x. This is a filtering join.

#left_join(x, y): Return all rows from x, and all columns from x and y.
#If there are multiple matches between x and y, all combination of the matches are returned. This is a mutating join.


#anti_join(x, y): Return all rows from x where there are not matching values in y, keeping just columns from x. This is a filtering join.


#full_join(x, y): Return all rows and all columns from both x and y. Where there are not matching values, returns NA for the one missing. This is a mutating join.



# Controlling how the tables are matched

# NULL, the default. dplyr will will use all variables that appear in both tables, a natural join.
#For example, the flights and weather tables match on their common variables: year, month, day, hour and origin.

flights2 %>% left_join(weather)


#A character vector, by = "x". Like a natural join, but uses only some of the common variables.
#For example, flights and planes have year columns, but they mean different things so we only want to join by tailnum.

flights2 %>% left_join(planes, by = "tailnum")

#A named character vector: by = c("x" = "a"). This will match variable x in table x to variable a in table b.
#The variables from use will be used in the output.
#Each flight has an origin and destination airport, so we need to specify which one we want to join to:

flights2 %>% left_join(airports, c("dest" = "faa"))



#inner_join(x, y) only includes observations that match in both x and y.

#left_join(x, y) includes all observations in x, regardless of whether they match or not. Most commonly used as it ensures that you don't lose observations from your primary table.
#right_join(x, y) includes all observations in y. It's equivalent to left_join(y, x), but the columns will be ordered differently.
#full_join() includes all observations from x and y.
#The left, right and full joins are collectively know as outer joins



#Filtering joins match obserations like mutating joins, but affect the observations, not the variables. There are two types:
# semi_join(x, y) keeps all observations in x that have a match in y.
# anti_join(x, y) drops all observations in x that have a match in y.
# These are most useful for diagnosing join mismatches. For example, there are many flights in the nycflights13 dataset that don't have a matching tail number in the planes table:





###########
# time
library(lubridate)
today()
now()

#These functions also take unquoted numbers
ymd("2017-01-31")         #> [1] "2017-01-31"
ymd(20170131)
mdy("January 31st, 2017") #> [1] "2017-01-31"
dmy("31-Jan-2017")        #> [1] "2017-01-31"
ymd_hms("2017-01-31 20:11:59")       #> [1] "2017-01-31 20:11:59 UTC"
mdy_hm("01/31/2017 08:01")

#You can also force the creation of a date-time from a date by supplying a timezone:
ymd(20170131, tz = "UTC")

# eg mutate(departure = make_datetime(year, month, day, hour, minute))
make_datetime(year, month, day, hour, minute)
make_date()


# make funtion
#make_datetime_100 <- function(year, month, day, time) {make_datetime(year, month, day, time %/% 100, time %% 100) }
#select(origin, dest, ends_with("delay"), ends_with("time"))
as_datetime(today())
as_date(now())
#date/times as numeric offsets from the “Unix Epoch”, 1970-01-01
as_datetime(60 * 60 * 10)   # > [1] "1970-01-01 10:00:00 UTC"
as_date(365 * 10 + 2)       # > [1] "1980-01-01"

datetime <- ymd_hms("2016-07-08 12:34:56")
year(datetime)  #> [1] 2016
month(datetime) #> [1] 7
mday(datetime)  #> [1] 8
yday(datetime)  #> [1] 190
wday(datetime)  #> [1] 6

#month() and wday() ,  set label = TRUE for abbreviated name. Set abbr = FALSE to return the full name.

flights_dt %>%  mutate(minute = minute(dep_time)) %>%
  group_by(minute) %>%  summarise(avg_delay = mean(arr_delay, na.rm = TRUE),n = n()) %>%
  ggplot(aes(minute, avg_delay)) + geom_line()

summarise(  avg_delay = mean(  arr_delay, na.rm = TRUE ),   n = n()   )

# floor_date(), round_date(), and ceiling_date()  #floor_date(dep_time, "week")

(datetime <- ymd_hms("2016-07-08 12:34:56"))
year(datetime) <- 2020  #datetime  #> [1] "2020-07-08 12:34:56 UTC"

hour(datetime) <- hour(datetime) + 1

#rather than modifying in place, you can create a new date-time with update()
update(datetime, year = 2020, month = 2, mday = 2, hour = 2) #> [1] "2020-02-02 02:34:56 UTC"

ymd("2015-02-01") %>% update(hour = 400)  #> [1] "2015-02-17 16:00:00 UTC"

#Durations
h_age <- today() - ymd(19791014)
h_age
as.duration(h_age)

dseconds(15)    #> [1] "15s"
dminutes(10)    #> [1] "600s (~10 minutes)"
dhours(c(12, 24))   #> [1] "43200s (~12 hours)" "86400s (~1 days)"
ddays(0:5)      #> [1] "0s"  "86400s (~1 days)"  "172800s (~2 days)" "259200s (~3 days)" "345600s (~4 days)" "432000s (~5 days)"
dweeks(3)         #> [1] "1814400s (~3 weeks)"
dyears(1)         #> [1] "31536000s (~52.14 weeks)"
#You can add and multiply durations:
2 * dyears(1)       #> [1] "63072000s (~2 years)"
dyears(1) + dweeks(12) + dhours(15)   #"38847600s (~1.23 years)"

(tomorrow <- today() + ddays(1))
last_year <- today() - dyears(1)

# dyears(1) = ddays(365)
next_year <- today() + years(1)
(today() %--% next_year) / ddays(1)

(today() %--% next_year) %/% days(1)

Sys.timezone()
x4a <- with_tz(x4, tzone = "Australia/Lord_Howe")
x4b <- force_tz(x4, tzone = "Australia/Lord_Howe")

#Subsetting



## DPLYR

bands2 <- left_join(bands, artists, by = c("first", "last"))

bands3 <- right_join(artists, bands, by = c("first", "last"))  #setequal(bands2, bands3) would be true

# left_join - prioritizes left dataset
# right_join - prioritizes right dataset
# inner_join - only retains rows in both datasets
# full_join - retains all rows
# Use %>% (pipes) to string together these joins

#    Find guitarists in bands dataset (don't change)
#temp <- left_join(bands, artists, by = c("first", "last"))
#temp <- filter(temp, instrument == "Guitar")
#select(temp, first, last, band)

# Reproduce code above using pipes
#bands %>% left_join(artists, by = c("first","last")) %>% filter(instrument == "Guitar") %>%   select(first, last, band)

# Create one table that combines all information
collection <- artists %>%
  full_join(bands, by = c("first","last")) %>%
  full_join(songs, by = c("first","last")) %>%
  full_join(albums, by = c("album", "band"))

#Semi-joins  Apply a semi-join
# View the output of semi_join()
artists %>%   semi_join(songs, by = c("first", "last"))


# Create the same result
artists %>%
  right_join(songs, by = c("first", "last")) %>%
  filter(!is.na(instrument)) %>%
  select(first, last, instrument)

#Exploring with semi-joins
# Collect the albums made by a band
albums %>%
  semi_join(bands, by = "band") %>%
  nrow()  # Count the albums made by a band

## With semi-join
tracks %>%
  semi_join(matches,by = c("band", "year", "first"))


# With dply filter statement
tracks %>%
  filter( (band == "The Beatles" & year == 1964 & first == "Paul") |
          (band == "The Beatles" & year == 1965 & first == "John") |
          (band == "Simon and Garfunkel" & year == 1966 & first == "Paul")      )


#Apply an anti-join
# Return rows of artists that don't have bands info
artists %>% anti_join(bands, by = c("first","last"))


labels %>%
  anti_join(albums, by = c("album"))


# Check your understanding
songs %>%
  # Find the rows of songs that match a row in labels
  semi_join(labels, by = c("album")) %>%
  # Number of matches between labels and songs
  nrow()

## set operations

aerosmith %>%
  # Create the new dataset using a set operation
  union(greatest_hits) %>%
  # Count the total number of songs
  nrow()

#Greatest hits  Which songs from Aerosmith made it onto Greates Hits?
# Create the new dataset using a set operation
aerosmith %>%   intersect(greatest_hits)


#Live! Bootleg songs Which songs are on Live! Bootleg but not on Greatest Hits?
# Notice that the length of songs may be different when they are performed live.
# Select the song names from live
live_songs <- live %>%   select(song)

# Select the song names from greatest_hits
greatest_songs <- greatest_hits %>%   select(song)

# Create the new dataset using a set operation
live_songs %>%   setdiff(greatest_songs)


#Multiple operations: Which songs appear on one of Live! Bootleg or Greatest Hits, but not both?
# Select songs from live and greatest_hits
live_songs <- live %>% select(song)
greatest_songs <- greatest_hits %>% select(song)

# Return the songs that only exist in one dataset
all_songs <- union(live_songs, greatest_songs)
both_songs <- intersect(live_songs, greatest_songs)
one_songs <- setdiff(all_songs, both_songs)
one_songs

#
#Comparing datasets :  if one data set is the same as another dataset
# dplyr's setequal will do this easily
# base R's identical is will only return true if the datasets have the exact same rows in the exact same order
#Recap:
#  Mutating Joins:
# left_join, right_join, inner_join, full_join
# Filtering Joins:    semi_join, anti_join,

#Set Operations:    union, intersect, setdiff, Comparisions
# setequal


# Check if same order: definitive and complete
#identical(definitive, complete)  will give TRUE or FALSE

# Check if any order: definitive and complete
#setequal(definitive, complete)     ## FALSE: Different number of rows

## Songs in definitive but not complete
# setdiff(definitive, complete)

## Songs in complete but not definitive
# setdiff(complete, definitive)

#Apply setequal again
#If the datasets have the same columns and all are used in the key, then intersect is analagous to semi_join
# In the same scenario setdiff is similar to anti_join
# Return songs in definitive that are not in complete
#definitive %>%   anti_join(complete, by = c("song", "album"))


## Return songs in complete that are not in definitive
complete %>%   anti_join(definitive, by = c("song", "album"))

# Check if same order: definitive and union of complete and soundtrack
union(complete, soundtrack) %>%   identical(definitive)

# Check if any order: definitive and union of complete and soundtrack
union(complete, soundtrack) %>%   setequal(definitive)

#Assemblying the data Binds
#Base R binds  :  rbind, cbind
# dplyr binds  : bind_rows, bind_cols


#Advantages of dplyr versions : faster, return a tibble
# .id argument in bind_rows allows you to pass in a name for each source dataframe
# rbind return an error if the column names do not match exactly.
# bind_rows will create a column for each unique column and distribute NAs appropriately




## Remove NA's from key before joining
two_songs %>% filter(!is.na(movie)) %>% inner_join(singers, by = "movie")


#elvis_movies %>%
# Left join elvis_songs to elvis_movies by this column
left_join(elvis_songs, by = c("name" = "movie")) %>%
  # Rename columns
  rename(movie = name, song = name.y)

#movie_years %>%
# Left join movie_directors to movie_years
left_join(movie_directors, by = c("movie" = "name")) %>%
  # Arrange the columns using select()
  select(year, movie, artist = name, director, studio)



# Joining multiple tables # could do a bunch of left_joins in a row
# The purrr package has a function reduce() that lets us apply a function recursively to a list of datasets
# it apply R functions to data in efficient ways
#Join multiple tables #Using reduce to do a left_quick join of all tables
# Load the purrr library

library(purrr)

# Place supergroups, more_bands, and more_artists into a list
list(supergroups, more_bands, more_artists) %>%
  # Use reduce to join together the contents of the list
  reduce(left_join, by = c("first" = "first","last" = "last"))




#Populating Missing Dates with Complete and Fill Functions in R and Exploratory
#https://blog.exploratory.io/populating-missing-dates-with-complete-and-fill-functions-in-r-and-exploratory-79f2a321e6b5
# from tidyr
#complete(Date = seq.Date(<start_date>, <end_date>, by=<date_unit>))
complete(Date = seq.Date(min(Date), max(Date), by="day"))

discount_data_df %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))

#The Product column has all the NAs, but we want to fill them with either A or B.
#This is actually easy to address, again, thanks to complete function.

# Add Product, it fill all combinations of prodect
complete(Date = seq.Date(min(Date), max(Date), by="day"), Product)
# use fill function
discount_data_df %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day")) %>%
  fill(`Discount Rate`)

#Fill Missing Values within Each Group
#This is when the group_by command from the dplyr

discount_data_df %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day")) %>%
  group_by(Product) %>%
  fill(`Discount Rate`)

#forcats::fct_reorder(). It will basically sort the factors specified in the 1st arg, according to the values in the 2nd arg after applying a specified function

theTable <- data.frame(
  Position = c("Zoalkeeper", "Zoalkeeper", "Defense", "Defense", "Defense", "Striker"),
  Name     = c("James", "Frank","Jean", "Steve","John", "Tim")       )

p1 <- ggplot(theTable, aes(x = Position)) + geom_bar()
p2 <- ggplot(theTable, aes(x = fct_infreq(Position))) + geom_bar()
p3 <- ggplot(theTable, aes(x = fct_rev(fct_infreq(Position)))) + geom_bar()

gridExtra::grid.arrange(p1, p2, p3, nrow=3)
