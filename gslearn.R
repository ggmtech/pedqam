


# You can review the differences with a:

!git log HEAD..origin/master
!git reset --hard origin/master

git merge origin/master

git rebase origin/master

The git pull command provides a shorthand way to fetch from origin and rebase local work on it:
  
$ git pull --rebase

git pull --rebase origin/master  # is a single command that can help you most of the time. Pulls the commits from the origin/master and applies your changes upon the newly pulled branch history.
git reset --hard origin/master

git merge master
git push origin master -f  # -f (force) switch. This deleted the "bad changes" that had been pushed to origin/master by mistake and now the local and remote branches are in sync.

To view the differences:
  
  git difftool --dir-diff master origin/master

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
faa_mapping <- faa_mapping_html %>%   html_node("table") %>%   rvest::html_table()


search <- html_form(read_html("http://www.google.com"))[[1]]
set_values(search, q = "My little pony")
set_values(search, hl = "fr")


###############################################


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

msleep %>%  slice(50:55)








#Subsetting

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
