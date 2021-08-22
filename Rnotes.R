







install.packages("bookdown")
library(bookdown)
library(NHSRdatasets)
ae <- NHSRdatasets::ae_attendances
write.csv(ae, "ae_nhsr.csv", row.names = FALSE) #Set row names to false
#Use data.table to read in the document
ae_dt <- fread("ae_nhsr.csv")
#Create a random uniform distribution
big_data <- data.frame(BigNumbers=runif(matrix(10000000, 10000000)))
write.csv(big_data, "bigdata.csv")
# Start to benchmark using system.time
base_metrics <- system.time(  read.csv("bigdata.csv") )
dt_metrics  <- system.time(  data.table::fread("bigdata.csv")  )
print(base_metrics)
print(dt_metrics)


#Convert base data.frame to data.table
ae_dt <- as.data.table(ae)   
class(ae_dt)  
ae_copy <- ae
data.table::setDT(ae_copy)  #Using the setDT command
class(ae_copy)

data.table::setDF(ae_copy)  # Converting this back to a data.frame
class(ae_copy)

chained <- ae_dt_copy[, .(mean_attendance=mean(attendances),
                          mean_breaches=mean(breaches), 
                          sum_attendances=sum(attendances)),
                      by=.(org_code)][order(org_code)]
# Adding square brackets, instead of %>%, chains the ordering
# Here we create a group by and summarise function and at the end we add another
# Command sequence i.e. group by org code, summarise the mean and then order by ord code
glimpse(chained)





library("imager")
original = imager::load.image("owl.png")
d = dim(original)[1:2]
d
########
#install.packages(data.table)
library(data.table)



######
seq.Date( as.Date("2018-01-31"), length.out = 6, by = "month")


lubridate::today()  # today's date
lubridate::now()    # current date-time
lubridate::tz(now())       # uses local system setting as default
lubridate::Sys.timezone()  # show local system setting
library(tidyverse)
library(lubridate)
# Parsing dates and times
lubridate::ymd("06 02 04")
lubridate::ymd_hms("2020-04-01 10:30:13")

dt <- c("10:05 29/02/2020", "20:10 24/12/2020")
tb <- tibble::tibble(datetime = dt)
tb2 <- tb %>% 
  separate(datetime, into = c("time", "day"), sep = " ", remove = FALSE) %>% 
  separate(time,     into = c("hour", "min"), sep = ":", remove = FALSE) %>% 
  mutate(dt_1 = paste(day, time),  # a character string
         dt_2 = dmy_hm(dt_1),      # a <dttm> object
         ti_2 = hm(time)           # a <period> object
  )

tb2


lubridate::make_date(year = 2020, month = 7, day = 13) 
lubridate::make_date(year = 2020, month = "007", day = "013")  # even mixed data type char and number
lubridate::make_date(year = 2020, month = 7)   # day = 1# Note defaults for missing elements:
lubridate::make_date(month = 2, day = 13)      # year = 1970
lubridate::is.Date(make_date(year = 2020))
lubridate::is.POSIXct(make_date(year = 2020))

lubridate::make_datetime(year = 2020, month = 7, day = 13, hour = 10, min = 30, sec = 45, tz = "Europe/Zurich") # tx default UTC
lubridate::make_datetime(year = 2020)  # "2020-01-01 UTC"
lubridate::make_datetime(sec = 33   )  # "1970-01-01 00:00:33 UTC"
# amkedatetotime only expect numberic

t_end   <- lubridate::ceiling_date(now(), "year")
t_end

# Dates from numeric inputs:
lubridate::as_date(0)    # Unix epoch #> [1] "1970-01-01"
lubridate::as_date(1)    # increment: +1 day  #> [1] "1970-01-02"
lubridate::as_date(365)  # +1 year


# Get names instead of numbers:
lubridate::month(tnow, label = TRUE, abbr = TRUE)   # month in year (name)
lubridate::wday( tnow, label = TRUE, abbr = FALSE)  # day of week (name)

# corresponding difftime() function (see Section 10.2.4) offers a range of units varying from “secs” to “weeks”
difftime()
# For time spans exceeding a few months, the duration class provided by lubridate is a better 



# Define interval by start %--% end:
i4 <- tm_911 %--% tm_now

as.duration(i1)
#> [1] "605675158.68614s (~19.19 years)"
as.period(i1)
#> [1] "19y 2m 9d 2H 5M 58.6861400604248S"



# # Repurpose date column to be used as dataframe index
df <- column_to_rownames(df, var = "date")

install.packages("tesseract")
library(tesseract)
tesseract_download('fra')  # If you want to OCR french text:

library(magick)
image_read("https://jeroen.github.io/images/birds.jpg") %>%
  image_crop('1200x300+100+1700') %>%
  image_ocr() %>%
  cat()

install.packages("opencv")
library(opencv)

# ocv_face(image), ocv_facemask(image), ocv_read(path), ocv_write(image, path), 
# ocv_destroy(image),  ocv_bitmap(image), ocv_edges(image), ocv_picture()
# ocv_resize(image, width = 0, height = 0), ocv_mog2(image), ocv_knn(image)
# ocv_hog(image) , ocv_blur(image, ksize = 5), ocv_sketch(image, color = TRUE)
# ocv_stylize(image), ocv_markers(image), ocv_info(image), ocv_copyto(image, target, mask)
# ocv_display(image), ocv_video(filter), ocv_grayscale(image), ocv_version()

# Silly example
mona <- ocv_read('https://jeroen.github.io/images/monalisa.jpg')
ocv_edges(mona) # Edge detection
ocv_markers(mona)

faces <- ocv_face(mona)# Find face
facemask <- ocv_facemask(mona)# To show locations of faces
attr(facemask, 'faces')
#ocv_destroy(mona)# This is not strictly needed

ocv_read('NAIRbatch.jpg') %>% ocv_face()   # ocv_hog()
ocv_read('peoples2.jpg') %>% ocv_hog()
# Edge detection
ocv_read('peoples2.jpg') %>%   ocv_edges()
ocv_read('peoples2.jpg') %>%  ocv_markers()

ocv_read('peoples3.jpg') %>%  ocv_face() # detected but just 4 out of 10
ocv_read('peoples2.jpg') %>% ocv_sketch()

############### keras
library(keras)
# Pretrained VGG16 model
model <- application_vgg16( weights = "imagenet",  include_top = TRUE )
# Convert images to Keras array
get_img <- function(x) {
  arrays <- lapply(x, function(path) {
    img <- image_load(path, target_size = c(224,224))
    x <- image_to_array(img)
    x <- array_reshape(x, c(1, dim(x)))
    x <- imagenet_preprocess_input(x)
  })
  do.call(abind::abind, c(arrays, list(along = 1)))
}



######################

stingr::str_split(words, " ")[[1]]
str_split(words, boundary("word"))[[1]]
str_extract_all("The Cat in the Hat", regex("[a-z]+", TRUE))
str_subset(string, pattern, negate = FALSE)
str_subset(fruit, "a$")
str_subset(fruit, "^p", negate = TRUE)# Returns elements that do NOT match
# fixed(pattern, ignore_case = FALSE)
# coll(pattern, ignore_case = FALSE, locale = "en", ...)
# boundary(type = c("character", "line_break", "sentence", "word"), skip_word_none = NA, ... )
# regex(  pattern,  ignore_case = FALSE,  multiline = FALSE,  comments = FALSE,  dotall = FALSE,  ... )

see <- function(rx) str_view_all("abc ABC 123\t.!?\\(){}\n", rx)
see("a")

# install.packages("here")
here::i_am("README.Rmd")
here::here("inst", "demo-project", "data", "penguins.csv")
#> [1] "/home/kirill/git/R/here/inst/demo-project/data/penguins.csv"
readr::write_csv(palmerpenguins::penguins, 
                 here::here("inst", "demo-project", "data", "penguins.csv")  )


library(snakecase)  ## install.packages("snakecase")
string <- c("lowerCamelCase", "ALL_CAPS", "I-DontKNOWWhat_thisCASE_is")

snakecase::to_any_case( string                 )  # convert to snake case or with option any case
snakecase::to_any_case( string, case = "parsed") # "lower_camel", "upper_camel", "all_caps", "lower_upper", "upper_lower", "sentence" and "mixed", which are based on "parsed" case:
snakecase::to_snake_case(string)
snakecase::to_snake_case( c("SomeBAdInput", "someGoodInput") ) %>%  dput()
## c("some_b_ad_input", "some_good_input")

library(tidyverse)
starwars
starwars %>% group_by(eye_color) %>% tally() # or
starwars %>% count(sex, eye_color)
starwars %>% add_count(eye_color, wt = birth_year) # mutating add_count
starwars %>% add_tally(wt = birth_year)


vroom::vroom("mtcars.tsv", col_types = list(cyl = "i",   gear = "f",   hp = "i",    disp = "_",   drat = "_",   vs = "l",   am = "l",   carb = "i") )

#if (!require(devtools)) install.packages("devtools")
# devtools::install_github("boxuancui/DataExplorer")
library(DataExplorer)
create_report(airquality)              # To get a report for the airquality dataset:
create_report(diamonds, y = "price")   # To get a report for the diamonds dataset with response variable price:

# You may also run each function individually for your analysis, e.g.,

introduce(airquality)           ## View basic description for airquality data
plot_intro(airquality)          ## Plot basic description for airquality data


plot_missing(airquality)        ## View missing value distribution for airquality data

plot_bar(diamonds)                 ## Left: frequency distribution of all discrete variables
plot_bar(diamonds, with = "price") ## Right: `price` distribution of all discrete variables
plot_bar(diamonds, by = "cut")     ## View frequency distribution by a discrete variable
plot_histogram(diamonds)       ## View histogram of all continuous variables


## View estimated density distribution of all continuous variables
plot_density(diamonds)

## View quantile-quantile plot of all continuous variables
plot_qq(diamonds)


plot_qq(diamonds, by = "cut")                ## View quantile-quantile plot of all continuous variables by feature `cut`
plot_correlation(diamonds)                   ## View overall correlation heatmap
plot_boxplot(diamonds, by = "cut")           ## View bivariate continuous distribution based on `cut`
plot_scatterplot(split_columns(diamonds)$continuous, by = "price", sampled_rows = 1000L)## Scatterplot `price` with all other continuous features
plot_prcomp(diamonds, maxcat = 5L)           ## Visualize principal component analysis


# To make quick updates to your data:
group_category(diamonds, feature = "clarity", threshold = 0.2, update = TRUE)   ## Group bottom 20% `clarity` by frequency
group_category(diamonds, feature = "clarity", threshold = 0.2, measure = "price", update = TRUE)## Group bottom 20% `clarity` by `price`
dummify(diamonds)                                                         ## Dummify diamonds dataset
dummify(diamonds, select = "cut")


df <- data.frame("a" = rnorm(260), "b" = rep(letters, 10))           ## Set values for missing observations
df[sample.int(260, 50), ] <- NA
set_missing(df, list(0L, "unknown"))

## Update columns
update_columns(airquality, c("Month", "Day"), as.factor)
update_columns(airquality, 1L, function(x) x^2)

## Drop columns
drop_columns(diamonds, 8:10)
drop_columns(diamonds, "clarity")






