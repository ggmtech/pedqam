# Use standard tidyverse tools
library(readr)
library(dplyr)
library(tidyverse)
library(maps)

agc<-read.csv("https://raw.githubusercontent.com/openrelia/WeibullR.gallery/master/data/acid_gas_compressor.csv", header=T)$agc
agc

s1 <- source("https://raw.githubusercontent.com/openrelia/WeibullR.gallery/master/scripts/contour_challenge.r")
cat(s1 )
s1

source("https://raw.githubusercontent.com/openrelia/WeibullR.gallery/master/scripts/manipulate_contour_challenge.r")



source("https://raw.githubusercontent.com/openrelia/WeibullR.gallery/master/scripts/Fig3.13ln.r")

source("https://raw.githubusercontent.com/openrelia/WeibullR.gallery/master/scripts/Fig3.13multi.r")

source("https://raw.githubusercontent.com/openrelia/WeibullR.gallery/master/scripts/bathtub_life.r")



source("https://raw.githubusercontent.com/openrelia/WeibullR.gallery/master/scripts/competing_modes.r")


source("https://raw.githubusercontent.com/openrelia/WeibullR.gallery/master/scripts/linearized3p.r")


source("https://raw.githubusercontent.com/openrelia/WeibullR.gallery/master/scripts/compare_6mp.r")






############ OPENCAGE_KEY=3721d0dffdf54328b9dfc5eacd95005d

# usethis::edit_r_environ()
# install.packages("opencage")
#remotes::install_github("ropensci/opencage", force = TRUE)
library("opencage")   ## opencage some errors 

oc_config(
    key = Sys.getenv("OPENCAGE_KEY"),
    rate_sec = getOption("oc_rate_sec", default = 1L),
    no_record = getOption("oc_no_record", default = TRUE),
    show_key = getOption("oc_show_key", default = FALSE),
    ...
)


oc_forward_df(placename = "Sarzeau")
# Or turn a set of coordinates into the name and address of the location:
oc_reverse_df(latitude = 51.5034070, longitude = -0.1275920)

# All geocoding functions are vectorised
# oc_forward_df() returns three columns: oc_lat (for latitude), oc_lon (for longitude), and oc_formatted (the formatted address). 
# If you specify oc_forward_df(output = all),  all result columns often quite extensive. 

# pass a data frame to oc_forward_df(). results columns are added to the input data frame, else set bind_cols = FALSE.
concert_df %>% oc_forward_df(location)

oc_reverse_df(latitude = 51.5034070, longitude = -0.1275920)
# CRS must adhere  WGS 84 (also known as EPSG:4326) coordinate reference system in decimal format.


##############
#install.packages('tidygeocoder')
# devtools::install_github("jessecambon/tidygeocoder")
library(dplyr)
library(tibble)
library(tidygeocoder)

# create a dataframe with addresses
some_addresses <- tribble(
    ~name,                  ~addr,
    "White House",          "1600 Pennsylvania Ave NW, Washington, DC",
    "Transamerica Pyramid", "600 Montgomery St, San Francisco, CA 94111",     
    "Willis Tower",         "233 S Wacker Dr, Chicago, IL 60606"                                  
     )

some_addresses <- tribble(
    ~name,                  ~addr,         ~city,  ~country,
    "Rail Bhawan",          "Rail Bhawan", "New Delhi", "India",
    "RDSO",                 "Manak Nagar",  "Lucknow", "India",     
    "Baroda House",         "Baroda House, New Delhi", "new delhi", "India"      ,
    "SGRH",                 "Rajinder Nagar", "New Delhi", "India"
)
some_addresses

# geocode the addresses
lat_longs <- some_addresses %>%
             geocode(addr, method = 'osm', lat = latitude , long = longitude)

lat_longs

library(ggplot2)
library(maps)
library(ggrepel)
library(sf)

plot(lat_longs$longitude, lat_longs$latitude)

ggplot(lat_longs, aes(longitude, latitude), color = "grey99") +
    borders("state") + geom_point()  # +
    geom_label_repel(aes(label = name)) + theme_void()

# reverse geocode
reverse <- lat_longs %>%
    reverse_geocode(lat = latitude, long = longitude, method = 'osm',
                    address = address_found, full_results = TRUE) %>%
    select(-addr, -licence)


################
#----- Download and unzip files -----
zipFile <- file.path(tempdir(), "RIDBFullExport_V1_CSV.zip")
        download.file(
        url = "https://ridb.recreation.gov/downloads/RIDBFullExport_V1_CSV.zip",
        destfile = zipFile
         )
    # trying URL
    # 'https://ridb.recreation.gov/downloads/RIDBFullExport_V1_CSV.zip'
    # Content type 'application/zip' length 173491403 bytes (165.5 MB)
    # ==================================================
    # downloaded 165.5 MB
unzip(zipFile)
#    ----- Read in data -----
        # The "Facility" dataset has some longitude/latitude problems
        # > list.files(pattern = "^Facil")
        # [1] "Facilities_API_v1.csv" "FacilityAddresses_API_v1.csv"
Facilities <- readr::read_csv("Facilities_API_v1.csv")
    # Parsed with column specification:
    # cols(
    #   .default = col_character(),
    #   LegacyFacilityID = col_double(),
    #   OrgFacilityID = col_double(),
    #   ParentOrgID = col_double(),
    #   ParentRecAreaID = col_double(),
    #   FacilityLongitude = col_double(),
    #   FacilityLatitude = col_double(),
    #   Reservable = col_logical(),
    #   Enabled = col_logical(),
    #    LastUpdatedDate = col_date(format = "")
    # )
    # See spec(…) for full column specifications.
    # Warning: 3658 parsing failures.
FacilityAddresses <- readr::read_csv("FAcilityAddresses_API_v1.csv")
    # Parsed with column specification:
    # cols(
    #   FacilityAddressID = col_character(),
    #   FacilityID = col_double(),
    #   FacilityAddressType = col_character(),
    #   FacilityStreetAddress1 = col_character(),
    #   FacilityStreetAddress2 = col_character(),
    #   FacilityStreetAddress3 = col_character(),
    #   City = col_character(),
    #   PostalCode = col_character(),
    #   AddressStateCode = col_character(),
    #   AddressCountryCode = col_character(),
    #   LastUpdatedDate = col_date(format = "")
    # )
 #   ----- Validate locations -----
maps::map('world')
    points(
        Facilities$FacilityLongitude, 
        Facilities$FacilityLatitude,
        pch = 15, col = 'red', cex = 0.5
    )




############  vendor  below old file

library(DBI)    # Connect to the MySQL database: con
con <- dbConnect(RMySQL::MySQL(), 
                 dbname = "mydb", 
                 host = "https://predictivehacks.com/", 
                 port = 3306,
                 #user = "george",
                 user = rstudioapi::askForPassword("Database user"),
                 #password = "predictivehacks"
                 password = rstudioapi::askForPassword("Database password")      )

# Connect to the MySQL database: con
con <- dbConnect(RMySQL::MySQL(), 
                 dbname = "mydb", 
                 host = "https://predictivehacks.com/", 
                 port = 3306,
                 
                   )

tables <- dbListTables(con)   # # Get table names
str(tables)     # Display structure of tables

accounts <- dbReadTable(con, "accounts ") # Import the accounts table from mydb
tables   <- dbListTables(con)  # # Get table names
tables <- lapply(tables , dbReadTable, conn = con)  # Import all tables

my_table <- dbGetQuery(con, "SELECT country, count(*) as tims FROM accounts WHERE region = 'EU'")
my_table

# Send query to the database
myquery <- dbSendQuery(con, "SELECT country, count(*) as tims FROM accounts WHERE region = 'EU'")

dbFetch(myquery , n = 2)  # get the first two rows
dbFetch(myquery, n=-1 )  # get all rows
dbClearResult(myquery ) # Clear myquery 

# Scraping and Tidying Unclean Data
## tidyverse – An ecosystem for wrangling and visualizing data in R
## tabulizer – PDF Scraping
## fuzzyjoin – Joining data with inexact matching
## rvest – Web Scraping
## tidyxl – Importing non-tabular (non-tidy) Excel Data

# Part 1 – Data Sources
## The tabulizer package  for extracting data from PDFs  extract_tables() 
## The fuzzyjoin package is a variation on dplyr’s join operations
## The rvest package makes it easy to scrape daily treasury yield curve rates from the website 
## The tidyxl package imports non-tabular data from Excel files into R
# set version of java to use
Sys.setenv(JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/")

library(knitr)
library(kableExtra)
library(ggpage)
library(magrittr)
library(tidyverse)
library(WDI)
library(pdftools)
library(tabulizer)  #??? error
library(fuzzyjoin)
library(rvest)
library(janitor)
library(tidyxl)

# Data exporation 
# Core Packages
library(tidyverse); 
library(tidyquant); library(recipes); library(rsample)
library(knitr)

library(janitor) # Data Cleaning
library(skimr)  # EDA
library(DataExplorer)
library(scales)  # ggplot2 Helpers
theme_set(theme_tq())


#Summary statistics: skimr::skim()
#Missing data: plot_missing()
#Categorical data: plot_bar()
#Numerical data: plot_historgram

# Automated Data Extraction Functions 

# Download Helper - Creates a Data folder
download.f <- function(url) {
    data.folder = file.path(getwd(), 'data')  # setup temp folder
    if (!dir.exists(data.folder)){dir.create(data.folder, F)}
    filename = file.path(data.folder, basename(url))
    if(!file.exists(filename))
        tryCatch({ download.file(url, filename, mode='wb') }, 
                 error = function(ex) cat('', file=filename))
    message(paste0('File located at: ', filename))
    filename
}

# Tidy PDF Scraping and Fuzzy Joining Helper
extract.values.f <- function(pdf.file, page, names){
                               require(tabulizer)
                               require(fuzzyjoin) # regex_inner_join()
# PDF: https://www.saudiaramco.com/-/media/images/investors/saudi-aramco-prospectus-en.pdf
# Locate table areas
    area = case_when(
        page == 220  ~ c(459.77, 69.76, 601, 427.98), # Table 42 (pg 131)
        page == 221  ~ c(168.03, 69.76, 394.53, 404.59), # Table 43 (pg 132)
        page == 222  ~ c(180.11, 68.38, 413.04, 412.05), # Table 45 (pg 133)
        page == 233  ~ c(181.57, 70.99, 673.96, 448.91) # Table 52 (pg 144)
    )
    
# Extract the tables
    extract_tables(
        pdf.file, 
        pages  = page, 
        area   = list(area), 
        guess  = FALSE, 
        output = "data.frame"
    ) %>% 
        purrr::pluck(1) %>%
        map_dfc(~trimws(gsub("\\.|[[:punct:]]", "", .x))) %>%
        set_names( c("Heading", paste0("X", if(page==233){1:4}else{0:4})) ) %>%
        regex_inner_join(
            data.frame(regex_name = names, stringsAsFactors = FALSE), 
            by = c(Heading = "regex_name")
        ) %>%
        select(X4) %>% 
        pull() %>% 
        as.numeric()
}

url <- 'https://www.saudiaramco.com/-/media/images/investors/saudi-aramco-prospectus-en.pdf'

url <- "https://rdso.indianrailways.gov.in/works/uploads/File/`Final VD QAM 8.1-1 wef 1.1.2020 to 30.06.2020.pdf`"
url <- "Final VD QAM 8.1-1 wef 1.1.2020 to 30.06.2020"
path 
prospectus.pdf <- url(`url`)
## File located at: /Users/gk/Google Drive/data/saudi-aramco-prospectus-en.pdf
prospectus.pdf


# 4.6.1.2 - Long reserves life
inputs <- prospectus.pdf %>%pdf_text()  %>% 
    read_lines() %>% 
    grep("brake", ., value = TRUE) %>%  View()
    str_match_all("[0-9]+") %>% 
    pluck(1) %>% 
    unlist() %>% 
    first() %>% 
    as.numeric() %>% 
    set_names(c("LONG_RESERVES_LIFE")) %>% 
    as.list()

inputs
inputs %>% enframe() %>% unnest(value) %>% kable()


# Table 42 - Gearing and reconciliation - leaverage
inputs <- extract.values.f(
    pdf.file = prospectus.pdf, 
    page     = 220, 
    names    = c("Total borrowings", "Cash and cash equivalents", 
                 "Total equity")
         ) %>% 
    set_names(c("TOTAL_BORROWINGS", "CASH_AND_CASH_EQUIVALENTS", "TOTAL_EQUITY")) %>%
    as.list() %>% 
    append(inputs, .)

inputs %>% enframe() %>% unnest(value) %>% kable()



library(help = "graphics")
plot(airquality$Ozone)
plot(airquality$Ozone, airquality$Wind)

plot(airquality)
# points and lines 
plot(airquality$Ozone, type= "b")  # b= both, l line, p point
plot(airquality$Ozone, type= "h")   # high density vertical lines.
plot(airquality$Ozone, 
     xlab = 'ozone Concentration',   # xlabel
     ylab = 'No of Instances', 
     main = 'Ozone levels in NY city',  # main title
     col = 'green'      # colour
     )


library(sf)
(x = st_crs(4326))

?sf

## RDSO Vendor Analysis  #########################
library(tidyverse)
library(lubridate)
library(cowplot)
# library(ggthemes)      # or install_github('cttobin/ggthemr') #library(themr) # ggthemr("<theme name>") #ggthemr_reset()
# library(plotly)        # plot_grid(gp1, gp2, NULL, gp1, labels = "AUTO", label_size=12, align="v" , scale = c(1, 1, .85, .9))
library(ggpubr)          # with plotgrid, and cowplot
# theme_set( theme_bw()  +  theme( legend.position = "top" )   )
# mydatapasta1   <-    pastefromdatapasta  










#writexl::write_xlsx(setoutdata1419)
library(readxl)

setwd("/Users/gk/Google Drive")   #  
ls()  # list.files()  

ls("/Users/gk/Google Drive/setouts1819final.csv")
list.files("/Users/gk/Google Drive/setouts1819final.csv")
#setoutbak <- readxl::read_xlsx("/Users/gk/Google Drive/setouts1819final.csv", sheet = 1)
setoutbak <- read_csv("/Users/gk/Google Drive/setouts1819final.csv") #  OK !
setoutbak
# write_excel_csv(x, path, na = "NA", append = FALSE, col_names = !append)

# setoutsheetexl <-  readxl::read_excel( "/Users/gk/Google Drive/allsetout.xlsx",  sheet= 1,  skip = 1, col_names = TRUE, range = NULL)      # cf ""
# or                       ::read_xlsx(    filepath                   ,    sheet = NULL, skip = 0, col_names = TRUE, range = NULL, ..
# , col_types = NULL,  na = "", trim_ws = TRUE,  n_max = Inf, guess_max = min(1000, n_max)   )
#  range = NULL, col_types = NULL,  na = "", trim_ws = TRUE,  n_max = Inf, guess_max = min(1000, n_max)  )




########### DATA from gs_read ###############
library(googlesheets)   
gs_auth()                           # gs_auth(new_user = TRUE)
gs_user()                           # current user 
####gs_ls("list")                     # list the worksheets containing "setout"                                   
gs_ls("list")$`sheet_title`            # gives name of files gs_ls("list")[ 1, 1]  error ?
# gs handle with_title(), gs_url(), gs_gs(), gs_key(), get key extract_key_from_url()
# gs_ws_(ls, new, rename, modify, delete, resize, feed )
# explore str, glimpse, typeof(), class(), glimpse(),
# extract_key_from_url(URL), gs_key(URL),  gs_url()  # gs_browse(ss = setouts1718, ws = "setouts1718" )
# lf17handle        <- gs_url("https://docs.google.com/spreadsheets/d/1LXx._0K9U/edit#gid=0")



#########################################################################################
file <- "Mastervendor list .."
#gs_ls("list")$`sheet_title`  
gs_ls("MASTER VENDOR LIST March2020", ignore.case = TRUE)$`FinalMasterList31march` 
gs_ls("MASTER VENDOR LIST March2020")
gs_ls("vendor", ignore.case = TRUE)
# above not working

library(googledrive) # better  use  for files suggested by jenny bryon
drive_user()
drive_auth()

#??drive_find(n_max = 30)
#??drive_find("Master")
#??drive_find("MASTER VENDOR LIST March2020")

#??allvendorhandle   <-   gs_title("RDSOvlist")      # gs by title
# final success
allvendorhandle   <-   gs_title("MASTER VENDOR LIST March2020")      # gs by title

allvendorhandle
# range = "R1C1:R4C3" | "B3:F80" | cell_cols(1:4) | cell_limits(  c(1, 4), c(5, NA) | cell_rows(1:1000) )
# locale, trim_ws, na, comment, n_max
#?setout1819dataraw  <-   gs_read(ss = allsetoutshandle,   ws = 1 , skip = 1, col_names = TRUE,verbose = TRUE) %>% rowid_to_column()
RDSOmarch31           <-   gs_read(ss = allvendorhandle,   ws = 1 , skip = 2, col_names = TRUE,verbose = TRUE) # %>% rowid_to_column()
RDSOmarch31 %>% View()

####

RDSOmech           <-   gs_read(ss = allvendorhandle,   ws = 1 , skip = 1, col_names = TRUE,verbose = TRUE) %>% rowid_to_column()
RDSOsig            <-   gs_read(ss = allsetoutshandle,  ws = 2 , skip = 1, col_names = TRUE,verbose = TRUE) %>% rowid_to_column()
RDSOcivil          <-   gs_read(ss = allsetoutshandle,  ws = 3 , skip = 1, col_names = TRUE,verbose = TRUE) %>% rowid_to_column()
RDSOelect          <-   gs_read(ss = allsetoutshandle,  ws = 4 , skip = 1, col_names = TRUE,verbose = TRUE) %>% rowid_to_column()

## data view and compare columns
RDSOmech  %>% View()    # explore and match columns
RDSOcivil  %>% View() 

a = colnames(setout1819dataraw) ; b = colnames(setout1718dataraw)
setdiff(b,a)  ;  setdiff(a,b)  ; intersect(a,b)  # common in a and b

# rectify the headings
head(RDSOmech) # %>% mutate(), filter(), rbind(lf18a,lf18b), filter(!is.na(Date), dmy(Date)
RDSOmech %>% str()


#### Year wise Data view and standardisation ############################################

RDSOmech %>%   select( rowid, 
                       SN ="Date" , 
                       where = "Terrotory" , 
                       "ICMS" , Loco = "Loco no."  , 
                       "SHED" , # contains("name"),
                 )          ->  RDSOmech

glimpse(RDSOmech)

 library(summarytools)

summarytools::dfSummary(RDSOmech)
view(dfSummary(RDSOmech))   # mix up of mdy and dmy

# freq(RDSOmech$x4, plain.ascii = FALSE, style = "rmarkdown")
# print(ctable(tobacco$smoker, tobacco$diseased, prop = "r"), method = "render")

RDSOmech %>% distinct()   %>%   mutate(ICMS = NA, CRO = NA  ) %>% View() #, pldate = mdy(pldate)  )  %>% 
################################################################################################################

colnames(RDSOmech)
setdiff(  colnames(RDSOmech), colnames(RDSOcivil) )

RDSOallitems <- bind_rows(RDSOmech, RDSOsig,  RDSOcivil,  RDSOelect )

RDSOallitems %>% View()
str(RDSOallitems)

### summarise data and infrence ###########################
# RDSOallitems %>% group_by(group,     var1)    %>% tally()

RDSOallitems %>% group_by(mech,   cat)    %>% tally()  # works
RDSOallitems %>% count( "SHED", "failcat")   %>% View() 
RDSOallitems %>% add_count(SHED, failcat)  %>% select(SHED, n) %>% arrange(desc(n))

add_count()



####################################################
plot(RDSOallitems$Value)

ggplot( RDSOallitems, aes(x=Datets, y=freq)) + 
                   geom_jitter(col = "blue") +    
                   scale_x_datetime(breaks=date_breaks("1 year")) + ylim(0, 25)



# Alluvial diagaram
library(ggfortify)
autoplot(RDSOallitems) +    # time series element
    labs(title="RDSOallitems") +               
    theme(plot.title = element_text(hjust=0.5))


#####################################
# exploration
library(summarytools)
descr(RDSOallitems, style = "rmarkdown")   # stats = "common", transpose = TRUE, headings = FALSE) for summary
view(dfSummary(RDSOallitems))              #  view(dfSummary(nrshedsetouts))

library("DataExplorer")
create_report(RDSOallitems)  # All consolidated reports
introduce(RDSOallitems)
plot_intro(RDSOallitems)
plot_missing(RDSOallitems)
plot_bar(RDSOallitems)
plot_correlation(na.omit(RDSOallitems), maxcat = 5L)

data_list <-  list( RDSOallitems,  RDSOallitems )   # list of databases to visualise structure
plot_str( data_list )

# install.packages("ExPanDaR")
# library(ExPanDaR) interactive no use

library(esquisse)   #helps in launching the add-in graphs drag and drop!
esquisse::esquisser() 

RDSOallitems %>% add_count(SHED)  %>% View()  # adds n column
RDSOallitems
RDSOallitems %>% add_tally()  %>% View()  # adds total count colun else use %>% tally()
#?RDSOallitems %>% group_by("mech") > % filter( !is.na("RDSOallitems$SN") ) %>% tally(sort = TRUE)


#############
RDSOallitems
names(RDSOallitems)
library(googleVis)
library(plyr)
gg<- gvisMotionChart(RDSOallitems,    # Your data has 9736 rows, but idvar and timevar only define 4338 unique rows.
                     idvar   = "type", #"country",
                     timevar = "Date", #"year",
                     xvar    = "Date", #"GDP",
                     yvar    =  "where" ,   "Life Expectancy",
                     sizevar =   2, #    "Population",
                     colorvar =  "Dept" #  "region"
)

plot(gg)

cat(gg$html$chart, file="chart1.html")


###### examine

RDSOallitems %>% filter(!is.na(Date) ) %>% group_by(Date)  %>% # year(Date), month(Date)  )  %>%
    summarise(   n = n()    )    ->    RDSOvanalyise  # , !is.na(ICMS) 
RDSOvanalyise
## learnpoint  df %>% summarize(function(colname[conditional_colname])) 

#colnames(nrsetouts)  <- c("sumdates", "totalcases" )

RDSOvanalyise  %>% View()
str(RDSOvanalyise)

RDSOvanalyise 



RDSOvanalyise  %>%    ggplot( aes(x = Date, y = n) )   +  theme_bw()  +
    geom_jitter(  aes(x = Date, y = n, color =  as.factor( year(Date)  )    ) ,  size =3.5 , alpha = 0.25 )  +
    #  scale_size(range = c(0.5, 12)) + # Adjust the range of points size 
    geom_smooth( method = "loess" ) + #method=lm,     se=TRUE )  +   # color, size ,linetype , fill 
    ## auto", se=TRUE, fullrange=FALSE, level=0.95 (confidence),  method = “loess”(small obs, lm = liniar model),  method = "lm", formula = y ~ poly(x, 3), se = FALSE)
    # "auto", "lm", "glm", "gam", "loess" 
    
    geom_hline(yintercept = 12, size = 0.8, colour="black", alpha = 0.8 )  + 
    geom_vline( xintercept =  as.Date("2016-08-02") , size = 0.5, colour="black", alpha = 0.5 , text=element_text(size=6) )  +  # as Traction department
    geom_text(aes(x=as.Date("2016-08-02"), label= "2 Aug 2016", #" paste0( " Changeover date ", "2016-08-02") , 
                  y=15 ),  angle=90, vjust = -1 , alpha = 0.2, text=element_text(size=10) ) + # , colour="blue" , text=element_text(size=11)
    #geom_text(aes(x=as.Date("2016-08-02"), label="Traction Dept",   y=15), colour="green", angle=90, vjust = 1.2, text=element_text(size=11) ) +
    geom_vline( xintercept =  as.Date("15/12/2017")   )  +  # Elect loco not coming
    #? scale_fill_discrete(name = "Legends", labels = c("A", "B", "C")) +
    # ?labs(fill = "Dose (mg)") +  # ???
    geom_abline()  +
    #annotate("text", x = ymd("2017-11-30"), y = 20, label = "italic(LocoFail) ^ 2 == 0.75", parse = TRUE) +
    annotate("text",  x = ymd("2018-03-31"), y = 11, label = "Elect Locos introduced in Diesel Shed", parse = FALSE) +
    
    #  theme(legend.title = element_blank() )  +
    # scale_x_discrete(limits = c("2", "0.5", "1" ))  +
    scale_fill_discrete(name = "Years", labels = c("A"," B", "C")) +
    # scale_fill_viridis_d() +
    #  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
    # ? theme(legend.position = c(0.7, 0.2),  legend.direction = "horizontal") +
    # ?facet_grid(~ as.factor(year(nrsetouts$sumdates) )  ) +
    # theme(legend.position = "top" )  +
    rotate_x_text(45) +
    theme(legend.position = c(0.7, 0.8),   legend.direction = "horizontal") +
    labs(title = "Temperatures\n", x = "Date [°Y-M-D]", y = "Total Loco Troubles", color = "black") +
    labs(   title    = " All Diesel loco trouble reported over last five years",          
            subtitle = " Total Daily Diesel loco traubles reported ", 
            caption  = " (Details of each single case is avialble in shareable googlesheet link)"  ,  
            color =  " Year ",  
            size =  "size" , 
            alpha = " Size"  )   #  +
# guides( color=guide_legend("my title"), fill = guide_legend(reverse = TRUE) )  # add guide properties by aesthetic  



#labs(x="miles per gallon", y="displacement", size="horsepower",  col="# of cylinders", shape="# of gears")

library(GGally)
ggpairs(RDSOvanalyise[,2])+ theme_bw()

library(factoextra)
RDSOvanalyise[ , 5:5 ] %>%
    scale() %>%                           # Scale the data
    dist() %>%                            # Compute distance matrix
    hclust(method = "ward.D2") %>%        # Hierarchical clustering
    fviz_dend(cex = 0.5, k = 4, palette = "jco") # Visualize and cut 

#######
ggplot(iris, aes(Sepal.Length)) +
    geom_histogram(aes(fill = Species, color = Species), bins = 20,  position = "identity", alpha = 0.5) +
    scale_fill_viridis_d() +
    scale_color_viridis_d()

############# second graph


RDSOvanalyise  %>%  ggplot() +
    geom_point(  aes(x = Date, y = n,  col = ifelse(Date > dmy("1/08/2016"), "red", "blue")  )  )  + 
    geom_smooth( aes(x = Date, y = n), method = "loess" ) +
    # scale_x_continuous(breaks = pretty(x, n = 10)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    scale_color_manual(values=c('#999999','#E69F00','#56B4E9'),  guide=FALSE)

    geom_smooth( method = "loess" ) + #method=lm,     se=TRUE )  +   # color, size ,linetype , fill 
    ## auto", se=TRUE, fullrange=FALSE, level=0.95 (confidence),  method = “loess”(small obs, lm = liniar model),  method = "lm", formula = y ~ poly(x, 3), se = FALSE)
    # "auto", "lm", "glm", "gam", "loess" 
    geom_hline(yintercept = 12, size = 0.8, colour="black", alpha = 0.8 )  + 
    geom_vline( xintercept =  as.Date("2016-08-02") , size = 0.5, colour="black", alpha = 0.5 , text=element_text(size=6) )  +  # as Traction department
    geom_text(aes(x=as.Date("2016-08-02"), label= "2 Aug 2016", #" paste0( " Changeover date ", "2016-08-02") , 
                  y=15 ),  angle=90, vjust = -1 , alpha = 0.2, text=element_text(size=10) ) + # , colour="blue" , text=element_text(size=11)
    #geom_text(aes(x=as.Date("2016-08-02"), label="Traction Dept",   y=15), colour="green", angle=90, vjust = 1.2, text=element_text(size=11) ) +
    geom_vline( xintercept =  as.Date("15/12/2017")   )  +  # Elect loco not coming
    #? scale_fill_discrete(name = "Legends", labels = c("A", "B", "C")) +
    # ?labs(fill = "Dose (mg)") +  # ???
    geom_abline()  +
    theme(legend.title = element_blank()) +
    # ? theme(legend.position = c(0.7, 0.2),  legend.direction = "horizontal") +
    # ?facet_grid(~ as.factor(year(nrsetouts$sumdates) )  ) +
    theme(legend.position = "top" )  +
    labs(title = "Temperatures\n", x = "Date [°Y-M-D]", y = "Total Loco Troubles", color = "black") +
    labs(   title    = " All Diesel loco trouble reported over last three years",          
            subtitle = " Total Daily Diesel loco traubles reported ", 
            caption  = " (Details of each single case is avialble in shareable googlesheet link)"        )


# p + annotate("text", x = 4, y = 25, label = "Some text")
# p + annotate("text", x = 2:5, y = 25, label = "Some text")  # multi same lables
# p + annotate("text", x = 2:3, y = 20:21, label = c("my label", "label 2")) # multi lables

# p + annotate("rect", xmin = 3, xmax = 4.2, ymin = 12, ymax = 21,  alpha = .2) # rect area zone
# p + annotate("segment", x = 2.5, xend = 4, y = 15, yend = 25,  colour = "blue")   # Line segment
# p + annotate("pointrange", x = 3.5, y = 20, ymin = 12, ymax = 28,  colour = "red", size = 1.5) # error bar
# p + annotate("text", x = 4, y = 25, label = "italic(R) ^ 2 == 0.75",  parse = TRUE) # parse to commands
# p + annotate("text", x = 4, y = 25, label = "paste(italic(R) ^ 2, \" = .75\")", parse = TRUE)
# 

# p + labs(). Change legend title.   Use p + labs(fill = “dose”) for geom_boxplot(aes(fill = dose))    
# p + scale_x_discrete(limits = c(“2”, “0.5”, “1”)). Change the order of the item in the legend.
# p + scale_fill_discrete(name = “Dose”, labels = c(“A”, “B”, “C”)). Rename the legend title and text labels.    
#########################
# Data table
library(tidyr)
ggplot() + theme_nothing() + annotate("text", x = 4, y = 12, cex=12.5, label = " ") +
    #annotate("text", x = 4, y = 13, cex=12.5, label = "Second ") +
    # geom_label(aes(fill = factor(cyl)), colour = "white", fontface = "bold")  +
    #geom_label(" this label") + 
    text(4,8,"big",cex=1.5)
# 
geom_label_repel(  aes( label = rownames(df), fill = factor(cyl) ), color = 'white',  size = 3.5 )
geom_text_repel(   aes(label = rownames(df)),                                         size = 3.5 )

text(4,4,"big",cex=1.5)


RDSOvanalyise1 <- left_join(d1, d2, by = c("rowid" = "rowid")  )
setouts

summarytools::dfSummary(RDSOmech) %>% view()
freq(RDSOvanalyise$Date, plain.ascii = FALSE, style = "rmarkdown")
view(dfSummary(RDSOvanalyise))

# pivot_longer(- rowid, -Date, -where, -Loco, -Shed,  names_to = "attribute", values_to = "atvalue")
gather(- rowid, -Date, -where, -Mech, -Cat,  key = "attribute", value = "atvalue" ) %>% 
    View()


RDSOvanalyise %>% group_by( year(Date) ) %>%  summarise(n =)


pivot_longer(-religion, names_to = "income", values_to = "count")
pivot_longer(`2000`:`2017`, names_to = "year", values_to = "value")
pivot_wider(names_from = variable, values_from = value)

RDSOvanalyise %>% pivot_wider(names_from = SHED, values_from = count() )

# mutate usefulfn  +, - etc, log(), lead(), lag(), 
# , row_number(),  percent_rank(), ntile(), dense_rank(), min_rank(),
# cumsum(), cummean(), cummin(), cummax(), cumany(), cumall() , cume_dist()
# na_if(), if_else(),  case_when(), recode(), coalesce(), 
Sys.getlocale("LC_TIME")

slice(setoutdata1819clean, 51:60)
setoutdata1819clean[51:60,   ]
nth(setoutdata1819clean, n = 51)
select(arrange(bison, desc(year)), year, ITISscientificName)
bison <- mutate_each(bison, funs(tolower), contains("name"))

#??str_detect( lf18final$"Brief actual cause", lock)     
lf18final %>% recode(  ICMS = "yes", YES = "yes" ) %>% View()
#recode(num_vec, "a", "b", "c", .default = "other")
mtcars %>%    mutate( mpg = replace(mpg,  cyl==4, NA)   ) 


#mutate(x = replace(x, x<0, NA))
#mutate(mpg=replace(mpg, cyl==4, NA)) %>%

# df[df$height == 20, "height"] <- NA
# df %>% mutate(height = replace(height, height == 20, NA))
# df %>% mutate(height = ifelse(height == 20, NA, height))
# mutate(X25 = ifelse(X25 == "Other", Other, X25))
# mutate(mpg=replace(mpg, cyl==4, NA)) %>%
#  mutate(x = replace_na(x, 0))



#################################################################
#################################################################
## Base summery
base::summary(data)
by(data, data$SHED, summary)

# with  group, apply multipel functoin to one variable
mtcars %>%  group_by(cyl) %>%  summarise_each( funs(min_mpg = min,  max_mpg = max),   mpg   )
# more compact with one funtion summerise_each to many variables,  provide  
mtcars %>%  group_by(cyl) %>%  summarise_each( funs(mean), mpg, disp)

# apply many functions to many variables  - or less if want, but give names to vars
mtcars %>%   group_by(cyl) %>% 
    summarise_each(funs(min, max) , mpg, disp) %>%
    setNames(c("gear", "min_mpg", "min_disp", "max_mpg", "max_disp"))


dim( na.omit(RDSOvanalyise) )

summary(aggr(lf18final, sortVar=TRUE))$SHED

matrixplot(RDSOvanalyise, sortby = 4)

marginplot(RDSOvanalyise[,c("SN","ICMS")])


library(GGally)
ggpairs(data)
ggpairs(data, mapping = aes(colour = category))

library(descriptr)
ds_summary_stats(data$score)
ds_screener(data)


library(naniar)
gg_miss_var(lf18final)
getAnywhere(gg_miss_var())

##### usingg Hmisc
Hmisc::describe(data)


library(psych)
psych::describe(data)
psych::describeBy(data, data$type)
psych::describeBy(data, data$type, mat = TRUE)  # matrix of output

library(skimr)
skim(data)
skim_with(integer = list(hist = NULL))

data %>% group_by( SHED)  %>%     skim()

##########################################
library(summarytools)
summarytools::descr(data)
summarytools::descr(data, transpose = TRUE)

dfSummary(data)           # Better 
view(dfSummary(data))     # (view) View in HTML note small v in view

dfSummary( data, 
           plain.ascii = FALSE, 
           style = "grid", 
           graph.magnif = 0.75, 
           valid.col = FALSE, 
           tmp.img.dir = "/tmp" )   # for markdown printing



library(GGally)
data[, 1:10]  -> data1 #  %>% View()
ggpairs(data1)
ggpairs(data1, mapping = aes(colour = SHED), cardinality_threshold= 2000)   # ??? unable 


library(descriptr)
ds_summary_stats(data$score)


library(VIM)
matrixplot(data, sortby = 2)
marginplot(data[ ,  c("SHED","Loco")]  ) 
aggr(data,only.miss=TRUE,numbers=TRUE,sortVar=TRUE)  # ok
summary(aggr(data,prop=TRUE,combined=TRUE))$combinations 



library(prophet)
data %>% filter(!is.na(Date)) %>% group_by(`Date`) %>% mutate(ds = dmy(Date ) )  %>% summarise( y = n() ) -> t
m <- prophet(t)
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m, forecast)           # some how plotted
prophet_plot_components(m, forecast)


##############################################################################
##############################################################################
data <- setoutdata1819clean

gs_new(  title = "setouts1819final",  ws_title = "setouts1819final",   input = setoutdata1819clean,  verbose = TRUE , trim = TRUE) #col_extent = NULL,


# or faster with gs_upload
write_csv( setoutdata1819clean , "/Users/gk/Google Drive/setouts1819final.csv",    na = "NA", append = FALSE)
gs_upload("/Users/gk/Google Drive/setouts1819final.csv", sheet_title = NULL, verbose = TRUE, overwrite = TRUE) # faster

gs_ls("setou")  
gs_ls("setou")$sheet_title    # # yes above done


# lf18finalhandle <- gs_title("setouts1819fina.csv", ws = 1, skip = 0, cpl  ) # it was csv
# read.csv(file, header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", ...)

#lf18finalread <- read.csv("/Users/gk/Google Drive/setouts1819final.csv", header = TRUE,  quote = "\"", skip =0, dec = "."  ) 
#, fill = TRUE, col.names = c(), stringsAsFactors = TRUE, strip.white = TRUE, sep = ",", nrows	= 4636, check.names = TRUE,
# row.names = col vector or idcolumn, na.strings ="NA"

setout1819finalread <- read_csv("/Users/gk/Google Drive/setouts1819final.csv", col_names = TRUE, locale = default_locale(),
                                skip = 0, n_max = Inf, na = c("", "NA"), trim_ws = TRUE )
spec(setout1819finalread) 
glimpse(setout1819finalread)
str(setout1819finalread)
setout1819finalread  %>% View()

setouts1819 <- setout1819finalread


# or using gs_read
mtcars_sheet <- gs_new(title = "mtcars", ws_title = "first_sheet", input = mtcars)
#insert the rownames vertically in column B as anchor B2
gs_edit_cells(mtcars_sheet, ws = "first_sheet", anchor = "B2", input = rownames(mtcars), byrow = FALSE)
######################

################# Now we have data in lf18finalread  ####### 
#################  lf18finalread  ####### ####### ####### 

setouts1819  %>% View()
#??setouts1819 %>%  summarise (min_mpg = min(mpg), max_mpg = max(mpg), mean_mpg = mean(mpg), mean_disp = mean(disp))
colnames(setouts1819)
setouts1819p1   <-   setouts1819  %>% select( ., 
                                              `Date` , `Loco`, SHED, System, pltype, ICMS,  # `PL RESP`,
                                              `Train`,
                                              "Brief FIR", 
                                              "Failreason" , 
                                              "Brief main sub assembly",
                                              `System`,
                                              #`Brief RESP cause`,
                                              # everything() ,
                                              `Responsibilty`   )    %>%   
    # mutate( resp1 =  paste0(SHED, `Loco no.` )  )    %>%
    mutate( MU = ifelse( str_length(`Loco`)  > 5 ,  "MU",  "SU" )  )  %>%
    mutate( HHP = ifelse( str_sub(`Loco`, 1, 2)  %in% c("12", "40", "70") ,  " HHP" , "ALCO" ) ) %>% 
    filter(SHED %in% c("AMV", "LKO", "TKD", "LDH"  ) )  %>%
    arrange(System, `Loco`, Date)   %>%
    # filter(!is.na(Date) )   %>% 
    select(`Date` , `Loco`, `MU`, `HHP`, everything() )       


View(setouts1819p1) 
view(dfSummary(setouts1819p1))     # (view) View in HTML note small v in view


count(setouts1819$Loco) 

setouts1819p1 %>% group_by(ymd("Date"))  %>% summarise( countsn =  count( ymd("Date") )   )

setouts1819 %>% group_by("ICMS") %>% summarise_each (funs(mean) , mean_mpg = mpg) #summarise( mean_mpg = n()   )
str()

setouts1819 %>%  filter( pltype == "DIR",  SHED %in% c( "TKD", "LDH", "LKO", "SSB")   ) %>%
    group_by(pltype, SHED) %>%  summarise( pl = n()) 

punctuality1819

# Days of failures

setouts1819 %>%
    tidyr::extract(`SCH.`, into = c("DOCe"),  "DOC-(\\d{1,2}.\\d{1,2}.\\d{2})"  , remove = FALSE  ) %>%
    tidyr::extract(`SCH.`, into = c("POHe"),  "POH-(\\d{1,2}.\\d{1,2}.\\d{2})"  , remove = FALSE  ) %>% 
    tidyr::extract(`SCH.`, into = c("M24e"),  "(M24-\\d{2}.\\d{2}.\\d{2})"      , remove = FALSE  ) %>% 
    tidyr::extract(`SCH.`, into = c("Monthe"),"(M\\d{1,2}-\\d{2}.\\d{2}.\\d{2})", remove = FALSE  ) %>% 
    tidyr::extract(`SCH.`, into = c("Te"),  "(T\\d{0,3}-\\d{2}.\\d{2}.\\d{2})"  , remove = FALSE  ) %>% 
    mutate( DOC = dmy(DOCe)   )      %>% 
    mutate(POH = dmy(POH)  )   %>%
    mutate( M24 = dmy(`M24/M72/M48/6Y`)  ) %>% 
    mutate( T90 = dmy(`T/T90`)  ) %>% 
    mutate( T   = dmy(`T/T90`)  ) %>% 
    mutate( Td   = dmy( str_sub(Te,-8,-1))    ) %>% 
    mutate(DOCdays = Date - DOC)   %>%
    mutate(Tdays = Date - Td )   %>%
    select(`Loco no.`, `SCH.`,DOC, DOCe, DOCdays,   T, Tdays,
           POH,
           M24,M24e,
           T90,
           
           Monthe, Te, 
           everything())   ->  Td #    %>%     View()
glimpse(Td)

Td %>% filter(Tdays < 120 , Tdays > 0 ) %>%  arrange(Tdays) -> Td
plot(Td$Tdays)


View(setouts1819)


##########
library(survival)   # essential for fitting
library(survminer)  # for cutomisable plot with table
lung
fit <- survfit(Surv(time, status) ~ 1, data = lung)
# Drawing curves
ggsurvplot(fit, color = "#2E9FDF")

Td$status <- 2
#Td$status[21:nrow(Td) ] <- 1
Td

fit <- survfit(Surv(Tdays, status) ~ 1, data = Td)

ggsurvplot(fit, color = "#2E9FDF")


fit<- survfit(  Surv(Tdays, status)  ~  HHP,     data = Td   )  # multi Surv(time, status) ~ rx + adhere,
ggsurvplot(fit,
           #fun = "event",
           #fun = "cumhaz",
           fun = function(y) y*100,   # arb fn
           #palette = c("#E7B800", "#2E9FDF"), # custom color palette
           #palette = "Dark2",
           conf.int = TRUE, # Add confidence interval
           pval = TRUE,  # Add p-value,   3sigma  p< 0.0001
           #size = 3,  # change line size
           xlab = "Time in days",   # customize X axis label.
           break.time.by = 20, # break time axis by 250
           linetype = "strata", # change line type by groups
           # censor.shape="|", censor.size = 4, # shape of censor
           # legend.labs = c("A", "B", "C", "D", "E", "F")
           risk.table = TRUE, 
           risk.table.y.text.col = TRUE,
           risk.table.col = "strata", # Change risk table color by groups
           xlim = c(0, 100),
           risk.table.height = 0.25, # risk table height of total height
           risk.table.y.text = FALSE,# show bars instead of names in text annotations
           ncensor.plot = TRUE,      # plot the number of censored subjects at time t
           ncensor.plot.height = 0.25,
           conf.int.style = "step",  # customize style of confidence intervals
           # ? surv.median.line = "hv",  # add the median survival pointer.
           surv.median.line = "hv",  # add the median survival pointer from c("none", "hv", "h", "v")..
           ggtheme = theme_bw() # theme_light(), Change ggplot2 theme
)   -> lifeplot2

lifeplot2



# ggsurvplot() 
#ggsurvplot_list(),   ggsurvplot_facet(), ggsurvplot_group_by(), ggsurvplot_add_all(), ggsurvplot_combine()



# number of char in string  cchar(), nchar(NA, keepNA=TRUE) ,  str_length("foo")
########## model from https://rpkgs.datanovia.com/survminer/index.html ##
fit <- survfit(Surv(time, status) ~ sex, data = lung)
ggsurv <- ggsurvplot(
    fit,                     # survfit object with calculated statistics.
    data = lung,             # data used to fit survival curves.
    risk.table = TRUE,       # show risk table.
    pval = TRUE,             # show p-value of log-rank test.
    conf.int = TRUE,         # show confidence intervals for point estimates of survival curves.
    palette = c("#E7B800", "#2E9FDF"),
    xlim = c(0,500),         # present narrower X axis, but not affectsurvival estimates.
    xlab = "Time in days",   # customize X axis label.
    break.time.by = 100,     # break X axis in time intervals by 500.
    ggtheme = theme_light(), # customize plot and risk table with a theme.
    risk.table.y.text.col = T,# colour risk table text annotations.
    risk.table.height = 0.25, # the height of the risk table
    risk.table.y.text = FALSE,# show bars instead of names in text annotations in legend of risk table.
    ncensor.plot = TRUE,      # plot the number of censored subjects at time t
    ncensor.plot.height = 0.25,
    conf.int.style = "step",  # customize style of confidence intervals
    surv.median.line = "hv",  # add the median survival pointer from c("none", "hv", "h", "v")..
    # group.by = " " ,
    # facet.by = " " ,
    # ?add.all,
    legend.labs = c("Male", "Female")    # change legend labels.
)
ggsurv


# Fit a Cox proportional hazards model
#fit.coxph <- coxph(surv_object ~ rx + resid.ds + age_group + ecog.ps, data = ovarian)
#ggforest(fit.coxph, data = ovarian)


library("ggfortify")

# Heatmap
df <- mtcars[, c("mpg", "disp", "hp", "drat", "wt")]
df <- as.matrix(df)
Td
Td[ , c("Loco no.", "Tdays" )]  # ??
df <-  Td[, c(as.factor("Loco no."), as.numeric("Tdays") )]   # ??

autoplot(scale(df)) # 

# autoplot() can handle also other time-series-likes packages, including:
# zoo::zooreg()  ; xts::xts() ; timeSeries::timSeries() ; tseries::irts() ; forecast::forecast() ; vars:vars()


library(changepoint)
autoplot(cpt.meanvar(AirPassengers))
library(strucchange)
autoplot(breakpoints(Nile ~ 1))

# Principal component analysis
df <- iris[, -5]   # Prepare the data
pca <- prcomp(df, scale. = TRUE) 
autoplot(pca, loadings = TRUE, loadings.label = TRUE,   data = iris, colour = 'Species')

# K means
autoplot(kmeans(USArrests, 3), data = USArrests, label = TRUE, label.size = 3, frame = TRUE)

library(cluster)
autoplot(pam(iris[-5], 3), frame = TRUE, frame.type = 'norm')

library(survival)
fit <- survfit(Surv(time, status) ~ sex, data = lung)
autoplot(fit)


autoplot(AirPassengers, ts.colour = 'blue') + 
    ggtitle('AirPassengers') + xlab('Year') + ylab('Passengers') 

set.seed(1)
autoplot(kmeans(iris[-5], 3), data = iris)   + scale_colour_brewer()

#########################################

setouts1819 %>%
    filter(., SHED %in% c("TKD", "LDH", "LKO", "AMV", "SSB")) %>%    
    filter(., !is.na(Date) )        %>%                            # remove indirect cases without date
    filter(ICMS == "YES")           %>%
    ggplot() + 
    geom_bar( aes(x= factor( month(Date, label = TRUE), 
                             levels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar") ),
                  fill = SHED  ), 
              alpha = 0.5,  width=.80, position =  "dodge" )   +  facet_wrap(~SHED)  +
    # scale_fill_discrete(drop=FALSE) + 
    scale_x_discrete( drop=FALSE ) +       # to include factors with zero count
    theme( legend.position = "none" ) +  theme_bw(base_size = 11) +
    labs( x=   "Loco setouts per month" )                         +       
    labs(title    =  "Loco setouts per month in 2018-19")         +
    labs(subtitle =  "Loco setouts of NR sheds")                  +
    labs(caption  =  "(based on allsetout data sheet 21 march 2019...)" )    ->    gp1 
gp1

setouts1819  %>%  
    filter(., SHED %in% c("TKD", "LDH", "LKO", "AMV")) %>%      #View()
    # mutate(  Date = dmy(Date) ) %>%  
    # distinct()   %>% 
    #filter(!is.na(Date))  %>%          # remove indirect cases
    #filter(ICMS == "YES")  %>%
    ggplot() + 
    geom_bar( aes(x= parse_factor( month( Date, label = TRUE) ,     # lable to displays the abr name
                                   levels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")    ),
                  fill = SHED ), 
              alpha = 0.5,
              width= 0.85, position = "dodge" ) + 
    scale_x_discrete(drop=FALSE) +       # to include factors with zero count
    facet_wrap(~SHED) +  
    coord_flip( ylim = c(3,50), xlim = c(1, 13) ) +            # better way for limiting graphs
    # coord_cartesian( ylim = c(3,50), xlim = c(1, 13) ) +      # months +na # removes scale_x_continuous(limits = c(-5000, 5000))
    # scale_color_viridis(trans="log")  +
    theme(legend.position = "none") + theme_bw(base_size = 10) +
    labs(x=" Loco setouts per month - NR Sheds")   + 
    labs(y=" setouts counts per month")   + 
    labs(title = "Loco setouts per month in 2017-18") +
    labs(subtitle = "Loco setouts per month of NR only") +
    labs(caption = "(based on set out data from 21/3/2019 sheet...)")        -> gp2 

gp2
##### plot grids  of plots #######

plot_grid(gp1, gp2, NULL, gp1, labels = "AUTO")
# ? plot_grid(gp1, gp2, NULL, ggsurv, labels = "AUTO")


## cause wise analysis   #########################################
# ord_ages_class <- c(factor ordered list "", "", "")
# new_ages %>%   mutate(age_class = factor(age_class, levels = ord_ages_class)) #is.factor(), is.ordered()
# new_ages %>%   fct_relevel( ord_ages_class) %>% data.frame(age_class = .) -> plot_ages_forcats

#data$carb <- factor(data$carb, levels = data$carb[order(-data$mpg)])
#lfcause16$System  <- as.factor(lfcause16$System)
#lfcause16$System <- factor(lfcause16$System, levels = lfcause16$Date[ order(as.factor( lfcause16$System) ) ] )

#lf18 %>% mutate(System = factor(System,    levels = orderlevellist ))

setouts1819

setouts1819  %>%    select(., Date, `Loco no.`,  SHED, `System`,  
                           `Main sub assembly`, everything() )  %>%
    distinct() %>%                           # remove duplicate and blank
    filter(., !is.na(Date) ) %>%             # remove repercussions, 
    filter(., SHED %in% c( "LKO" , "TKD", "LDH" )) %>%    # , "TKD", "LDH"
    mutate( System =  factor(System ) )  %>%
    # filter(., `ALCO/ HHP` == "HHP" ) %>%
    # mutate( System = fct_lump(  System,  n = 30  )  ) %>%     #  lump to prop or n, or -n reverse
    # mutate( System = fct_recode( System, "Water Cooling System" = "WCS")  ) %>% # not reqd
    mutate( System = fct_collapse( System,
                                   `Power Assemblies` = c("PP", "WCS+PP", "PP+TSC", "PP/WCS", "PP/FOS"),
                                   `Air Brake System` = c("ABS", "CCB", "CCB1.5","CCB2.0"),
                                   `Compressor` = c("Comp", "Comp.", "COMP"),
                                   `Water Cooling System` = c("WCS", "wcs", "wcs"),
                                   `Fuel oil system` = c("FOS"),
                                   `Lube oil system` = c("LOS", "los"),
                                   `Turbo Super Charger` = c("TSC", "Turbo"),
                                   `Under Gear`      = c("U/G", "UG" , "u/g", "Under Gear"),
                                   `Other Mechanical`      = c("O/M"),
                                   `Radiator and assem.` = c(   "Rad & its assembly",
                                                                "Rad & assembly"       ),
                                   `Engine Governor` = c("Gov.", "Gov" , "GOV" ),
                                   
                                   `Traction Motors` = c("TM", "TM3" ),
                                   `Traction Alternator TG` = c("TA/CA/AG" , "TA/TG", "TA/CA", "TA", "Traction Alternator/ TG"),
                                   `Small Motors` = c("Small motor" , "Small Motor"),
                                   `CROW BAR`      = c("Crow bar", "CRO BAR "),
                                   `Convetors and invertors` = c("Cab", "Convetors and invertors", "Rectifier"),
                                   `Reversor & BKT`  = c("Rev. and BKT", 
                                                         "Rev and BKT", "Rev. & BKT", "BKT& REV",
                                                         "REV/BKT",     "Rev.& BKT", 
                                                         "Reversor & BKT", "Reversor", "Rev & BKT" ,
                                                         "REV &BKT","Reversor & BKT"),
                                   `Relay & Contactors` = c("Relay & Contactors", "Relay & contactor",
                                                            "Relay & CONTACTOR", "Contactors",
                                                            "Relay & Contactor"),
                                   `Power & Control Cables` = c("Power & CNL Cable","Power& CNL cable",
                                                                "Power & CNL cable", "Power &CNL cable","Power & cnl CABLE",
                                                                "Power & CNLcable" ,"Power& CNL cable" ),
                                   
                                   
                                   `Head Light or Flasher` = c("HL & FL", "HL& FL"),
                                   `Dynamic Brake System`  = c("Dy.Brake", "Brake chopper resistor"),
                                   Accidental = c("Accident"),
                                   `Switch & CCT Breaker` = c("Switch & CCT breaker", 
                                                              "Switch & cct breaker", 
                                                              "Switch & CCt breaker",
                                                              "Switch & Cct. breaker" ,
                                                              "Switch & Ccct breaker",
                                                              "Switch & Cct breaker"),
                                   `Batteries`  = c("Battries", "Battery"),
                                   `Control Stand` = c("CNL stand" , "TCC" ,  "TFT" ),
                                   `Other Electrical` = c("O/E", "o/e", "Resistance"),
                                   `Other Gen and Misc`  = c("Other gen" , "Other Gen")
    )   )  ->   lf18cause


levels(lf18cause$System)

lf18cause

# levels(droplevels(subdf$letters), reorder = FALSE)  # FALSE to preserve origibal order
# fct_infreq() %>%     fct_rev()  %>%     
# fct_reorder(lf18cause$System`,  lf18cause$Date  ,  .desc = TRUE) %>% 
# lf18t$System %>%  fct_relevel("Power & Cable", "Power & Control Cables") %>% levels()    


gp3  <- lf18cause  %>%   filter(HHP == "ALCO")  %>%  filter(!is.na(System))  %>%
    ggplot()    + 
    geom_bar( aes(  x =  fct_rev( fct_infreq( factor(System) ) ), fill = SHED ),  alpha = 0.8, position = "dodge") + 
    facet_wrap( ~SHED ) +   
    # facet_grid(HHP ~ SHED ) +  
    # scale_y_continuous(limits = c(0,50)) +   # ok but removes the plot altogether
    coord_flip( ylim = c(3,30) ) +  #, xlim = c(1, 13) ) +  
    # geom_text(  aes(x = System, y = System, label = System) ,  hjust = 0.50,  color = "#FF22FF" ) +
    theme_bw()  +
    labs(title = "The top 25 leading cause of loco troubles")   +
    labs(subtitle = " Analysis of set outs of all NR locos data in 2017-18")   +
    labs(x = "The cause wise loco failures/ troubles")  

gp3  + draw_image(
    "https://upload.wikimedia.org/wikipedia/en/7/77/EricCartman.png",
    x = 5, y = 2.5, width = 12, height = 11.5
)

plot_grid(gp1, gp2, NULL, gp3, labels = "AUTO")


TKDlife
dplyr::lead(TKDlife)
####################################


# ggplotly(gp3)  # issues with Cairo
lf18cause

lf18cause      %>%       filter( !is.na(System) )  %>% 
    filter( SHED  %in% c("LDH", "TKD", "LKO")  ) %>% 
    group_by( System)  %>%
    summarise( tcases= n() )  %>%    #   View()   
    filter(tcases >5) %>%     
    ggplot( ) + coord_flip() +
    #?? scale_color_viridis(trans="log") +
    geom_bar( aes(x= reorder( System, tcases), 
                  y= tcases   ),
              position = "dodge", stat= "identity" )                

# ?? spread(lf18cause, System, tcases) %>%  t()   # transpose


# ?? lf18cause  %>%  filter(n> 0)  %>% #arrange(System) %>%
ggplot()    +   
    geom_bar(    aes(x = System, y= tcases), fill = `System` , stat = "identity"  )   +
    coord_flip() + 
    theme(legend.position = "none")

################################
p <-ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars)))
p + geom_text()  +  annotate("text", label = "plot mpg vs. wt", x = 4, y = 35, size = 8, colour = "red")
p + geom_text(check_overlap = TRUE)
p + geom_text(aes( colour = factor(cyl))) + scale_colour_discrete(l = 40)
p + geom_text(aes(label = paste(wt, "^(", cyl, ")", sep = "")), parse = TRUE)

p + geom_label(size = 3, family = "Times New Roman", aes(colour = factor(cyl)) )
p + geom_label(aes(fill = factor(cyl), size = wt), colour = "white", fontface = "bold" )

p + geom_point() + geom_text(hjust = 0, nudge_x = 0.05)

# Aligning labels and bars 
df <- data.frame(   x = factor(c(1, 1, 2, 2)),    
                    y = c(1, 3, 2, 1),
                    grp = c("a", "b", "a", "b") )

# ggplot2 doesn't know you want to give the labels the same virtual width as bars:
ggplot(data = df, aes(x, y, group = grp)) +
    geom_col( aes(fill = grp), position = "dodge") +
    geom_text(aes(label = y), position = "dodge")
# So tell it:
ggplot(data = df, aes(x, y, group = grp)) +
    geom_col(aes(fill = grp), position = "dodge") +
    geom_text(aes(label = y), position = position_dodge(0.9))
# Use you can't nudge and dodge text, so instead adjust the y postion
ggplot(data = df, aes(x, y, group = grp)) +
    geom_col(aes(fill = grp), position = "dodge") +
    geom_text(  aes(label = y, y = y + 0.05), position = position_dodge(0.9), vjust = 0  )


############ using farcats  ########
monthlevels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# using farcats
gss_cat %>% mutate(marital = marital %>% 
                       fct_infreq() %>%    
                       fct_rev()) %>%
    ggplot(aes(marital)) +  geom_bar()


# fct_recode() for variables specified
# Can assign multiple old levels to the same new level: !!!
# collapse a lot of levels, fct_collapse() is a useful variant of fct_recode(). 

gss_cat %>%
    mutate(partyid = fct_collapse(partyid,
                                  other = c("No answer", "Don't know", "Other party"),
                                  rep = c("Strong republican", "Not str republican"),
                                  ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                  dem = c("Not str democrat", "Strong democrat")
    )) %>%
    count(partyid)

# lump together all the small groups with fct_lump():  !!
# Avoid overcollapse, use the n parameter to specify how many groups (excluding other) we want to keep:
gss_cat %>%
    mutate(relig = fct_lump(relig, n = 10)) %>%
    count(relig, sort = TRUE) %>%
    print(n = Inf)

# use like  ?   data(tips, package = "reshape2")
# plot by factore if no factor by alphabatic
#make factores will plot levelwise tips2$day <- factor(tips2$day,levels = c("Fri", "Sat", "Sun", "Thur"))

# sorting numerical freq with reorder
ggplot(tips2, aes(x = reorder(day, -perc), y = perc)) + geom_bar(stat = "identity")

#If you want to change the levels, 
#use `tips2$day <- factor(tips2$day,levels = c("First nice Name", "Second nice name", "jköljklj", "Thkljökljur"))`.

##################

#Center: mean(), median() ,  sd(), IQR(), mad(), min(), max(), quantile(), first(), last(), nth(),
#Count: n(), n_distinct(), any(), all()


lf18cause %>% group_by(SHED) %>%
    summarise(totalm  = n() , 
              mxdate = max(Date), 
              mndate = min(Date) , 
              tressd= sd(Date)   )  -> lf16pm

lf16pm

lf16pm %>% arrange( totalm ) %>% top_n( 10 , totalm ) %>%   #top_n(x, n, wt) wt =  variable for ordering
    ggplot(aes(  x= reorder(SHED, totalm) ,  y = totalm ) ) +
    geom_bar(  stat = "identity", fill = "dark red" , alpha = 0.2 ) +
    
    labs(title = "The top 10 highest failure sheds" )  +
    
    labs(y= " The no failures") +
    labs(x= "The major sheds") +
    coord_flip()  + theme_bw() +
    geom_text(  aes(label = totalm) ,   hjust = 01.50,  color = "#FF22FF" ) 
# scale_y_continuous(labels = scales::comma_format()) 



lf16pm
lf16pm %>% filter(year(mxdate) == 2017 |year(mxdate) == 2016 ) %>% 
    arrange( totalm ) %>% top_n(10, totalm ) %>%
    ggplot(aes(x = reorder(SHED, -desc(totalm)), y = totalm)) +
    geom_bar(stat = "identity", fill = "dark red") +
    labs(title = "Busiest container ports in the world") +
    labs(subtitle = '2014, in order of shipping volume') +
    labs(x = "Port", y = "Shipping\nVolume") +
    scale_y_continuous(labels = scales::comma_format()) +
    coord_flip()



#########################

mutate( fdate = coalesce(fdate, pldate ) )


#ggplot(tips2, aes(x = reorder(day, -perc), y = perc)) + geom_bar(stat = "identity")

#tips2$day <- factor(tips2$day,levels = c("Fri", "Sat", "Sun", "Thur"))
#ggplot(tips2, aes(x = day, y = perc)) + geom_bar(stat = "identity")


shedwise %>%
    ggplot() + geom_bar(aes(x= fdate, fill = shed) ) + #, position = "dodge") + 
    scale_x_date( limits = as.Date(c("2016-04-01","2016-05-01")) ) +
    scale_x_date(position = "top") +
    # scale_x_date(breaks      = as.Date(c("2016-05-01", "2016-07-01")),
    #        minor_breaks = as.Date(c("2016-04-01", "2016-01-15", "2016-08-01", "2016-11-01"))) +
    # scale_x_date( expand = c(0, 10) )   +
    labs(title = "Title As on date 25 june" ) +
    labs(subtitle = "As on date 25 june" ) +
    #coord_flip() +
    theme_bw() +
    ggtitle("limits = as.Date(c(\"1980-01-01\",\"2000-01-01\"))") 


###########  tidyverse - tidr package gather, spread, unite, separate


stocks <- data.frame(    time = as.Date('2009-01-01') + 0:9,
                         X = rnorm(10, 0, 1),   Y = rnorm(10, 0, 2),  Z = rnorm(10, 0, 4)  )
stocks
stocksm <- stocks %>% gather(stock, price, -time)
stocksm
stocksm %>% spread(stock, price)
stocksm %>% spread(time, price)


shedwise
shedwise %>% #select(make, shed, locono, shedrly) %>%
    spread(., key = make , value = shed ) #??


# unite columns names by unite( "new collumn in quotes", fields, sep = "-etc" )
nlf16 %>% unite(col = "shedrly", shed, shedrly, sep = " of " )   # good worked

#separate(data, col, into, sep = "[^[:alnum:]]+")
#separate(my_data4, col = "Murder_Assault", into = c("Murder", "Assault"), sep = "-")
separate(nlf16, "fdate", into = c("YEAR", "Month", "day"), sep = "-")   #worked


##############

#?count(shedwise, locono) %>% group_by(shed) %>%  arrange(desc(n))



########### learn 
select()
# inside you ma use 
# starts_with(), ends_with(), contains(), matches(), num_range(), one_of(), everything()
#To drop variables, use -. ,  except for :, - and c(), all complex expressions are evaluated outside the data frame
# The three scoped variants of select() (select_all(), select_if() and select_at()) 
# three variants of rename() (rename_all(), rename_if(), rename_at()) 
# tibble::rownames_to_column(). to prserve colomn names





#########################
lf16  %>% 
    mutate( Date = as.Date(Date, "%m/%d/%Y")) %>%  
    mutate( 'PL-DATE' = as.Date('PL-DATE', "%m/%d/%Y")) 

str(lf16)
names(lfnr16) -> nlf
colnames(lfnr16)
nlf

lfnr16 <- select(lfnr16,  "Terrotory" , "PL", PLdate =  "PL-DATE" , "Date" ,  "Link owner", "Loco no."  , "SHED" )


lf <- dplyr::rename(lf16,  "PLDATE" = "PL-DATE"   )
View(lf)
lf <- select(lf, !is.na(SHED))


lf %>% 
    filter( SHED == "TKD" | SHED == "LDH" | SHED == "LKO" | SHED == "AMV" | SHED == "SSB" | SHED == "KLK") %>%
    ggplot() + geom_bar(aes(x=SHED, fill = SHED) ) + theme_bw()  # + coord_flip() 


mutate(lf16, Date = coalesce(Date, PL-DATE) )           #collese

lf16 <- distinct(lf16)

filter(lf16, !is.na(Date)  )

filter(lf16, is.na(Month))

filter(lf16, is.na(RLY) & is.na(SHED))

filter(lf16, is.na(SHED))

lfnr16 <- filter(lf16,  SHED == "TKD" | SHED == "LDH" | SHED == "LKO" | SHED == "AMV" | SHED == "SSB" | SHED == "KLK")

??lfnr16 %>% dplyr::rename( c( "as.Date(PLDATE)" = "as.Date(PL-DATE)" ) ) %>% View()

## workaround
names(lfnr16$`PL-DATE`) <- "PLDATE"
names(lfnr16)
lfnr16$`PL-DATE`
lfnr16$`Date`
ifelse(is.na(lfnr16$Date), lfnr16$`PL-DATE`, lfnr16$Date)

lfnr16 %>% mutate( Date = coalesce(Date, PL-DATE) ) %>% View()

spec(lfnr16)

?rename
# eg filter(!CARRIER %in% c("UA", "AA") & ORIGIN == "SFO" )
filter(lf16, !SHED %in% c("TKD", "LDH", "LKO", "AMV", "KLK" )  )

names(lf16)
colnames(lf16)
row.names(lf16)
rownames(lf16)

# rectify the data 
####### rectify data

# check if Date = PL-DATE
filter(lf16, !("Date" == "PL-DATE") )

#lf16 %>% mutate(x = coalesce(x, y))
mutate(lf16, Date = as.Date(Date, "%m/%d/%Y") )
mutate(lf16, PL-DATE = as.Date(PL-DATE), "%m/%d/%Y") 

??mutate(lf16,  Date1 = as.Date(Date, "%m/%d/%Y"), PL-DATE1 = as.Date(PL-DATE, "%m/%d/%Y")  ) 
??mutate(lf16, PLDATE = as.Date(PL-DATE), "%m/%d/%Y" )  

lf16
mutate(lf16, Date = coalesce(Date, PL-DATE) )

## ? df %>% mutate(x = ifelse(is.na(x), y, x))
## ? df[is.na(x),x := y]

## fill(data, ..., .direction = c("down", "up"))

## discount_data_df %>%  mutate(Date = as.Date(Date)) %>% 
##   complete(Date = seq.Date(min(Date), max(Date), by="day")) %>%   fill(`Discount Rate`)

##discount_data_df %>% mutate(Date = as.Date(Date)) %>%
##   complete(Date = seq.Date(min(Date), max(Date), by="day")) %>%
##  group_by(Product) %>%   fill(`Discount Rate`)


# check 
#? filter(lf16, PL_DATE == Date )
filter(lf16, PL_DATE == Date )

#??spread(lf16, SHED, Month)

# names(lf17) <- lf17[1,]



# gs_read() is a wrapper  common methods to read data from the API and transform it for downstream use.

View(lf17)
lf17




# plot histogram
ggplot(lf16) +  geom_histogram(aes(x = Month, fill = Month), stat = "count" ) + theme_bw() + coord_flip() 
ggplot(lf16) +  geom_histogram(aes(x = Month              ), stat = "count" ) + theme_bw() + coord_flip() 

#ggplot(   ) +  geom_boxplot( lf17, aes(  x=reorder(format(lf17$X1,'%B'), lf17$X1),  stat="count" )

ggplot(lf16) +  geom_histogram(aes(x=Month, fill = Month), stat = "count" ) + theme_bw()

ggplot(lf17) +  geom_histogram(aes(x=X1 , fill = X1), stat="count")   + theme_bw() # good looking

ggplot(lf17) +  geom_histogram(aes(x=X2 , fill = X2), stat="count") + coord_flip()  + theme_bw()







#remove first k row being the file name  slice( k:n() )

# plot histogram

#ggplot(   ) +  geom_boxplot( lf17, aes(  x=reorder(format(lf17$X1,'%B'), lf17$X1),  stat="count" )

ggplot() +  geom_histogram( df= lf16, aes(x=Month) , stat = count )+ coord_flip()  + theme_bw()

ggplot(lf17) +  geom_histogram(aes(x=X1 , fill = X1), stat="count")   + theme_bw() # good looking

ggplot(lf17) +  geom_histogram(aes(x=X2 , fill = X2), stat="count") + coord_flip()  + theme_bw()

# names(lf17) <- lf17[1,]

##########
# working example rightward flow aesthetics for vaccine survey data
data(vaccinations)
vaccinations
levels(vaccinations$response) <- rev(levels(vaccinations$response))
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           y = freq, fill = response,  label = round(a, 3)   )     )  +
    geom_lode() +  geom_flow()  +
    geom_stratum(alpha = 0)     +
    geom_text(stat = "stratum")

# time series alluvia of WorldPhones data  # not wow
wph <- as.data.frame(as.table(WorldPhones))
names(wph) <- c("Year", "Region", "Telephones")
ggplot(wph,  aes(x = Year, alluvium = Region, y = Telephones)) +
    geom_flow(aes(fill = Region, colour = Region), width = 0)
# time series bump chart
gg <- ggplot(alluvial::Refugees,  aes(y = refugees, x = year, alluvium = country))

gg + geom_alluvium(aes(fill = country, colour = country),
                   width = 1/4, alpha = 2/3, decreasing = FALSE)
# time series line plot of refugees data, sorted by country
gg + geom_alluvium(aes(fill = country, colour = country),
                   decreasing = NA, width = 0, knot.pos = 0)


####### ggdendro ########
library(ggdendro)
hc  <-  hclust(dist(USArrests), "ave")
hc
hcdata <- dendro_data(hc, type="rectangle")
hcdata
ggplot() + 
    geom_segment(data=segment(hcdata), aes(x=x, y=y, xend=xend, yend=yend)) +
    geom_text(data=label(hcdata), aes(x=x, y=y, label=label, hjust=0), size=3) +
    coord_flip() + 
    scale_y_reverse(expand=c(0.2, 0))

####  plotting directly from object class hclust ####
ggdendrogram(hc)
ggdendrogram(hc, rotate=TRUE)

### demonstrate converting hclust to dendro using dendro_data first
hcdata <- dendro_data(hc)
ggdendrogram(hcdata, rotate=TRUE) + labs(title="Dendrogram in ggplot2")

# alluvial
library(alluvial)

lf18 %>% group_by( Terrotory, SHED, RLY) %>%  count(SHED)-> locof
#remove missing values
loco2 <-  locof  %>%  replace_na(list( Month = "UN", SHED = "UN", RLY = "UN")) %>% 
    arrange(SHED)               %>% 
    filter(Terrotory == "NR")   %>% 
    mutate(  sheds = paste(RLY, SHED)  )   #,   k = pal[ match(ss, sort(unique(ss))) ]
loco2

alluvial(       loco2[ , c(1 , 3,  5 , 4) ],   
                col   =  ifelse( loco2$RLY == "NR",   "red" ,  c( "pink", "green", 4:86)  ) , # color ordering of layers
                # col    = ifelse( loco2$RLY == "NR",   "red" ,  "blue") ,
                # border = ifelse( loco2$RLY == "NR",   "red" ,  "blue") ,
                freq  = loco2$n , 
                hide  = loco2$n < 10 , 
                axis_labels = c("Territorial", "Resp.Rly", "Sheds", "Total"), #else default to cname
                # blocks = FALSE, # colour blocks also
                xw = 0.0,        # connection curvature 0 better
                cw = 0.1,        # block width inches?
                cex = 0.7,       # Lable text size
                cex.axis = 0.8,  # axis lable sixe
                gap.width=0.001 , 
                layer= c( 86:1 ), # list all layer order to reorder layers
                alpha = 0.4               
                # ordering = list( NULL, order(tit$RLY, tit$Terrotory == "NR" ),  NULL  ) #order(tit$SHED, tit$Terrotory == "NR" )
)   ->  lfalluviam
lfalluviam
??save_plot("setoutsalluv.png" , lfalluviam, base_aspect_ratio = 1.3)


################### Good so far   #########
###########  using ggalluviam  ######
library(ggalluvial)

loco2[ , c(1 ,2, 3,  5 , 4) ]

gg <- ggplot( loco2[ , c(1 , 2, 3,  5 , 4) ],
              aes(   y = n,
                     axis1 = Terrotory, 
                     axis2 = RLY, 
                     axis3 = sheds ,
                     axis4 = n  )  )  +
    geom_stratum()                +   
    geom_text(stat = "stratum", label.strata = TRUE)      +
    scale_x_discrete( limits = c("Terrotory", "RLY", "sheds", "n")  )
gg
gg + geom_flow(aes(fill   =  ifelse( loco2$RLY == "NR",   "green" ,  "red"  ),  
                   col   =  ifelse( loco2$RLY == "NR",   "red" ,  c( "pink", "green", 4:86)  ),  # color ordering of layers
                   # col   =  ifelse( loco2$RLY == "NR",   "red" ,  c( 2:8)  ),  # color ordering of layers
                   # alpha    = ifelse( loco2$RLY == "NR",   0.9 , 0.3 )
                   size = 3,
                   linetype = "dotted"
                   #
),
#alpha = 0.2), 
stat     = "alluvium",
#    aes.bind = TRUE,  
#xw = 0.0,        # connection curvature
#cw = 0.1,        # block width inches?
#cex = 0.007,       # Lable text size
#cex.axis = 0.8,  # axis lable sixe
#    gap.width=0.3 ,   
# lode.ordering = lode_ord,
lode.guidance = "leftward" ) +
    #  lode_rightward(n, i) +
    # geom_stratum(width = .4) +   
    labs( title= "Loco troubles compiles (sets outs) ", 
          x = "Category" ,  y = "Total troubles reported"  )  +
    # geom_alluvium(fill = "darkgrey", na.rm = TRUE)  +
    # geom_stratum(aes(fill = sheds), color = NA, na.rm = TRUE)  # contnewus to descret ?
    # coord_flip()  +
    # facet_wrap(~ RLY, scales = "fixed") +
    theme(legend.position = "none" ) + # c(0.8, 0.8)) +
    theme_base() 

# cowplot::plot_grid(iris1, iris2, labels = "AUTO")



###################
arrange(loco2, RLY) -> loco2
alluvial(  loco2[,c(1,3,2)],   freq   = loco2$n  )

# or with bells
# ord <- list(NULL, with(tit3d, order(Sex, Survived)), NULL)
ord <- list(NULL, with(loco2, order(SHED, n)))

alluvial(tit[,2:3], 
         freq   = tit$n,
         col    = ifelse(tit$RLY == "NR", "red", "blue"),
         border = ifelse(tit$SHED == "TKD", "blue", "grey"),
         hide   = tit$n < 12, #uncomplicate by ignoring small
         cex    = 0.7,    #font size
         alpha  = 0.4    #  , blocks=FALSE   # for merging; ? 
         #?? , ordering=ord  #ord <- list(NULL, with(tit, order(RLY, n)))
         # other options ...
)

######

# to complete    group_by(Product) %>%   fill(`Discount Rate`) # for group wise
#                 complete(Date = seq.Date( min(Date), max(Date), by="days"))  
# incompletedata  %>% group_by(Date) %>%  tally(Date) %>% 
#                    complete( `PL-DATE` = seq.Date(  min(`PL-DATE`),  max(`PL-DATE`),  by="day" )  )
#
# write.csv( lf18,  "/Users/gk/Google Drive/locofailjan19.csv")
# write_csv(x, path, na = "NA", append = FALSE, col_names = !append) # faster , write_delim
# write.table(lf18, file = "foo.csv",  sep = ","  ,  col.names = NA,  qmethod = "double")
# read.table(   "foo.csv", sep = ",",    row.names = 1 ,  header = TRUE   ) 

#######################
dfm <- mtcars
dfm$cyl <- as.factor(dfm$cyl)   # Convert the cyl variable to a factor
dfm$name <- rownames(dfm)      ## Add the name colums
dfm$mpg_z <- (dfm$mpg -mean(dfm$mpg))/sd(dfm$mpg)
dfm$mpg_grp <- factor(ifelse(dfm$mpg_z < 0, "low", "high"), levels = c("low", "high"))

ggbarplot(dfm, x = "name", y = "mpg_z",
          fill = "mpg_grp",           # change fill color by mpg_level
          color = "white",            # Set bar border colors to white
          palette = "jco",            # jco journal color palett. see ?ggpar
          sort.val = "desc",          # Sort the value in descending order
          sort.by.groups = FALSE,     # Don't sort inside each group
          x.text.angle = 90,          # Rotate vertically x axis texts
          ylab = "MPG z-score",
          legend.title = "MPG Group",
          rotate = TRUE,             # Rotate axis ~ coord flif
          ggtheme = theme_minimal()
)


ggdotchart(dfm, x = "name", y = "mpg_z",
           color = "cyl",                                # Color by groups
           palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
           sorting = "descending",                       # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           add.params = list(color = "lightgray", size = 2), # Change segment color and size
           group = "cyl",                                # Order by groups
           dot.size = 6,                                 # Large dot size
           label = round(dfm$mpg_z,1),                        # Add mpg values as dot labels
           font.label = list(color = "white", size = 9,  vjust = 0.5) #,   # Adjust label parameters
           # ggtheme = theme_pubr()                        # ggplot2 theme
)   # +  geom_hline(yintercept = 0, linetype = 2, color = "lightgray")
#########################################
