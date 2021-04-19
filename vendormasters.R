# webscaping
library(rvest)
library(stringr)
library(janitor)

library(easypackages)       #install.packages("easypackages")
packages("tidyverse", "rvest", "stringr", "janitor",
         "googlesheets4",
         "rtweet", "tidytext", "rtweet", "wordcloud2", "patchwork",  "data.table", 
         "gameofthrones", "ggimage", "magick", "ggpubr", "jpeg", "png")    # "cran.stats",



# stringr::str_trim(string, side = c("both", "left", "right"))
# stringr::str_squish()  reduces repeated whitespace inside a string.
# mtcars %>% mutate(mpg=replace(mpg, cyl==4, NA)) %>%   as.data.frame()
# data %>% mutate(X25 = ifelse(X25 == "Other", Other, X25))
# or data %>% mutate(X25 = case_when(X25=="Other"~ Other,  TRUE ~ X25))


#gs4_deauth()
# deaths_dribble <- googledrive::drive_get("deaths")# from gdrive sheet name
# ss <- gs4_create("newgooglesheet", sheets = list(flowers = head(iris), autos = head(mtcars)) ) #Creat new Sheet: "able-aardvark"
gs4_browse(ss)

vendormaster <- read_sheet("https://docs.google.com/spreadsheets/d/1FlEhwZ3Yn-lEs7Xb5qQXAlRQaqcQaKipK-XGU7wrHhg/edit#gid=526290481", sheet = 6 )
vendormaster

newvendors <- read_sheet("https://docs.google.com/spreadsheets/d/1FlEhwZ3Yn-lEs7Xb5qQXAlRQaqcQaKipK-XGU7wrHhg/edit#gid=526290481", sheet = 8 )
newvendors

######################
ss = "https://docs.google.com/spreadsheets/d/1rTTlYq5GlorsTVHcjGUPCmFGoyoDh0WSfv1vBxUjr6o/edit#gid=1476174023"
CRISvlist <- read_sheet(ss, sheet = 1)
CRISvlist %>% View()

str(CRISvlist)

# str_trim(x) only leading /training space
#str_replace_all(x, fixed(" "), "")   #  stringr::str_squish(OPIN) #str_replace_all(x, space(), "")

CRISvlist %>% mutate(OPIN = stringr::str_replace_all(OPIN, " ", "" ))   %>%
              mutate(  lpin =   str_length(OPIN) )                      %>%   # View()
              separate(OPHONE, into = c("SCODE", "Phone"), sep = "-" )   %>%   # View()
              mutate(Phone2 = if_else(!Phone, SCODE, Phone)) %>%
              googlesheets4::write_sheet( ss = ss)








# new worksheet within  existing (spread)Sheet with write_sheet().
write_sheet(my_data, ss = ss)




lsf.str("package:fuzzyjoin")
ls("package:fuzzyjoin")
# stringdist_join( x,y, by = NULL, max_dist = 2,
#                    method = c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex"),
#                    mode = "inner", ignore_case = FALSE, distance_col = NULL, ...  )

vendormaster[,3] 
newvendors[,3] 

names(vendormaster[,3] ) <- names(newvendors[,3])
names(vendormaster[,3] )
names(newvendors[,3])
joined <- vendormaster[,3] %>%  stringdist_inner_join( newvendors[,3] , by = c( "VENDOR NAME" = "Vendor") , max_dist = 3)

joined %>% View()  # view all those found approximate common in both the lsit

newvendors %>% anti_join( joined  ) %>% View()

# inner join, a semi join will never duplicate the rows in the left table
joined <- vendormaster[,3] %>%  stringdist_inner_join( newvendors[,3] , by = c( "VENDOR NAME" = "Vendor") , max_dist = 3)




??googlesheets4::sheet_write(iris,  ss = ss1, sheet = "iris")    # explicitly make a new sheet named "iris"
















(ss <- gs4_create("fluffy-bunny", sheets = list(flowers = head(iris)))) # brandnew googlesheet

# https://googlesheets4.tidyverse.org/articles/articles/write-sheets.html
# sheet_write() (over)writes a whole data frame into a (work)sheet within a (spread)Sheet. can create new
# sheet_append(), range_write(), range_flood(), and range_clear() are more specialized writing functions
# gs4_create() -> creation of a new (spread)Sheet. But it can create (work)sheets and write data.
# sheet_append() is about adding rows to an existing data table.
# gs4_create(), sheet_write(), and sheet_append() implement holistic operations for representing a single R data frame as a table in a (work)sheet within a Google Sheet.
# gs4_create() and sheet_write() both impose this mentality via specific formatting, such as special treatment of the column header row.
# range_write()  write arbitrary data into an arbitrary range, although it is still oriented around a data frame.
# range_flood() writes common content into all of the cells in a range.
# range_delete() deletes a range of cells and shifts other cells into the deleted area. clearing their existing values and, optionally, formatting.

ss2 <- gs4_create(   "sheets-create-demo-2",   sheets = c("alpha", "beta")  )

# create multiple wsheets
my_data_frames <- list(iris = head(iris), chickwts = head(chickwts))

ss4 <- gs4_create("sheets-create-demo-4",  sheets = my_data_frames )

# clean up/ delete 
gs4_find("sheets-create-demo") %>%   googledrive::drive_trash()

# sheet_write() can also write data into a new sheet, if sheet implicitly or explicitly targets a non-existent sheet.
# ss1 = googlesheet name
sheet_write(iris,  ss = ss1, sheet = "iris")    # explicitly make a new sheet named "iris"
sheet_write(mtcars, ss = ss1               )    # implicitly make a new sheet named "mtcars"

?sheet_write()


# or googledrive::drive_get("gapminder") %>% read_sheet()
#vendormaster <- read_sheet("1FlEhwZ3Yn-lEs7Xb5qQXAlRQaqcQaKipK-XGU7wrHhg")
# (ss <- gs4_create("fluffy-bunny", sheets = list(flowers = head(iris))))
# head(mtcars) %>% sheet_write(ss, sheet = "autos")
# sheet_append(), range_write(), range_flood(), and range_clear() 


ss = "1FlEhwZ3Yn-lEs7Xb5qQXAlRQaqcQaKipK-XGU7wrHhg"
#googlesheets4::
gs4_get() # metadata

sheets_get(ss)

sheet_properties(ss) # sheet details
sheet_names(ss) # names

# range =  "B3:F80"  | cell_limits(  c(1, 4), c(5, NA) | cell_rows(1:1000) )
# locale, trim_ws, na, comment, n_max

RDSOApril  <-  read_sheet(ss =ss ,  sheet = 6 , skip = 2, col_names = TRUE) # %>% rowid_to_column()

#googledrive::drive_trash(ss) deleate files



################### time series