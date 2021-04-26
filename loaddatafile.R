
packages <- c("tidyverse",   "magrittr",  "here",
              
              "lubridate", "anytime",
              "googlesheets4", "readxl",
              "DataExplorer", "inspectdf", "summarytools", "skimr",
              "broom", "coefplot", "cowplot", "drat",
              
              "survival", "survminer", "ggfortify",
              "ggfortify", "DT", "knitr",
              "gridExtra","plotly", "timetk", "ggforce", "ggraph", 
              #"ggplot2", "dplyr", "tidyr", "magrittr", "stringr","forcats","reshape2",
              
              "ggpubr","scales", "GGally", "ggrepel", "ggridges",
              
              "ggthemes", #"themr",
              "viridis", "viridisLite",  
              "magick",
              "here", "devtools",
              #"cart", "psy", "car", "psych", "ordinal", "lmtest",  "dslabs",  
              "graphlayouts",  "interplot", "margins", 
            #"sf", "maps", "mapproj", "mapdata", "MASS", 
              "naniar", "prismatic"
)
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) { install.packages(packages[!installed_packages]) }
lapply(packages, library, character.only = TRUE) #%>% invisible()
?library(lubridate)
##################################################################################
#raw_img <- magick::image_read("http:link") ; magick::image_ggplot(raw_img)
here::here()  # getwd() and list.files()
ls()  # global env values in memory ;
##################################################################################


# Get Data raw ###################################################################
# googlesheets4::sheet_add() sheet_append() sheet_copy() sheet_delete() sheet_write() sheet_properties() sheet_rename()
# googlesheets4::read_sheet( ss, sheet = NULL, range = NULL, col_names = TRUE, col_types = NULL, or "cidDl"
#         skip = 0, na = "", trim_ws = TRUE , n_max = Inf  ,  guess_max = min(1000, n_max),  .name_repair = "unique" )

########## EM pads supplies data tidy ######################### 
myfile = "https://raw.githubusercontent.com/ggmtech/pedqam/master/EMPadsAnalysis.R"
myfile = read_file(myfile)

ss = "https://docs.google.com/spreadsheets/d/1lshFtdQf87ONyGdMHbwDRvRtWPM8B_bRPtuuq67P6xE" ## EM Pads supplies data Google Sheet
sheet_names(ss)  # see sheets names
EMpadsupplies <-  googlesheets4::read_sheet(ss, sheet = "EMpadsupplied" , col_names = TRUE,  col_types = "c"  , skip = 1, trim_ws = TRUE, na = "")  # col_types = "ccilDD"
EMpadsupplies %>% tibble::glimpse() #View()
