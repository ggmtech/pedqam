
# rvest vendors
packages <- c("tidyverse", "lubridate", "parsedate", "janitor",
              "cowplot", "ggpubr",  
              #"summarytools",  "here", "scales", "stringr", 
              "googledrive" , "googlesheets4", 
              "knitr",  "kableExtra",  
              "writexl", "readxl", 
              "alluvial", "ggalluvial")
installed_packages <- packages %in% rownames(  installed.packages() )
if (any(installed_packages == FALSE)) { install.packages(packages[!installed_packages]) }

lapply(packages, library, character.only = TRUE) # %>% invisible()



read_html(“address of website”)).
