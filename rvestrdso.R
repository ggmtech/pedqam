
# rvest vendors
# https://stackabuse.com/web-scraping-the-java-way/  # better
# https://www.webscrapingapi.com/java-web-scraping/
# 
# 
packages <- c("tidyverse", "lubridate","stringr", "parsedate", "janitor", 
              "cowplot", "ggpubr",  
              "rvest", "RSelenium", "RCurl",
              #"summarytools",  "here", "scales", "stringr", 
              "googledrive" , "googlesheets4", 
              "knitr",  "kableExtra",  
              "writexl", "readxl", 
              "alluvial", "ggalluvial")
installed_packages <- packages %in% rownames(  installed.packages() )
if (any(installed_packages == FALSE)) { install.packages(packages[!installed_packages]) }

lapply(packages, library, character.only = TRUE) # %>% invisible()

read_html("https://rdso.indianrailways.gov.in/")


myurl = "https://www.ireps.gov.in/epsn/cvap/admintab/viewItemMasterAnon.do"

tmp <- read_html(myurl) 
flat_html <- readLines(con = myurl)
flat_html 
str_length(tmp[3])
tmp[2]


# getURI(url, ..., .opts = list(), write = basicTextGatherer(.mapUnicode = .mapUnicode), curl = getCurlHandle(), async = length(url) > 1,  .encoding = integer(), .mapUnicode = TRUE)

# getURLContent(url, ..., curl = getCurlHandle(.opts = .opts), .encoding = NA,
#             binary = NA, .opts = list(...),
#             header = dynCurlReader(curl, binary = binary, baseURL = url, isHTTP = isHTTP, encoding = .encoding),
#             isHTTP = length(grep('^[[:space:]]*http', url)) > 0)

get_files <- RCurl::getURL(myurl, dirlistonly = TRUE)
get_files

get_files <- RCurl::getURLContent(myurl)


omegahatExists = url.exists("http://www.omegahat.net")
if(omegahatExists) { txt = getURL("http://www.omegahat.net/RCurl/")
                     if(require(XML))    htmlTreeParse(txt, asText = TRUE)    
                    }

# Test the passwords.
if(omegahatExists){ x = getURL("http://www.omegahat.net/RCurl/testPassword/index.html", userpwd = "bob:duncantl")
               # Catch an error for no authorization, specific "Unauthorized" error
               x = tryCatch(getURLContent("http://www.omegahat.net/RCurl/testPassword/index.html"),
               HTTPError = function(e) { cat("HTTP error: ", e$message, "\n")     })
}




#############
#############
#############
library(rvest)

hot100page <- "https://www.billboard.com/charts/hot-100"
hot100page <- "https://rdso.indianrailways.gov.in/"

hot100 <- read_html(hot100page)
hot100
str(hot100)

# we are always interested in the body of a web page. 
# You can select a node using html_node() and then see its child nodes using html_children().
body_nodes <- hot100 %>% 
  html_node("body") %>% 
  html_children()
body_nodes

# To go one level deeper, to  see the nodes inside the nodes.just continue to pipe deeper into the code. keep playing:
body_nodes %>% 
  html_children()

# We can use Chrome Developer to tell us where we can find the data in the code, 
# and then we can use rvest to harvest out the data.

# run your mouse over the code in the Developer to see that the elements of the page highlighted.
#  You can click to expand embedded nodes to get to more specific parts of the page to progressively drill down to  precise nodes that contain the details of each chart entry .
# eg we see is that each chart entry appears to be in <span> tag with the class names chart-element__rank__number, chart-element__information__artist and chart-element__information__song.

# use xml_find_all() to find all <span> nodes in the body of the document that have a class name containing the class names we want. 
# xml_find_all() accepts xpath syntax. Once those precise nodes are located,  use rvest::html_text() to extract:
  
rank <- hot100 %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//span[contains(@class, 'chart-element__rank__number')]") %>% 
  rvest::html_text()
artist <- hot100 %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//span[contains(@class, 'chart-element__information__artist')]") %>% 
  rvest::html_text()
title <- hot100 %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//span[contains(@class, 'chart-element__information__song')]") %>% 
  rvest::html_text()

chart_df <- data.frame(rank, artist, title)
chart_df
knitr::kable( chart_df ) %>% head(10)
