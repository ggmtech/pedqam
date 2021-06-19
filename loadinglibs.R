# load libraries
install.packages("easypackages")
library(easypackages)       #install.packages("easypackages")
packages("tidyverse", "rvest", "stringr", "janitor",
         "googlesheets4","data.table", 
         "rtweet", "tidytext", "rtweet", "wordcloud2", "patchwork",  
         "gameofthrones", 
         "ggimage", "magick", "ggpubr", "jpeg", "png")    # "cran.stats",

packages <- c("tidyverse", 
              "ggpubr",
              "lubridate", "stringr", "writexl", "readxl", 
              "parsedate", "janitor",
              "cowplot", "ggpubr",  "tidyquant", "patchwork",
              #"summarytools",  "here", "scales", 
              "googledrive" , "googlesheets4", 
              "eulerr",
              "knitr",  "kableExtra",  
              "alluvial", "ggalluvial",
             # "reticulate",
              "here")
installed_packages <- packages %in% rownames(  installed.packages() )
if (any(installed_packages == FALSE)) { install.packages(packages[!installed_packages]) }

lapply(packages, library, character.only = TRUE) # %>% invisible()

install.packages(reticulate)

library(reticulate)  # working with python

############ installing packages ####

# Bioconductor
install.packages("BiocManager") # Then install any package from Bioconductor using the BiocManager::install() function
BiocManager::install("ArrayTools")

# Github
install.packages("remotes") # GitHub by providing "username/repository" as argument to remotes::install_github()
remotes::install_github("tidyverse/ggplot2")

remotes::install_version("dplyr", "0.8.5") # for specific version 

install.packages("dplyr", repos = "https://cran.microsoft.com/snapshot/2017-03-15/") # from MRAN
# or globally set    options(repos = "https://cran.microsoft.com/snapshot/2017-03-15/")
install.packages("tidyverse", type = "source") # build from source

# see the package path
.libPaths()

###############





install.packages(c('tinytex', 'rmarkdown'))
tinytex::install_tinytex()
# after restarting RStudio, confirm that you have LaTeX with 
tinytex:::is_tinytex() 



#tinytex::tlmgr_install("babel-portuges")
tinytex::tlmgr_install("babel-portuges")


if (!require("remotes")) install.packages("remotes", repos = "https://cran.rstudio.org")
remotes::install_github("rstudio/bookdown")
remotes::install_github("ismayc/thesisdown")


if (!require("remotes")) install.packages("remotes", repos = "https://cran.rstudio.org")
remotes::install_github("rstudio/bookdown")
remotes::install_github("ismayc/thesisdown")

# Components of thesis: 
# _bookdown.yml  # main configuration file for your thesis.  You can change the name of your outputted file  and other options
# index.Rmd #  meta information that goes at the beginning of your document.   first page, the title of your thesis, etc. 
# 01-chap1.Rmd, 02-chap2.Rmd, etc.
# bib/    #  bibtex files) here. We recommend using the citr addin and Zotero to efficiently manage and insert citations.
# csl/    # Specific style files for bibliographies , good source for citation styles is https://github.com/citation-style-language/styles#readme.
# figure/
# data/   #  Store your figures and data here and reference them in your R Markdown files.
  
## Robert Hyndman
  
# install.packages("remotes")
remotes::install_github("robjhyndman/MonashEBSTemplates")
  
