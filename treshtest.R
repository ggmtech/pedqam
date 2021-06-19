packages <- c("tidyverse", "lubridate", "readxl", "writexl","stringr", 
              "here", 
              "cowplot", # "ggpubr", 
              "scales",  "timetk",
              "summarytools",  
              "googledrive",  "googlesheets4", 
              "knitr",  "kableExtra",
              "fuzzyjoin",
              "alluvial", "ggalluvial")

# if not Instaled 
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) { install.packages(packages[!installed_packages]) }

lapply(packages, library, character.only = TRUE) # %>% invisible()




if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ggtree")
browseVignettes("ggtree")
#?install.packages(ggtree)

library(ggtree)
set.seed(2017-02-16)
tree <- rtree(50)
ggtree(tree)
ggtree(tree, layout="roundrect")
ggtree(tree, layout="slanted")
ggtree(tree, layout="ellipse")
ggtree(tree, layout="circular")
ggtree(tree, layout="fan", open.angle=120)
ggtree(tree, layout="equal_angle")
ggtree(tree, layout="daylight")
ggtree(tree, branch.length='none')
ggtree(tree, layout="ellipse", branch.length="none")
ggtree(tree, branch.length='none', layout='circular')
ggtree(tree, layout="daylight", branch.length = 'none')

