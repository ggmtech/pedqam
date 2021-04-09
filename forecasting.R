

# Data Exploration
library(tidyverse)
library(skimr)
library(lubridate)
library(tidytext)
library(timetk)
library(gt)

# color pallete
library(tidyquant)

# modeling
library(tidymodels)
library(modeltime)

# reading the data and converting categorical features to factor
exp_imp_tbl <- read_csv("data/data_comexstat.csv") %>% mutate_if(is.character, as_factor)


skim(exp_imp_tbl) 