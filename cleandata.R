#  clean data
# Showcasing the janitor package

library(tidyverse)
nurses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-05/nurses.csv')
names(nurses)

library(janitor)
library(dplyr) # load for pipe %>%  and later wrangling
names(nurses %>% clean_names)


# Remove empt and consst column
xl_file <- readxl::read_excel('dirty_data.xlsx', skip = 1) %>% 
  clean_names() %>%
  remove_empty() %>% 
  remove_constant()

# remove_empty() defaulted to remove, both, rows and colums. else which = 'rows'.


# Excel numeric date to date
xl_file %>% 
  mutate(hire_date = excel_numeric_to_date(hire_date))


# rounding
round_half_up(seq(0.5, 4.5, 1))
round_to_fraction(seq(0.5, 2.0, 0.13), denominator = 4)

# get_dupes() function is really powerful. It allows us to find “similar” observations in a data set based on certain characteristics

starwars %>% 
  get_dupes(eye_color, hair_color, skin_color, sex, homeworld) %>% 
  select(1:8)




# 
# 
#install.packages("gtsummary")
library(gtsummary)

