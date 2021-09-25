
library(librarian)
librarian::shelf(tidyverse)
tidyverse_packages()
# Core : readr, dplyr, tidyr, ggplot2, forcats,  purrr ,  stringr, tibble.
# non cre : blob , feather , jsonlite , glue , googledrive , haven , hms , lubridate , magritt, modelr , readxl , reprex , rvest  and xml2 

library(tidymodels) # Loads: broom, recipes, dials, rsample, yardstick, infer, tune
#  modeldata, workflows, parsnip, workflowsets 
# dplyr , tibble, ggplot2 , tidyr ,  purrr  ,
# broom messy output like lm, nls, or t.test into tidy data frames.
# dials create and manage values of tuning parameters.
# dplyr ggplot2  purrr tibble
# infer statistical inference.
# parsnip tidy, unified interface to creating models.
# recipes  data preprocessor , can create model matrices incorporating feature engineering etc
# rsample  resampling data so that models can be assessed and empirically validated.
# tune functions to optimize model hyper-parameters.
# workflows methods to combine pre-processing steps and models into a single object.
# yardstick  tools for evaluating models (e.g. accuracy, RMSE, etc.)
# tidymodels_conflicts(), tidymodels_prefer() to resolve common conflicts.

# 
# 
# R TIPS # TIP 044 | ggalt: dumbbell plots --  https://mailchi.mp/business-science/r-tips-newsletter
# LIBRARIES ----

library(tidyverse)
library(tidyquant)
library(ggalt)

# DATA ----
mpg

# 1.0 DATA WRANGLING ----
# - Learn dplyr in R for Business Analysis DS4B 101-R Course

mpg_by_year_tbl <- mpg %>%
    dplyr::select(hwy, year, model, class) %>%
    pivot_wider(
        names_from   = year,
        values_from  = hwy,
        id_cols      = c(class, model),
        values_fn    = function(x) mean(x, na.rm = TRUE),
        names_prefix = "year_"
    ) %>%
    mutate(model = fct_reorder(model, year_2008)) %>%
    drop_na()

mpg_by_year_tbl
# 2.0 VISUALIZATION (Dumbell Plots) ----
# - Learn ggplot2 in R for Business Analysis DS4B 101-R Course

# * Basic Dumbbell Plot with ggalt ----
g1 <- mpg_by_year_tbl %>%
    ggplot(aes(x = year_1999, xend = year_2008, y = model, group = model)) +
    
    geom_dumbbell(
        colour="#a3c4dc",
        colour_xend="#0e668b",
        size=4.0,
        dot_guide=TRUE,
        dot_guide_size=0.15,
        dot_guide_colour = "grey60"
    )

g1