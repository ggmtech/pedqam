#gktemplete
library(here)
load(here(), loadinglibs.R)


# loadingkubs.R for loading inital packages various means
# rnotes.R  small learning notes
# webscraptesting.R   webscraping functions and notes
# dataparsing.R
# plottinglibraries.R
# approximatedatamatch
# opencv  computer vision libraires
# forecasting.R  time data series forcating




# dplyr joins
inner_join(superheroes, publishers)    # All colums of x, y   but of rows of x having matchings in Y
inner_join(publishers, superheroes)

semi_join(superheroes, publishers)     # Only columns from x,  and filtred x  also in y, and unrepeated
semi_join(publishers, superheroes)

anti_join(superheroes, publishers)     # Only columns from x,  and rows data which are not in y
anti_join(publishers, superheroes)

left_join(superheroes, publishers)     # also right_join
left_join(publishers, superheroes)

full_join(superheroes, publishers)     # all combinaitons



#Its common to want to know if one data set is the same as another dataset
# dplyr’s setequal ;
# base R’s identical true if the datasets  exact same rows in the exact same order

#  Mutating Joins:   left_join, right_join, inner_join, full_join
# Filtering Joins:   semi_join, anti_join

#Set Operations:
union()
intersect()
setdiff()   # miinus sets
setequal()  #Comparisions:

#If the datasets have the same columns and all are used in the key, then intersect is analagous to semi_join
#In the same scenario setdiff is similar to anti_join


# Check if any order: definitive and union of complete and soundtrack
union(complete, soundtrack) %>%    setequal(definitive)   # true

#Assembling data  : # Binds
# Base R binds:  rbind, cbind
# dplyr  binds:  bind_rows, bind_cols       # faster, tibble , handle lists of dataframes



