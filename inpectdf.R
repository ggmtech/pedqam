#First install devtools to allow you to install inspectdf from github
#install and load the package - https://github.com/alastairrushworth/inspectdf
install.packages("devtools")
library(devtools)
devtools::install_github("alastairrushworth/inspectdf")

library(inspectdf)   # load the package full code is available on my  github repo.
# you can execute the following R commands to download the whole repo through R
# install.packages("usethis")  library(usethis)
# use_course("https://github.com/lgellis/MiscTutorial/archive/master.zip")

library(tidyverse)
library(readr)

#Download the data set
df= read_csv('https://raw.githubusercontent.com/lgellis/STEM/master/DATA-ART-1/Data/FinalData.csv', col_names = TRUE)
df

# explore # dim(df)  # glimpse(df)  # summary(df)  # 
# library(skimr)  skim(df)
# library(visdat)   # vis_miss(df)  # vis_dat(df)
# library(DataExplorer)  # DataExplorer::create_report(df)

allGrades <- df

oldGrades   <- allGrades %>%   filter(Grade > 5)
youngGrades <- allGrades %>%   filter(Grade < 6)

#View the distribution of grade to ensure it was split properly
ggplot(oldGrades, aes(x=Grade)) + geom_histogram()
ggplot(youngGrades, aes(x=Grade)) + geom_histogram()

# inspect_types() command to very easily see a breakdown of character vs numeric variables
inspect_types(allGrades, show_plot = TRUE)
inspect_types(youngGrades, oldGrades, show_plot = TRUE)

# inspect_mem() function will tell us some basic sizing information, including data frame columns, rows, total size and the sizes of each variable
inspect_mem(allGrades, show_plot = TRUE)
inspect_mem(youngGrades, oldGrades, show_plot = TRUE)

# inspect_na() function shows us the percentage of na values for each variable
inspect_na(allGrades, show_plot = TRUE)
inspect_na(youngGrades, oldGrades, show_plot = TRUE)

# inspect_num() function shows us the distribution of the numeric variables
inspect_num(allGrades, show_plot = TRUE)
inspect_num(youngGrades, oldGrades, show_plot = TRUE)

# inspect_imb() function allows us to understand the a bit about the value distribution for our categorical values
inspect_imb(allGrades, show_plot = TRUE)
inspect_imb(youngGrades, oldGrades, show_plot = TRUE)

# inspect_cat() allows us to visualize the full distribution of our categorical values.
inspect_cat(allGrades, show_plot = TRUE)
inspect_cat(youngGrades, oldGrades, show_plot = TRUE)

# Pearson correlation coefficient, r, can take a range of values from +1 to -1
inspect_cor(allGrades, show_plot = TRUE)
inspect_cor(youngGrades, oldGrades, show_plot = TRUE)

# with my locofailures
lf20 <- load()
inspect_cor(lf20, show_plot = TRUE)
