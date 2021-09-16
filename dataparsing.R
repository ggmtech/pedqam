

library(janitor)
data<-read.csv("D:/RStudio/Website/FinData.csv",1)

clean <-clean_names(data)

clean_x<-clean %>% remove_empty(whic=c("rows"))
clean_x<-clean %>% remove_empty(whic=c("cols"))
# Remove duplicate records
clean %>% get_dupes(first_name)
clean %>% get_dupes(first_name,certification)
excel_numeric_to_date(41103)


# https://cran.r-project.org/web/packages/fedmatch/vignettes/Intro-to-fedmatch.html
## clean_strings(rownames) #lowercase,  names &, @, %, $ removes "," .  , tabs to spaces, rm extra spaces
## fedmatch::merge_plus() different types of matches: exact, fuzzy, and multivar.




# YUAM
# Data spliting for vendor data into separate fields

waldo::compare(df1,df2)

install.packages("prophet")  # with strong seasonal effects and several seasons of historical data. robust to missing data and shifts in the trend, and typically handles outliers well.
library(prophet)
library(readr)
df <- readr::read_csv('../tests/testthat/data.csv')
m <- prophet(df, seasonality=TRUE)        # seasonality=TRUE for disabling daily seasonality
future <- make_future_dataframe(m, periods = 365)

forecast <- predict(m, future)  # predict function to get our forecast:
head(forecast)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m, forecast)
prophet_plot_components(m, forecast)  # plot seasonality components





#######  Read google sheets 
ss = 
gsheet1  =            # QAM master lis of 613 vendors
gsheet2  =            # Mastersheet of vendor irps data


# antijoin on QAM vendor master list of 613
# left join of QAM vendor master


  
  
  
str_extract_all(text, "\\{.+?\\}")
#To only capture the text within the {}, one needs to use the regular expression's look behind and look head options.
str_extract_all(text, "(?<=(\\{)).+?(?=\\})")
# (?<=   ) Look behind this match
# \\{  look for the left curly bracket   
#  .+   with at least 1 character (any character)      
#  ?    do not perform a greedy match (without it will grab everything)    
# \\}  to the right curly bracket
# (?=   ) look head of match

###### 

# use separate to extract and splite columns


# write the ' cleaned' sheet in the same google sheet 