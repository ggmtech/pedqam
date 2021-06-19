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