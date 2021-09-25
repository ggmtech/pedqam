# YUAM
# Data spliting for vendor data into separate fields

# regexp search for an email address. \b[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,6}\b 
# phone  in 264 area code   \b1?[-( ]*264[-) ]*\d{3}[- ]*\d{4}\b 
# Date regexpr 
#  yyyy-mm-dd regexp  ^(19|20)\d\d[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])$
#  mm/dd/yyyy regexp  ^(0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])[- /.](19|20)\d\d$
#  dd-mm-yyyy format  ^(0[1-9]|[12][0-9]|3[01])[- /.](0[1-9]|1[012])[- /.](19|20)\d\d$
#  

library(stringr, lubridate)
string <- "<13>1 2018-04-18T10:29:00.581243+10:00 KOI-QWE-HUJ vmon 2318 - -  Some Description..."
string %>%   str_extract("\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}") %>%  ymd_hms() # YYYY-mm-dd HMSS


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