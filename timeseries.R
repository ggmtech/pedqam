# time series
#install.packages("tsbox") # Time series of the world, unite!


############# relocate at right place
# install.packages("prophet")  # with strong seasonal effects and historical data. robust to missing data and shifts in the trend, and typically handles outliers well.
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
###########

#remotes::install_github("christophsax/tsbox")
# tsbox built around a set of convertersof  time series stored as ts, xts, data.frame, data.table, tibble, zoo, tsibble, tibbletime, timeSeries, irts or tis to each other:
library(tsibble)
library(tibbletime)
library(timeSeries)
library(tis)

library(tsbox)
x.ts <- ts_c(fdeaths, mdeaths)
x.xts <- ts_xts(x.ts)
x.df <- ts_df(x.xts)
x.dt <- ts_dt(x.df)
x.tbl <- ts_tbl(x.dt)
x.zoo <- ts_zoo(x.tbl)
x.tsibble <- ts_tsibble(x.zoo)
x.tibbletime <- ts_tibbletime(x.tsibble)
x.timeSeries <- ts_timeSeries(x.tibbletime)
x.irts <- ts_irts(x.tibbletime)
x.tis <- ts_tis(x.irts)
all.equal(ts_ts(x.tis), x.ts)

# Thus for smooth, scale, differentiate, chain, forecast, regularize or seasonally adjust a time series, we can use the same commands to whatever time series class at hand:

ts_trend(x.ts)
ts_pc(x.xts)
ts_pcy(x.df)
ts_lag(x.dt)


# collect time series as multiple time series
ts_c(ts_dt(EuStockMarkets), AirPassengers)
ts_c(EuStockMarkets, mdeaths)

# combine time series to a new, single time series
ts_bind(ts_dt(mdeaths), AirPassengers)
ts_bind(ts_xts(AirPassengers), ts_tbl(mdeaths))

ts_plot(ts_scale(ts_c(mdeaths, austres, AirPassengers, DAX = EuStockMarkets[ ,'DAX'])))

# Cheatsheet  https://github.com/christophsax/tsbox

# ts_fn()
ts_c(mdeaths, austres)  # concat time sreies
ts_bind(mdeaths, austres) # combine time series to a new, single time series (first series wins if overlapping)
ts_chain(mdeaths, austres) #like ts_bind, but extra- and retropolate, using growth rates

# Plot time series of all classes and frequencies
ts_plot(mdeaths, austres)
ts_ggplot(mdeaths, austres)
ts_summary(ts_c(mdeaths, austres))

# Transform time series of all classes and frequencies

# ts_trend(): Trend estimation based on loess
ts_trend(fdeaths)

# ts_pc(), ts_pcy(), ts_pca(), ts_diff(), ts_diffy(): (annualized) Percentage change rates or differences to previous period, year
ts_pc(fdeaths)

#ts_scale(): normalize mean and variance
ts_scale(fdeaths)


# ts_index():Index, based on levels
# ts_compound(): Index, based on growth rates
ts_index(fdeaths, base = 1976)


# ts_seas(): seasonal adjustment using X-13 
ts_seas(fdeaths)

#ts_lag(): Lag or lead of time series
ts_lag(fdeaths, 4)
ts_plot(fdeaths, ts_lag(fdeaths, 15)) 

#ts_frequency(): convert to frequency
ts_frequency(fdeaths, "year")


#ts_span(): filter time series for a time span. 
ts_span(fdeaths, "1976-01-01")
ts_span(fdeaths, "-5 year") 

#converter function ts-boxable class
# ts_ts() ts, mts
# ts_data.frame(), ts_df() data.frame
# ts_data.table(), ts_dt() data.table
# ts_tbl() df_tbl, "tibble"
# ts_xts() xts
# ts_zoo() zoo
# ts_tibbletime() tibbletime
# ts_timeSeries() timeSeries
# ts_tsibble() tsibble
# ts_tslist() a list with ts objects

# Default structure to store multiple time series in long data frames (or data tables, or tibbles)
ts_df(ts_c(fdeaths, mdeaths))


# RESHAPE
# ts_wide(): convert default long structure to wide
# ts_long(): convert wide structure to default long
# 
# tsbox auto-detects a value-, a time- and zero, one or several
# id-columns. Alternatively, the time- and the value-column
# can be explicitly named time and value.
# ts_default(): standardize column names in data frames



# USE WITH PIPE
library(dplyr)
ts_c(fdeaths, mdeaths) %>%
        ts_tbl() %>%
        ts_trend() %>%
        ts_pc()

##########################################
# base
data("AirPassengers")
AP <- AirPassengers
str(AP)

ts(AP, frequency = 12, start=c(1949, 1)  )
plot(AP)

AP <- log(AP)  # log transform
plot(AP)

# Decomposition of additive time series
# Major components of time series

decomp <- decompose(AP)
decomp$figure
plot(decomp$figure,
     type = 'b',
     xlab = 'Month',
     ylab = 'Seasonality Index',
     col = 'blue',
     las = 2)


plot(decomp)


# Forecasting  ARIMA – Autoregressive Integrated Moving Average

library(forecast)
model <- auto.arima(AP)
model

# ACF and PACF plots
# It is always looking into ACF and PACF when we are dealing with time series data.

acf(model$residuals, main = 'Correlogram')

# The dotted lines are significant bounds. A log 0 its crossing the significance bound.
# Within 1 and 1.5 its just touching the significance bounds

pacf(model$residuals, main = 'Partial Correlogram' )

#Ljung-Box test
Box.test(model$residuals, lag=20, type = 'Ljung-Box')
# No significant difference was observed that indicates autocorrelation observed at lag 1 and 1.5 may be due to random chance.


# Residual plot
hist(model$residuals,
     col = 'red',
     xlab = 'Error',
     main = 'Histogram of Residuals',
     freq = FALSE)
lines(density(model$residuals))

# Forecast
f <- forecast(model, 48)
library(ggplot2)
autoplot(f)

accuracy(f)




###



# Time-series clustering
dturl = "https://raw.githubusercontent.com/finnstats/finnstats/main/synthetic_control.data.txt"
#data <- read.table(url(dturl))    # ok
data <- data.table::fread(dturl)  # ok
#data <- read.csv(url(dturl))      # wrong as truncated
# data <- readr::read_csv(dturl)  # failed
# readHTMLTable(dturl)
#data <- read.table("D:/RStudio/TimeseriesAnalysis/synthetic_control.data.txt", header = F, sep = "")  
str(data) 
plot(data[,60], type = 'l') 

j <- c(5, 105, 205, 305, 405, 505) 
sample <- t(data[j,]) plot.ts(sample,    main = "Time-series Plot",     col = 'blue',   type = 'b')


n <- 10
s <- sample(1:100, n)
i <- c(s,100+s, 200+s, 300+s, 400+s, 500+s)
d <- data[i,]
str(d)
pattern <- c(rep('Normal', n),
             rep('Cyclic', n),
             rep('Increasing trend', n),
             rep('Decreasing trend', n),
             rep('Upward shift', n),
             rep('Downward shift', n))

library(dtw)
distance <- dist(d, method = "DTW")
hc <- hclust(distance, method = 'average') #Hierarchical clustering
plot(hc,
     labels = pattern,
     cex = 0.7,
     hang = -1,
     col = 'blue')
rect.hclust(hc, k=4)

############
############
############
############
# timetk High-Performance Time Series Forecasting System (HPTSF) 
# 17 Algorithms: 8 hours of content on 17 TOP Algorithms. Divided into 5 groups:
# ARIMA
# Prophet
# Exponential Smoothing - ETS, TBATS, Seasonal Decomposition
# Machine Learning - Elastic Net, MARS, SVM, KNN, Random Forest, XGBOOST, Cubist, NNET & NNETAR
# Boosted Algorithms - Prophet Boost & ARIMA Boost
# Hyper Parameter Tuning: Strategies to reduce overfitting & increase model performance
# Time Series Groups: Scale your analysis from one time series to hundreds
# Parallel Processing: Needed to speed up hyper parameter tuning and forecasting at scale
# Ensembling: Combining many algorithms into a single super learner
# Deep Learning for Time Series
# GluonTS forecasting package  on top of mxnet (made by Amazon)
# Algorithms: Learn DeepAR, DeepVAR, NBEATS




library(tidyverse)
library(lubridate)
library(timetk)
# Timetk part of Modeltime Ecosystem for time series forecasting. 
# Ensembling and Resampling
# Machine Learning
# Deep Learning
# Scalable Modeling: 10,000+ time series
# Setup for the plotly charts (# FALSE returns ggplots)


interactive <- FALSE

timetk::taylor_30_min

taylor_30_min %>% 
        plot_time_series(date, value, 
                         .interactive = interactive,
                         .plotly_slider = TRUE)

m4_daily %>%
        group_by(id) %>%
        plot_time_series(date, value, 
                         .facet_ncol = 2, .facet_scales = "free",
                         .interactive = interactive)

m4_hourly %>%
        group_by(id) %>%
        plot_time_series(date, log(value),             # .value = log(value) Apply a Log Transformation
                         .color_var = week(date),      # .color_var = week(date) , Color applied to Week trfd
                         # Facet formatting
                         .facet_ncol = 2, 
                         .facet_scales = "free", 
                         .interactive = interactive)


# Can be converted from interactive plotly (great for exploring and shiny apps) to static ggplot2  reports).

taylor_30_min %>%
        plot_time_series(date, value, 
                         .color_var = month(date, label = TRUE),
                         
                         # Returns static ggplot
                         .interactive = FALSE,  
                         
                         # Customization
                         .title = "Taylor's MegaWatt Data",
                         .x_lab = "Date (30-min intervals)",
                         .y_lab = "Energy Demand (MW)",
                         .color_lab = "Month") +
        scale_y_continuous(labels = scales::comma_format())






###########################
