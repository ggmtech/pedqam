# time series


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
