# model time series forecasting  spark + R
# https://www.r-bloggers.com/2021/10/tidy-time-series-forecasting-in-r-with-spark/

install.packages(c("sparklyr", "modeltime", "tidyverse"), dependencies = TRUE)

# Java: Spark installation depends on Java being installed. Download Java here.
# Spark Installation: via sparklyr::spark_install() provided the user has sparklyr and Java installed above
library(sparklyr)
library(tidymodels)
library(modeltime)
library(tidyverse)
library(timetk)

# set up a Spark connection via sparklyr
# To run Spark locally:
sc <- spark_connect(master = "local")
# If using Databricks, you can use:
sc <- spark_connect(method = "databricks")

# Next register  Spark Backend using parallel_start(sc, .method = "spark"). a helper to set up registerDoSpark() foreach adaptor. In layman’s terms, this just means that we can now run parallel using Spark.

parallel_start(sc, .method = "spark")

# The dataset for forecasting is the walmart_sales_weekly ,  modified to just 3 columns: “id”, “date”, “value”.
walmart_sales_weekly %>%
  select(id, Date, Weekly_Sales) %>%
  set_names(c("id", "date", "value")) %>%
  group_by(id) %>%
  plot_time_series(date, value, .facet_ncol = 2, .interactive = F)

# prepare as nested data using the Nested Forecasting preparation functions.
# extend_timeseries():  extends each one of our time series into the future by 52 timestamps (one year for weeks).
# nest_timeseries(): converts  data to  nested data format indicating  our future data will be last 52 timestamps
# split_nested_timeseries():  adds indicies for the train / test splitting so we can develop accuracy metrics and determine which model to use for which time series.

nested_data_tbl <- walmart_sales_weekly %>%
  select(id, Date, Weekly_Sales) %>%    set_names(c("id", "date", "value"))  %>%
  extend_timeseries( .id_var   = id,   .date_var      = date,  .length_future = 52 ) %>%
  nest_timeseries(   .id_var   = id,                           .length_future = 52 ) %>%
  split_nested_timeseries(                                     .length_test = 52   )

nested_data_tbl
