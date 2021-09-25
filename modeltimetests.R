# modeltime and timetk
# 
# plot_time_series() # workhorse time-series function Generates plotly plots 
# Consolidates 20+ lines of ggplot2 & plotly code and scales Scales well to many time series
# interactive plotly to static ggplot2 plots
library(tidyverse)
library(lubridate)
library(timetk)

# Setup for the plotly charts (# FALSE returns ggplots)
interactive <- FALSE

taylor_30_min

taylor_30_min %>% 
  plot_time_series( date, 
                    value, 
                   .interactive = interactive,
                   .plotly_slider = TRUE)


m4_daily %>% group_by(id)

# Groups : by group_by() or by using the ... to add groups.
# Groups to facets:  .facet_ncol = 2  -> returns a 2-column faceted plot
#   .facet_scales = "free" , x and y-axis of each plot to scale independently of the other plots

m4_daily %>%
  group_by(id) %>%
  plot_time_series(date, 
                   value, 
                   .facet_ncol = 2, 
                   .facet_scales = "free",
                   .interactive = interactive)

m4_hourly %>% group_by(id)

m4_hourly %>%
  group_by(id) %>%
  plot_time_series(date, 
                   log(value),             # Apply a Log Transformation
                   .color_var    = lubridate::week(date),   # Color applied to Week transformation
                   .facet_ncol   = 2,         # Facet formatting
                   .facet_scales = "free", 
                   .interactive  = interactive )


# static ggplot
taylor_30_min %>%
  plot_time_series(date, value, 
                   .color_var = month(date, label = TRUE),
                   .interactive = FALSE,      # static ggplot, else .interactive = interactive
                   
                   # Customization
                   .title = "Taylor's MegaWatt Data",
                   .x_lab = "Date (30-min intervals)",
                   .y_lab = "Energy Demand (MW)",
                   .color_lab = "Month")  +
  scale_y_continuous(labels = scales::comma_format() )

taylor_30_min # date and value

#taylor_30_min %>%

m4_hourly %>%
  group_by(id) %>%
plot_time_series(
 # .data = taylor_30_min,
   .date_var = date,
   .value   = value,  # .values accepts transformations  eg log(sales)
   .color_var = NULL,
   .facet_vars = NULL,  # can manually supply facets as well.
   .facet_ncol = 2,   # group_by() if detected, returns multiple facets  
   .facet_scales = "free", #free_x", #free_y",
   .facet_collapse = TRUE,
   .facet_collapse_sep = " ",
   .line_color = "#2c3e50",
   .line_size = 0.5,
   .line_type = 1,
   .line_alpha = 1,
   .y_intercept = NULL,
   .y_intercept_color = "#2c3e50",
   .smooth = TRUE,
   .smooth_period = "auto",
   .smooth_message = FALSE,
   .smooth_span = NULL,
   .smooth_degree = 2,
   .smooth_color = "#3366FF",
   .smooth_size = 1,
   .smooth_alpha = 1,
   .legend_show = TRUE,
   .title = "Time Series Plot in R timetk",
   .x_lab = "Date labels",
   .y_lab = "value lables",
   .color_lab = "Legend",
   .interactive = TRUE,
   .plotly_slider = FALSE
)



# Can apply transformations to .value or .color_var
# - .value = log(adjusted)
# - .color_var = year(date)
library(tidyquant)
FANG %>%
  plot_time_series(date, log(adjusted),
                   .color_var    = year(date),
                   .facet_vars   = contains("symbol"),
                   .facet_ncol   = 2,
                   .facet_scales = "free",
                   .y_lab        = "Log Scale",
                   .interactive  = FALSE)


# 
# 
# modeltime
library(xgboost)
library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)
#install.packages("earth")
library(earth)
earth
earth::earth()

# This toggles plots from plotly (interactive) to ggplot (static)
interactive <- FALSE
m4_monthly
m750 <- m4_monthly %>% filter(id == "M750")
m750
# visualise
m750 %>%   plot_time_series(date, value, .interactive = interactive)


# Split Data 80/20
splits <- initial_time_split(m750, prop = 0.9)

# Model 1: auto_arima ----
model_fit_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(value ~ date, data = training(splits))  #> frequency = 12 observations per 1 year

# Model 2: arima_boost ----
model_fit_arima_boosted <- arima_boost( min_n = 2, learn_rate = 0.015 ) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(value ~ date + as.numeric(date) + factor(month(date, label = TRUE), ordered = F),
      data = training(splits))  #> frequency = 12 observations per 1 year

# Model 3: ets ----
model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(value ~ date, data = training(splits))
#> frequency = 12 observations per 1 year
#
# Model 4: prophet ----
model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(value ~ date, data = training(splits))
#> Disabling weekly seasonality. Run prophet with weekly.seasonality=TRUE to override this.
#> Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.

# Model 5: lm ----
model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(value ~ as.numeric(date) + factor(month(date, label = TRUE), ordered = FALSE),
      data = training(splits))

# Model 6: earth ----
model_spec_mars <- mars(mode = "regression") %>%  set_engine("earth") 


recipe_spec <- recipe(value ~ date, data = training(splits)) %>%
  step_date(date, features = "month", ordinal = FALSE) %>%
  step_mutate(date_num = as.numeric(date)) %>%
  step_normalize(date_num) %>%
  step_rm(date)


wflw_fit_mars <- workflow() %>%
  add_recipe(recipe_spec) %>%
  add_model(model_spec_mars) %>%
  fit(training(splits))

# Add fitted models to a Model Table.
models_tbl <- modeltime_table(
model_fit_arima_no_boost,
model_fit_arima_boosted,
model_fit_ets,
model_fit_prophet,
model_fit_lm,
wflw_fit_mars
)

models_tbl

# Calibrate the model to a testing set.
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl

# Testing Set Forecast & Accuracy Evaluation
# A - Visualizing the Forecast Test

calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = m750
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = interactive
  )

# Accuracy Metrics with  modeltime_accuracy() 
# Mean absolute error, mae(), Mean absolute percentage error, mape()
# Mean absolute scaled error, mase(), Symmetric mean absolute percentage error, smape()
# Root mean squared error, rmse()  ,   R-squared, rsq()
calibration_tbl %>%  modeltime_accuracy() %>%
                     table_modeltime_accuracy( .interactive = interactive )


# Refit to Full Dataset & Forecast Forward
refit_tbl <- calibration_tbl %>%  modeltime_refit(data = m750)

refit_tbl %>%
  modeltime_forecast(h = "3 years", actual_data = m750) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = interactive
  )
