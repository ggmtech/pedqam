# Modeltime
library(xgboost)
library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)
# This toggles plots from plotly (interactive) to ggplot (static)
interactive <- FALSE

# Step 1 - Collect data and split into training and test sets.

m750 <- m4_monthly %>% filter(id == "M750")


# visualize the dataset.

m750 %>%  plot_time_series(date, value, .interactive = interactive)

# Split Data 80/20
splits <- initial_time_split(m750, prop = 0.9)

# We can easily create dozens of forecasting models by combining modeltime and parsnip. 
# ARIMA, Exponential Smoothing, Linear Regression, MARS (Multivariate Adaptive Regression Splines)
# Modeltime models (e.g. arima_reg()) are created with a date or date time feature
# most models include a formula like fit(value ~ date, data).
# Parsnip models (e.g. linear_reg()) typically should not have date features, but may contain derivatives of dates (e.g. month, year, etc). formulas like fit(value ~ as.numeric(date) + month(date), data).
# 
# Model 1: Auto ARIMA (Modeltime) # basic univariate  using “Auto Arima” using arima_reg()
model_fit_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(value ~ date, data = training(splits))  #> frequency = 12 observations per 1 year

# Model 2: Boosted Auto ARIMA (Modeltime) using arima_boost(). Boosting uses XGBoost 
# Model 2: arima_boost ----
model_fit_arima_boosted <- arima_boost( min_n = 2,  learn_rate = 0.015 ) %>%
                               set_engine(engine = "auto_arima_xgboost") %>%
                              fit(value ~ date + as.numeric(date) + 
              factor(month(date, label = TRUE), ordered = F),  data = training(splits))
#> frequency = 12 observations per 1 year
#
# Model 3: ets ---- Model 3: Exponential Smoothing (Modeltime)
model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(value ~ date, data = training(splits))
#> frequency = 12 observations per 1 year

# Model 4: Prophet (Modeltime) create a prophet model using prophet_reg().

model_fit_prophet <- prophet_reg() %>%
                     set_engine(engine = "prophet") %>%
                     fit(value ~ date, data = training(splits))


# Model 5: lm ---- Linear Regression (Parsnip)
model_fit_lm <- linear_reg() %>%
                set_engine("lm") %>%
  fit(value ~ as.numeric(date) + factor(month(date, label = TRUE), ordered = FALSE),
      data = training(splits))


# Model 6: MARS (Workflow) Multivariate Adaptive Regression Spline model using mars()
# 
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

# Step 3 - Add fitted models to a Model Table.
# The next step is to add each of the models to a Modeltime Table using modeltime_table()

models_tbl <- modeltime_table(
  model_fit_arima_no_boost,
  model_fit_arima_boosted,
  model_fit_ets,
  model_fit_prophet,
  model_fit_lm,
  wflw_fit_mars
)

models_tbl

# Step 4 - Calibrate the model to a testing set.

calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl

# Step 5 - Testing Set Forecast & Accuracy Evaluation
# There are 2 critical parts to an evaluation.

# Visualizing the Forecast vs Test Data Set
# Evaluating the Test (Out of Sample) Accuracy
calibration_tbl %>%
  modeltime_forecast(  new_data    = testing(splits),   actual_data = m750  ) %>%
  plot_modeltime_forecast(.legend_max_width = 25, # For mobile screens
                          .interactive      = interactive    )




