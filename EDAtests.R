
## install.packages("snakecase")
library(snakecase)
string <- c("lowerCamelCase", "ALL_CAPS", "I-DontKNOWWhat_thisCASE_is")

to_any_case(string)  # convert to snake case or with option any case
to_any_case(string, case = "parsed") # "lower_camel", "upper_camel", "all_caps", "lower_upper", "upper_lower", "sentence" and "mixed", which are based on "parsed" case:

to_snake_case(c("SomeBAdInput", "someGoodInput")) %>% dput()
## c("some_b_ad_input", "some_good_input")

library(tidyverse)
starwars
starwars %>% group_by(eye_color) %>% tally() # or
starwars %>% count(sex, eye_color)
starwars %>% add_count(eye_color, wt = birth_year) # mutating add_count
starwars %>% add_tally(wt = birth_year)


#if (!require(devtools)) install.packages("devtools")
devtools::install_github("boxuancui/DataExplorer")
library(DataExplorer)
create_report(airquality)              # To get a report for the airquality dataset:
create_report(diamonds, y = "price")   # To get a report for the diamonds dataset with response variable price:

# You may also run each function individually for your analysis, e.g.,


introduce(airquality)           ## View basic description for airquality data
plot_intro(airquality)          ## Plot basic description for airquality data


plot_missing(airquality)        ## View missing value distribution for airquality data

plot_bar(diamonds)                 ## Left: frequency distribution of all discrete variables
plot_bar(diamonds, with = "price") ## Right: `price` distribution of all discrete variables
plot_bar(diamonds, by = "cut")     ## View frequency distribution by a discrete variable
plot_histogram(diamonds)       ## View histogram of all continuous variables


## View estimated density distribution of all continuous variables
plot_density(diamonds)

## View quantile-quantile plot of all continuous variables
plot_qq(diamonds)


plot_qq(diamonds, by = "cut")                ## View quantile-quantile plot of all continuous variables by feature `cut`
plot_correlation(diamonds)                   ## View overall correlation heatmap
plot_boxplot(diamonds, by = "cut")           ## View bivariate continuous distribution based on `cut`
plot_scatterplot(split_columns(diamonds)$continuous, by = "price", sampled_rows = 1000L)## Scatterplot `price` with all other continuous features
plot_prcomp(diamonds, maxcat = 5L)           ## Visualize principal component analysis


# To make quick updates to your data:
group_category(diamonds, feature = "clarity", threshold = 0.2, update = TRUE)   ## Group bottom 20% `clarity` by frequency
group_category(diamonds, feature = "clarity", threshold = 0.2, measure = "price", update = TRUE)## Group bottom 20% `clarity` by `price`
dummify(diamonds)                                                         ## Dummify diamonds dataset
dummify(diamonds, select = "cut")


df <- data.frame("a" = rnorm(260), "b" = rep(letters, 10))           ## Set values for missing observations
df[sample.int(260, 50), ] <- NA
set_missing(df, list(0L, "unknown"))

## Update columns
update_columns(airquality, c("Month", "Day"), as.factor)
update_columns(airquality, 1L, function(x) x^2)

## Drop columns
drop_columns(diamonds, 8:10)
drop_columns(diamonds, "clarity")

###########
library(tidyverse)
library(timetk)
walmart_sales_weekly

walmart_sales_weekly %>%
  group_by(Store, Dept) %>%
  tk_anomaly_diagnostics(Date, Weekly_Sales)

walmart_sales_weekly %>%
  group_by(Store, Dept) %>%
  plot_anomaly_diagnostics(Date, Weekly_Sales, .facet_ncol = 2)

