# install.packages('devtools')
# devtools::install_github('thomasp85/gganimate')
library(gapminder)
library(gganimate)
library(tidyverse)
ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
# Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')


#

library(devtools)
# No Remotes ----
# Attachments ----
to_install <- c("catboost", "caTools", "data.table", "doParallel", "foreach", "forecast", "ggplot2", "h2o", "itertools", "lubridate", "monreg", "pROC", "RColorBrewer", "recommenderlab", "ROCR", "scatterplot3d", "stringr", "tm", "tsoutliers", "wordcloud", "xgboost", "zoo")
for (i in to_install) {
    message(paste("looking for ", i))
    if(i == "catboost" & !requireNamespace(i)) {
        devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')
    } else if(i == "h2o" & !requireNamespace(i)) {
        if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
        if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
        pkgs <- c("RCurl","jsonlite")
        for (pkg in pkgs) {
            if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
        }
        install.packages("h2o", type="source", repos="https://h2o-release.s3.amazonaws.com/h2o/rel-yates/3/R")
    } else if (!requireNamespace(i)) {
        message(paste("     installing", i))
        install.packages(i)
    }
}


# 2. Next, Install R package from GitHub
# Depending on the development state (future versions, etc.) you can install via:
devtools::install_github('AdrianAntico/RemixAutoML', upgrade = FALSE)
#or  ...you can install via:
devtools::install_github('AdrianAntico/RemixAutoML', force = TRUE, dependencies = TRUE, upgrade = FALSE)



library(RemixAutoML)
library(data.table)
library(dplyr)
library(magrittr)
library(ggplot2)
library(scales)
library(magick)
library(grid)

# IMPORT DATA FROM REMIX INSTITUTE BOX ACCOUNT ----------

# link to manually download file: https://remixinstitute.app.box.com/v/walmart-store-sales-data/
walmart_store_sales_data = data.table::fread("https://remixinstitute.box.com/shared/static/9kzyttje3kd7l41y1e14to0akwl9vuje.csv", header = T, stringsAsFactors = FALSE)
walmart_store_sales_data

# FIND TOP GROSSING STORE (USING dplyr) ---------------------

# group by Store, sum Weekly Sales
top_grossing_store = walmart_store_sales_data %>% dplyr::group_by(., Store) %>%
    dplyr::summarize(., Weekly_Sales = sum(Weekly_Sales, na.rm = TRUE))

# max Sales of 45 stores
max_sales = max(top_grossing_store$Weekly_Sales)

# find top grossing store
top_grossing_store = top_grossing_store %>% dplyr::filter(., Weekly_Sales == max_sales)
top_grossing_store = top_grossing_store$Store %>% as.numeric(.)

# what is the top grossing store?
print(paste("Store Number: ", top_grossing_store, sep = ""))


# FIND WEEKLY SALES DATA FOR TOP GROSSING STORE (USING data.table) ----------
top_store_weekly_sales <- walmart_store_sales_data[Store == eval(top_grossing_store),
                                                   .(Weekly_Sales = sum(Weekly_Sales, na.rm = TRUE)),
                                                   by = "Date"]


# FORECAST WEEKLY SALES FOR WALMART STORE USING AutoTS ------

# forecast for the next 16 weeks - technically 1 line of code, but
# each argument was dedicated its own line for presentation purposes
weekly_forecast = RemixAutoML::AutoTS(
    data = top_store_weekly_sales,
    TargetName = "Weekly_Sales",
    DateName = "Date",
    FCPeriods = 16,
    HoldOutPeriods = 12,
    TimeUnit = "week"
)


# VISUALIZE AutoTS FORECASTS ----------------

# view 16 week forecast
View(weekly_forecast$Forecast)

# View model evaluation metrics
View(weekly_forecast$EvaluationMetrics)

# which model won?
print(weekly_forecast$ChampionModel)

# see ggplot of forecasts
plot = weekly_forecast$TimeSeriesPlot
#change y-axis to currency
plot = plot + ggplot2::scale_y_continuous(labels = scales::dollar)
#RemixAutoML branding. Inspiration here: https://michaeltoth.me/you-need-to-start-branding-your-graphs-heres-how-with-ggplot.html
logo = magick::image_read("https://www.remixinstitute.com/wp-content/uploads/7b-Cheetah_Charcoal_Inline_No_Sub_No_BG.png")
plot
grid::grid.raster(logo, x = .73, y = 0.01, just = c('left', 'bottom'), width = 0.25)













# Annimation


library(ggplot2)
library(tidyverse)
library(gganimate)
library(directlabels)
library(png)
library(transformr)
library(grid)


df = data.frame(A=sample(1:75, 50, replace=TRUE),
                B=sample(1:100, 50, replace=TRUE),
                stringsAsFactors = FALSE)

ggplot(df, aes(A, B)) +
                    geom_line() +
                    transition_reveal(A) +
                    labs(title = 'A: {frame_along}')

p = ggplot(df, aes(A, B, group = C)) +
    geom_line() +
    transition_reveal(A) +
    labs(title = 'A: {frame_along}')

animate(p, nframes=40)


p = ggplot(df, aes(A, B, group = C)) +
    geom_line() +
    transition_reveal(A) +
    labs(title = 'A: {frame_along}')

animate(p, nframes=40)


anim_save("basic_animation.gif", p)

animate(p, nframes=40, fps = 2)

animate(p, renderer = gifski_renderer(loop = FALSE))

# You can change height and width of plot by mentioning the size in animate( ) function.

animate(p, fps = 10, duration = 14, width = 800, height = 400)


set.seed(123)
dates = paste(rep(month.abb[1:10], each=10), 2018)
df = data.frame(Product=rep(sample(LETTERS[1:10],10), 10),
                Period=factor(dates, levels=unique(dates)),
                Sales=sample(1:100,100, replace = TRUE))
head(df)
# Ranking by Period and Sales
df = df %>%
    arrange(Period, Sales) %>%
    mutate(order = 1:n())

# Animation
p = df %>%
    ggplot(aes(order, Sales)) +
    geom_bar(stat = "identity", fill = "#ff9933") +
    labs(title='Total Sales in {closest_state}', x=NULL) +
    theme(plot.title = element_text(hjust = 0.5, size = 18)) +
    scale_x_continuous(breaks=df$order, labels=df$Product, position = "top") +
    transition_states(Period, transition_length = 1, state_length = 2) +
    view_follow(fixed_y=TRUE) +
    ease_aes('cubic-in-out')

animate(p, nframes=50, fps=4)
anim_save("bar_animation.gif", p)

