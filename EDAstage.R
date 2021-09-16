library(tidyverse)
###########################################
# plain plots

data <- data.frame( A = rpois(900, 3),
                    B = rnorm(900),
                    C = runif(900) )

boxplot(data)


head(ToothGrowth)
boxplot(ToothGrowth)
boxplot(len~supp,
        data= ToothGrowth,
        main="ToothGrowth ",
        xlab="supp",
        ylab="len",
        col="orange",border="black"
       )
boxplot(len~dose,
        data=ToothGrowth,
        main="Different boxplots for per day growth",
        xlab="Tooth length",
        col="green",
        border="brown",
        horizontal=TRUE
       )


# in ggplot
library(ggplot2)
ToothGrowth %>% ggplot(   aes(x=as.character(supp), y=len)  ) +
                geom_boxplot(  fill="orange"   ) +
                labs(title=" ToothGrowth ", x="Supp", y="len") + theme_bw()

###############
library(ggplot2)
theme_set(   theme_classic()   +   theme(legend.position = "top")   )

set.seed(1234)
wdata = data.frame(   sex = factor(   rep(c("F", "M"),  each=200)   ),     ## demo data
                   weight = c( rnorm(200, 55),  rnorm(200, 58)   )    )

# line color by sex , change fill and outline color manually 
wdata  %>% ggplot( aes(x = weight)) +
           geom_histogram(  aes(color = sex, fill = sex), 
                            position = "identity", 
                            bins = 30, 
                            alpha = 0.4   )  +
           scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
           scale_color_manual(values = c("#00AFBB", "#E7B800")) 


data("mtcars")
df <- mtcars
df$cyl <- as.factor(df$cyl)  # Convert cyl as a grouping variable
head(df[, c("wt", "mpg", "cyl", "qsec")], 4)  # Inspect the data


# Bubble chart
df %>%  ggplot(  aes(x = wt, y = mpg)  ) + 
        geom_point(aes(color = cyl, size = qsec ), alpha = 0.5 ) +
        scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
        scale_size(range = c(0.5, 12))  # Adjust the range of points size




# 3D plot
library(plotly)
df <- mtcars %>%
  rownames_to_column() %>%
  as_tibble() %>%
  mutate(am = ifelse(am == 0, "Automatic", "Manual")) %>%
  mutate(am = as.factor(am))
df

# How to Create a GGPlot-like 3D Scatter Plot using Plotly
df <- mtcars %>%
  rownames_to_column() %>%
  as_data_frame() %>%
  mutate(am = ifelse(am == 0, "Automatic", "Manual")) %>%
  mutate(am = as.factor(am))
df

# Basic 3D Scatter Plot
# Create the plot
p <- plot_ly( df, x = ~wt, y = ~hp, z = ~qsec, 
              color = ~am, colors = c('#BF382A', '#0C4B8E')
            ) %>%
      add_markers() %>%
      layout( scene  = list(xaxis = list(title = 'Weight'),
               yaxis = list(title = 'Gross horsepower'),
               zaxis = list(title = '1/4 mile time'))
  )
p


# 3D Scatter Plot with Color Scaling,  colored according to the mpg variable:
# Point colors
marker <- list(color = ~mpg, colorscale = c('#FFE1A1', '#683531'),   showscale = TRUE)
# Create the plot
df %>%  plot_ly( x = ~wt, y = ~hp, z = ~qsec, marker = marker) %>%
     add_markers() %>%
     layout(    scene = list(xaxis = list(title = 'Weight'),
                 yaxis = list(title = 'Gross horsepower'),
                 zaxis = list(title = '1/4 mile time') )
      )  

# create icon 
# Helper theme for ggplot icon
theme_icon <- function () {  
                  theme_void() + 
                  theme( panel.background = element_rect(fill = "transparent", colour = NA), 
                          plot.background = element_rect(fill = "transparent", colour = NA), 
                        legend.background = element_rect(fill = "transparent", colour = NA), 
                    legend.box.background = element_rect(fill = "transparent", colour = NA) )
                            }

# Create an icon form a ggplot graphic
p <- ggplot(iris, aes(Species, Sepal.Length)) + 
  geom_boxplot(color = "#478bca", fill = "transparent") +
  theme_icon()
p

# Save into a 72x72 pixel png or svg format for the icon:
ggsave( filename = "test.svg", p, dpi=72, width = 1, height = 1   )
# PNG
ggsave(  filename = "test.png", p,  dpi=72, width = 1, height = 1, bg = "transparent" )


#  Hex sticker
library(hexSticker)
p <- ggplot(iris, aes(Species, Sepal.Length)) + 
     geom_boxplot(color = "white", fill = "transparent") +
     theme_icon()  # defined above fn

?p.sticker <- sticker(
  p, package=" ", p_size=3, 
  s_x=1, s_y=1.1, s_width=1.3, s_height=1.5,
  h_color = "#478bca", h_fill = "#478bca",
  filename="figures/boxplot-icon-sticker.png" )



# misssing data visualise using heatmap
library(heatmaply)
heatmaply_na( airquality[1:30, ], showticklabels = c(TRUE, FALSE)  )

# venn diagram

set.seed(20190708)
genes <- paste("gene", 1:1000, sep="")    ## Créer une donnée de démonstration
x <- list(
  A = sample(genes,300), 
  B = sample(genes,525), 
  C = sample(genes,440),
  D = sample(genes,350)
)
x
#if (!require(devtools)) install.packages("devtools")
#devtools::install_github("yanlinlin82/ggvenn")

library(ggvenn)
ggvenn( x, 
        fill_color = c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF"),
        stroke_size = 0.5, set_name_size = 4
       )


# if (!require(devtools)) install.packages("devtools")  
devtools::install_github("gaospecial/ggVennDiagram")  ## Utilisation du package R ggVennDiagram
library("ggVennDiagram")
ggVennDiagram(x, label_alpha = 0.2)


# install.packages("VennDiagram")
library(VennDiagram)
venn.diagram(x, filename = "venn-4-dimensions.png")

# check https://www.datanovia.com/en/fr/blog/diagramme-de-venn-avec-r-ou-rstudio-un-million-de-facons/

# basic functions
data = mtcars[5]
mean(data)
median(data)
sd(data)
var(data)
max(data)
min(data)
quantile(data)
summary(data)



# if (condition) {   code executed when condition is TRUE } else { code executed when condition is FALSE }
# for(i in 1:x) { code }
# while (test_expression) {  statement }
# repeat {  statement }

rep("A",4)
rep(1:5,2)
rep(1:5,rep(2,5))

# break and next statements

# if (test_expression) {
#            break
#            }


# switch(expression, case1, case2, case3....)

# scan statements

scan(file = "", what = double(), nmax = -1, n = -1, sep = "",
     quote = if(identical(sep, "\n")) "" else "'\"", dec = ".",
     skip = 0, nlines = 0, na.strings = "NA",
     flush = FALSE, fill = FALSE, strip.white = FALSE,
     quiet = FALSE, blank.lines.skip = TRUE, multi.line = TRUE,
     comment.char = "", allowEscapes = FALSE,
     fileEncoding = "", encoding = "unknown", text, skipNul = FALSE)


Square<-function(x){x*x}

order(data)
rev(order(data))
cbind(a,b) ; rbind(a,b)
identical()
sum()
paste()
is.na()
is.logical()
stopifnot()
length()


# moving average
df %>%
  ggplot(aes(x = dt, y = AverageTemperature)) +
  geom_line() + 
  tidyquant::geom_ma(ma_fun = SMA, n = 30)  +                 # Plot 30-day SMA
  tidyquant::geom_ma(ma_fun = SMA, n = 365, color = "red") +  # Plot 365-day SMA
  coord_x_date(xlim = c("1999-01-01", "2013-08-01")) + # Zoom in
  labs(x = "Year", y = "Average Temperature", title = "Temperature by Year", subtitle = "Copenhagen") +
  theme_minimal() +
  theme(text = element_text(size = 20))

# ma_fun = SMA
# Tidyquant has 6 types of moving average:
# simple moving averages (SMA),  # weighted moving averages (WMA) # volume-weighted moving averages (VWMA)
# exponential moving averages (EMA),  double exponential moving averages (DEMA)
# zero-lag exponential moving averages (ZLEMA)
# elastic, volume-weighted moving averages (EVMA)

###########################################

# EDA involves the following checks among others:
#     
#     Descriptive Statistics - Gives a summarized understanding of the data, usually as measures of central tendency and variability.
# Mean - Arithmetic average
# Median - middle value
# Mode - most frequent value
# Standard Deviation - variation from the mean
# Kurtosis -peakedness of the data distribution
# Skewness - symmetry of the data distribution
# Groupings of data
# Missing values
# ANOVA: Analysis of variance
# Graphical visualization, not restricted to:
#     Histogram -frequency bar plots
# Density estimation - an estimation if the frequency distribution based on sample data
# Box plots - A visual representation of median, quantiles, symmetry and ourtliers
# Scatter plots - a graphical display of variables plotted on the x and y axes.

#Loading Data into R  toc_depth: 2
mobileBanking.Df <- read.csv("J://Personalprojects//Blogsite//data//MobileBankinginKenya.csv", header = TRUE, stringsAsFactors = FALSE )
mobileBanking.Df <- mobileBanking.Df[,-1] #Remove the number column which is Irrelevant

#Load required packages or install if not present.----
load.libraries <- c('data.table','tidyverse','gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071', 'dplyr')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]

for(libs in install.lib) install.packages(libs, dependences = TRUE)

sapply(load.libraries, require, character = TRUE)  #Load libraries and flag TRUE

#Observing the data structure
dim(mobileBanking.Df)
cat_vars <- names(mobileBanking.Df)[which(sapply(mobileBanking.Df, is.character))]
cat_vars

summary(mobileBanking.Df)
table(mobileBanking.Df$Gender)
table(mobileBanking.Df$Gender, mobileBanking.Df$MB.Safety)
prop.table(table(mobileBanking.Df$Gender))


#install.packages("DataExplorer")
library(DataExplorer)
web <- mtcars
glimpse(web)
 
library(DataExplorer)
DataExplorer::create_report(airquality)
DataExplorer::create_report(diamonds, y = "price")
# Instead of running create_report, you may also run each function individually for your analysis, e.g.,

DataExplorer::introduce(web)

DataExplorer::plot_intro(web)
DataExplorer::plot_intro(web, ggtheme = theme_minimal(), title = "Automated EDA with Data Explorer", )

DataExplorer::plot_bar(diamonds, with = "price")
DataExplorer::plot_bar(diamonds, by = "cut")
DataExplorer::plot_qq(diamonds)


DataExplorer::plot_missing(web)
DataExplorer::plot_histogram(web)
DataExplorer::plot_density(web)
DataExplorer::plot_boxplot(web, by= 'month',  ncol = 2)
DataExplorer::plot_correlation(web, cor_args = list( 'use' = 'complete.obs'))
DataExplorer::plot_correlation(web, type = 'c',cor_args = list( 'use' = 'complete.obs'))
DataExplorer::plot_bar(web,maxcat = 20, parallel = TRUE)

DataExplorer::plot_bar(web,with = c("home"), maxcat = 20, parallel = TRUE)

DataExplorer::create_report(web)



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
plot_histogram(diamonds)        ## View histogram of all continuous variables
plot_density(diamonds)          ## View estimated density distribution of all continuous variables
plot_qq(diamonds)               ## View quantile-quantile plot of all continuous variables
plot_qq(diamonds, by = "cut")   ## View qq plot of all continuous variables by feature `cut`
plot_correlation(diamonds)      ## View overall correlation heatmap
plot_boxplot(diamonds, by = "cut")    ## View bivariate continuous distribution based on `cut`

plot_scatterplot(split_columns(diamonds)$continuous, by = "price", sampled_rows = 1000L)## Scatterplot `price` with all other continuous features
plot_prcomp(diamonds, maxcat = 5L)           ## Visualize principal component analysis

# To make quick updates to your data:
group_category(diamonds, feature = "clarity", threshold = 0.2, update = TRUE) ## Group bottom 20% `clarity` by frequency
group_category(diamonds, feature = "clarity", threshold = 0.2, measure = "price", update = TRUE) ## Group bottom 20% `clarity` by `price`
dummify(diamonds)                    ## Dummify diamonds dataset
dummify(diamonds, select = "cut")


df <- data.frame("a" = rnorm(260), "b" = rep(letters, 10))  ## Set values for missing observations
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





#######################
library(highcharter)
highchart() %>% 
    hc_title(text = "Scatter chart with size and color") %>% 
    hc_add_series(mtcars, "point", hcaes(wt, mpg, size=drat, color=hp)) %>%
    widgetframe::frameWidget()

data <- faithful[1:4, ]
knitr::kable(data, caption = 'Table with kable')
data <- faithful[1:4, ]
print(xtable::xtable(data, caption = 'Table with xtable'), type = 'HTML', 
      HTML.table.attributes = 'border=0')
data <- faithful[1:4, ]
stargazer::stargazer(data, type = 'HTML', title = 'Table with stargazer')

library(tidyverse, tidytext)
data %>% unnest_tokens(word, line) %>% anti_join(stop_words) %>% count(word, sort = TRUE)

