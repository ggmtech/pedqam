# shenkey by panta rhei everything flows
 
# https://cran.r-project.org/web/packages/PantaRhei/vignettes/panta-rhei.html
# Sankey diagrams visualize the flow of conservative substances through a system. ‘PantaRhei’
# ‘PantaRhei’ simple syntax using data in tables, spread sheets produce publication-quality diagrams.
# you’ll need three different data frames information on nodes, flows, colors
# nodes coordinate data frame:
#   ID (character) to identify the node
#   x (numeric) the x-coordinate of the node (in arbitrarly units)
#   y (numeric) the y-coordinate of the node.
#   label, label_pos etc
# flows data frame between the nodes. [better multiple flow types, or substances]
#   from (character)  starting node ID 
#   to (character)    ending node ID
#   quantity (numeric) magnitude of flow.
#   substance,
#  rainbow() but optional colors <- tribble(~substance, ~color, "Cocoa",    "chocolate", "Sugar",    "#FFE4C4" )
# then sankey(nodes, flows),  but better sankey(nodes, flows, colors, legend=TRUE))
nodes <- tibble::tribble(
    ~ID,  ~label,   ~x,   ~y,      ~dir,    ~label_pos,
    "in",    "Input",  0,   "0",     "right", "left",
    "out",   "Output", 4,   "in",    "right", "right",
)
flows <- tibble::tribble(
    ~from,     ~to,   ~quantity, ~substance,
    "in",     "out",   1, "Oil",
    "",       "",      1, "Gas",
    "",       "",      1, "Biomass",
    "",       "",      1, "Electricity",
    "",       "",      1, "Solar",
    "",       "",      1, "Hydrogen",
    "",       "",      1, "Wind",
    "",       "",      1, "Water",
    "",       "",      1, "Nuclear",
)
colors <- tibble::tribble(
    ~substance, ~color,
    "<any>",    "cornflowerblue",
)

# reallife
nodes   <- read_xlsx("my_sankey_data.xlsx", "nodes")
flows   <- read_xlsx("my_sankey_data.xlsx", "flows")
colors  <- read_xlsx("my_sankey_data.xlsx", "colors")
#check_consistency(nodes, flows, colors)
#check_balance(nodes, flows)
sankey(nodes, flows, colors)


# PantaRhei::sankey(nodes, flows,  node_style=ns, 
#                   legend=gpar(filesize=18, col="blue", ncols=2),
#                   page_margin=c(0.1, 0.1, 0.1, 0.2),
#                   title=strformat("Panta Rhei", fontsize=18, col="blue") )
# Hard cop output
pdf("diagram.pdf", width=10, height=7) # Set up PDF device
sankey(nodes, flows, colors)           # plot diagram
dev.off()                              # close PDF device


# final example

library(PantaRhei)
data(MFA) # Material Flow Account data, list of three tables
str(MFA)
MFA[1]

# node style
library(grid) # loads: gpar()

dblue <- "#00008B" # Dark blue

my_title <- "Material Flow Account"
attr(my_title, "gp") <- grid::gpar(fontsize=18, fontface="bold", col=dblue)

# node style ns
ns <- list(type="arrow",gp=gpar(fill=dblue, col="white", lwd=2),
           length=0.7,
           label_gp=gpar(col=dblue, fontsize=8),
           mag_pos="label", mag_fmt="%.0f", 
           mag_gp=gpar(fontsize=10,fontface="bold",col=dblue)
           )

sankey(MFA$nodes, MFA$flows, MFA$palette,
       max_width   = 0.1,   rmin=0.5,
       node_style  = ns,
       page_margin = c(0.15, 0.05, 0.1, 0.1),
       legend=TRUE, title=my_title,  copyright="Statistics Netherlands"
       )

###########################################
# plain plots
library(ggplot2)
theme_set(   theme_classic()   +   theme(legend.position = "top")   )
# demo data
set.seed(1234)
wdata = data.frame(   sex = factor(   rep(c("F", "M"),  each=200)   ),
                   weight = c( rnorm(200, 55),  rnorm(200, 58)   )    )

# line color by sex , change fill and outline color manually 
wdata  %>% ggplot( aes(x = weight)) +
           geom_histogram(  aes(color = sex, fill = sex), 
                            position = "identity", 
                            bins = 30, 
                            alpha = 0.4
                                            )  +
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
Datanovia
Login|Register0
HOME
LEARN
TOPICS
PRICING
SHOP
RESOURCES
ABOUT
CONTACT
English
HOW TO CREATE A GGPLOT-LIKE 3D SCATTER PLOT USING PLOTLY
HOMEGGPLOT2HOW TO CREATE A GGPLOT-LIKE 3D SCATTER PLOT USING PLOTLY
Search






12 Jan
How to Create a GGPlot-like 3D Scatter Plot using Plotly
Alboukadel |  ggplot2 FAQ |  ggplot2 |  0
In this article you will learn how to create a ggplot-like 3D scatter plot using the plotly R package.





Contents:
  
  Prerequisites
Basic 3D Scatter Plot
3D Scatter Plot with Color Scaling
Related Book
GGPlot2 Essentials for Great Data Visualization in R
Prerequisites
Load required R packages

library(tidyverse)
library(plotly)
Data preparation:
  
  df <- mtcars %>%
  rownames_to_column() %>%
  as_data_frame() %>%
  mutate(am = ifelse(am == 0, "Automatic", "Manual")) %>%
  mutate(am = as.factor(am))
df

# Basic 3D Scatter Plot

df %>% plot_ly(  x = ~wt, y = ~hp, z = ~qsec, color = ~am, colors = c('#BF382A', '#0C4B8E')    ) %>%
      add_markers() %>%
      layout(    scene = list(xaxis = list(title = 'Weight'),
                 yaxis = list(title = 'Gross horsepower'),
                 zaxis = list(title = '1/4 mile time')    )    
            )  -> p

p
# HOME
LEARN
TOPICS
PRICING
SHOP
RESOURCES
ABOUT
CONTACT
English
HOW TO CREATE A GGPLOT-LIKE 3D SCATTER PLOT USING PLOTLY
HOMEGGPLOT2HOW TO CREATE A GGPLOT-LIKE 3D SCATTER PLOT USING PLOTLY
Search






12 Jan
How to Create a GGPlot-like 3D Scatter Plot using Plotly
Alboukadel |  ggplot2 FAQ |  ggplot2 |  0
In this article you will learn how to create a ggplot-like 3D scatter plot using the plotly R package.





Contents:
  
  Prerequisites
Basic 3D Scatter Plot
3D Scatter Plot with Color Scaling
Related Book
GGPlot2 Essentials for Great Data Visualization in R
Prerequisites
Load required R packages

library(tidyverse)
library(plotly)
Data preparation:
  
  df <- mtcars %>%
  rownames_to_column() %>%
  as_data_frame() %>%
  mutate(am = ifelse(am == 0, "Automatic", "Manual")) %>%
  mutate(am = as.factor(am))
df
## # A tibble: 32 x 12
##   rowname   mpg   cyl  disp    hp  drat    wt  qsec    vs am    gear  carb
##   <chr>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <fc> <dbl> <dbl>
## 1 Mazda …  21       6   160   110  3.9   2.62  16.5     0 Man…     4     4
## 2 Mazda …  21       6   160   110  3.9   2.88  17.0     0 Man…     4     4
## 3 Datsun…  22.8     4   108    93  3.85  2.32  18.6     1 Man…     4     1
## 4 Hornet…  21.4     6   258   110  3.08  3.22  19.4     1 Aut…     3     1
## 5 Hornet…  18.7     8   360   175  3.15  3.44  17.0     0 Aut…     3     2
## 6 Valiant  18.1     6   225   105  2.76  3.46  20.2     1 Aut…     3     1
## # ... with 26 more rows
Basic 3D Scatter Plot
# Create the plot
p <- plot_ly(
  df, x = ~wt, y = ~hp, z = ~qsec, 
  color = ~am, colors = c('#BF382A', '#0C4B8E')
) %>%
  add_markers() %>%
  layout(
    scene = list(xaxis = list(title = 'Weight'),
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
theme_icon <- function () {  theme_void() + 
                            theme( panel.background = element_rect(fill = "transparent", colour = NA), 
                                   plot.background = element_rect(fill = "transparent", colour = NA), 
                                   legend.background = element_rect(fill = "transparent", colour = NA), 
                                   legend.box.background = element_rect(fill = "transparent", colour = NA) )
                            }

library(ggplot2)
# Create an icon form a ggplot graphic
p <- ggplot(iris, aes(Species, Sepal.Length)) + 
  geom_boxplot(color = "#478bca", fill = "transparent") +
  theme_icon()
# Save the icon into a 72x72 pixel png or svg format:
# SVG
?ggsave(   filename = "figures/boxplot-icon_72px.svg", p, 
            dpi=72, width = 1, height = 1   )
# PNG
?ggsave(  filename = "figures/boxplot-icon_72px.png", p, 
          dpi=72, width = 1, height = 1, bg = "transparent" )


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
# Créer une donnée de démonstration
set.seed(20190708)
genes <- paste("gene", 1:1000, sep="")
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

# Utilisation du package R ggVennDiagram
if (!require(devtools)) install.packages("devtools")
devtools::install_github("gaospecial/ggVennDiagram")

library("ggVennDiagram")
ggVennDiagram(x, label_alpha = 0.2)


install.packages("VennDiagram")

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



if (condition) {   code executed when condition is TRUE } else { code executed when condition is FALSE }


for(i in 1:x) { code }


while (test_expression) {  statement }
      

repeat {  statement }

rep("A",4)
rep(1:5,2)
rep(1:5,rep(2,5))

#break and next statements

if (test_expression) {
           break
           }


switch(expression, case1, case2, case3....)

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

