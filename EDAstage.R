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

