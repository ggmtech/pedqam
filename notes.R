# biber  external programs to process bibliography info from .bib file for LaTeX document.
# biblatex LaTeX packages  format citations and bibliographies;  both bibtex and biber.

# before /begin{document}
# # \usepackage[<language options>]{babel}% Recommended
# \usepackage{csquotes}% Recommended
# \usepackage[style=<somebiblatexstyle>,<other options>]{biblatex}
# \addbibresource[<options for bib resources>]{<mybibfile>.bib}% Syntax for version >= 1.2
# Note that I used \autocite
# \printbibliography
# \addbibresource also allows to load remote resources and other data types (e.g., ris)
# biblatex-apa , biblatex-chicago , biblatex-mla
# biblatex-ieee ,biblatex-chem,  biblatex-jura ,
# biblatex-nature , biblatex-science
# To cite all
# nocite: |
#   @*    # or   @item1, @item2 @* is a wildcard for all references.

# or with netbib
# ---
#   title: "Stack Overflow Answer"
# author: "duckmayr"
# date: "1/2/2021"
# output:
#   pdf_document:
#   citation_package: natbib   #  with netbib
# bibliography: refs.bib
# ---
#
# \nocite{*}

# To create a bibliography, add a # References header at the end of your document.




library(tidyverse)
base <- tibble::tibble(a = 1:10, b = 1:10, c = 21:30) %>% head()
base
# case_when()
base %>% mutate(
    new = case_when(a == 1 ~ "a equals 1",
                    c == 25 ~ "c equals 25",
                    TRUE ~ "other case"   )
               )

# An example of a vectorized function that repeats the operations of the previous case_when():
vectorised_function <- function(a, b, c, ...){ ifelse(a == 1 , "a equals 1",
                                               ifelse(c == 25 , "c equals 25",
                                                "other case"  ))    }

# Here is the “same” function, but not vectorized:
non_vectorised_function <- function(a, b, c, ...){ if ( a == 1  ) { return("a equals 1") }
                                                   if ( c == 25 ) { return("c equals 25") }
                                                  return("autre")  }
non_vectorised_function(a = 1, c = 25, b = "R")
## [1] "a equals 1"
non_vectorised_function(a = c(1, 1, 3), c = 27:25, b = "R") # ne fonctionne pas

vectorised_function(a = 1, c = 25, b = "R")
## [1] "a equals 1"
vectorised_function(a = c(1, 1, 3), c = 27:25, b = "R")
## [1] "a equals 1"  "a equals 1"  "c equals 25"

base %>%
  mutate(
    new = vectorised_function(a = a, b = b, c = c)
  )

#With rowwise() rowwise() is back in the {dplyr} world and is specifically designed for this case:
base %>% rowwise() %>%  mutate( new = non_vectorised_function(a = a, b = b, c = c) )

base %>% mutate(  new = Vectorize(non_vectorised_function)(a = a, b = b, c = c) )



rmarkdown::render(output_format = "file_header")


library(datasets)
data  <-  iris$Sepal.Length

mean(data)
median(data)
sd(data)
var(data)
max(data)
min(data)
quantile(data)
summary(data)

# if statements

if (condition) {
  # code executed when condition is TRUE
} else {
  # code executed when condition is FALSE
}

# for statements

for(i in 1:x)
{ code }

# while statements

while (test_expression)
{
  statement
}

# repeat statements
repeat {
  statement
}

# break and next statements
if (test_expression) {
  break
}

# switch statements

switch(expression,
       case1,
       case2, case3....)

# scan statements

scan(file = "")

# UDF
Square<-function(x){x*x}
Square(10:20)  # 100 121 144 169 196 225 256 289 324 361 400

rep("A",4)
rep(1:5,2)
rep(1:5,rep(2,5)) #??

# Sorting
order(data)
rev(order(data))


#  combine the data frame, matrix or vectors, you can use cbind functions.
a<-c(1,2,3)
b<-c(4,5,6)
rbind(a,b)
cbind(a,b)

# Some useful functions are here



identical()
sum()
paste()
na.omit()
is.na()
is.logical()
stopifnot()
length()


cbind(a,b)








---
title: "Test"
author: "Ben"
date: "`r format(Sys.time(), '%d/%m/%Y')`"
output:
  word_document:
  toc: yes
#reference_docx: "../templates/word-styles-reference-01.docx"
---

```{r setup, include = FALSE }
knitr::opts_chunk$set(echo = TRUE)
```

###### Page break after table of contents

# Header 1

## Header 2

## Header 2

### Header 3






p + scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")
p + theme(axis.text.x=element_text(angle=60, hjust=1))

library(scales)
df$Month <- as.Date(df$Month)
ggplot(df, aes(x = Month, y = AvgVisits)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Month", y = "Average Visits per User") +
  scale_x_date(labels = date_format("%m-%Y"))

ggtitle(label) # for the main title
xlab(label) # for the x axis label
ylab(label) # for the y axis label
labs(...) # for the main title, axis labels and legend titles




library(ggplot2)
p <- ggplot(ToothGrowth, aes(x=dose, y=len)) + geom_boxplot()
p
# Hide the main title and axis titles
p + theme(
  plot.title = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank())

# Default plot
p <- ggplot(ToothGrowth, aes(x=dose, y=len)) + geom_boxplot() +
  ggtitle("Plot of length \n by dose") +
  xlab("Dose (mg)") + ylab("Teeth length")
p


p + ggtitle("Plot of length \n by dose") +  xlab("Dose (mg)") + ylab("Teeth length")
# or
p +labs(title="Plot of length \n by dose",   x ="Dose (mg)",     y = "Teeth length")

# ggplot2 title, axis labels, legend titles, R programming
# Change plot titles using the function labs() as follow :
# main title
p + theme(plot.title = element_text(family, face, colour, size))
# x axis title
p + theme(axis.title.x = element_text(family, face, colour, size))
# y axis title
p + theme(axis.title.y = element_text(family, face, colour, size))
 # Change the color, the size and the face of
# the main title, x and y axis labels
p + theme(
  plot.title = element_text(color="red", size=14, face="bold.italic"),
  axis.title.x = element_text(color="blue", size=14, face="bold"),
  axis.title.y = element_text(color="#993333", size=14, face="bold")
   )

# element_text(
# family : font family
# face : font face. Possible values are “plain”, “italic”, “bold” and “bold.italic”
# colour : text color
# size : text size in pts
# hjust : horizontal justification (in [0, 1])
# vjust : vertical justification (in [0, 1])
# lineheight : line height. In multi-line text, the lineheight argument is used to change the spacing between lines.
# color : an alias for colour
# )











# webshot2
remotes::install_github("rstudio/webshot2") # Chromium based eg Chrome, Edge, Opera, Vivaldi, Brave,
library(webshot2)
webshot2::webshot("https://www.r-project.org")   # Single page
webshot2::webshot(c("https://www.r-project.org", "https://www.rstudio.com")) # Multiple pages (in parallel!)

# Specific height and width
webshot2::webshot("https://www.r-project.org", vwidth = 1600, vheight = 900, cliprect = "viewport")


# install.packages("remotes")
remotes::install_github("gadenbuie/xaringanExtra")

library(tidymodels)
library(modeltime)
library(tidyverse)
library(timetk)

timetk::walmart_sales_weekly
data_tbl <- timetk::walmart_sales_weekly %>%
            select(id, Date, Weekly_Sales) %>%
            set_names(c("id", "date", "value"))
data_tbl


data_tbl %>% group_by(id) %>%
        timetk::plot_time_series(  date,
                                   value,
                                   .interactive = F, #T for plotly
                                   .facet_ncol = 3
                              )

# Nested Data Structure: Most critical to ensure your data is prepared (covered next)
# Nested Modeltime Workflow: create many models, fit models to data, and generate forecasts at scale
# conceptually combine nested data and tidymodels workflows using  modeltime_nested_fit().

# Extending each of the times series: How far into the future    extend_timeseries().
# Nesting by the grouping variable: nested structure. identify the ID column that separates each time series, and the number of timestamps to include in the “.future_data” and optionally “.actual_data”. Typically,  select the same .length_future as your extension from previous step. See nest_timeseries().

# take your .actual_data and Train/Test Set Splitting:into train/test splits for accuracy and CI estimation. See split_nested_timeseries().

nested_data_tbl <- data_tbl %>%
  # 1. Extending: We'll predict 52 weeks into the future.
  timetk::extend_timeseries(
                    .id_var        = id,
                    .date_var      = date,
                    .length_future = 52       ) %>%
  # 2. Nesting: We'll group by id, and create a future dataset
  #    that forecasts 52 weeks of extended data and
  #    an actual dataset that contains 104 weeks (2-years of data)
  nest_timeseries(
                    .id_var        = id,
                    .length_future = 52,
                    .length_actual = 52*2
  ) %>%

  # 3. Splitting: We'll take the actual data and create splits
  #    for accuracy and confidence interval estimation of 52 weeks (test)
  #    and the rest is training data
  ????::split_nested_timeseries(.length_test = 52 )

nested_data_tbl

rec_prophet <- recipe(value ~ date, training(nested_data_tbl$.splits[[1]]))

wflw_prophet <- workflow() %>%
                add_model( prophet_reg("regression", seasonality_yearly = TRUE) %>%
                set_engine("prophet")
          ) %>%
  add_recipe(rec_prophet)






##############
##############
timetk::flights
flights %>% count(flight_path   =   str_c(origin,  " -> ",  dest), sort = TRUE)
flights %>% slice_sample(n = 15)                     # or slice_sample(prop = 0.15)

number = parse_number(number)

flights %>%  mutate(
  origin = case_when(
    (origin == "EWR") & dep_delay > 20   ~ "Newark International Airport - DELAYED",
    (origin == "EWR") & dep_delay <= 20  ~ "Newark International Airport - ON TIME DEPARTURE",
                    )
                    ) %>%   count(origin)

mutate(origin = str_replace_all(
 origin, c( "^EWR$" = "Newark International", "^JFK$" = "John F. Kennedy International" ) ) ) %>% count(origin)

# extract the row information’s based on str_detect function
beginning_with_am <-  airlines %>%    filter( name %>% str_detect("^Am") )

crossing()





#notes


library(anytime)
library(stringr)
str1 <- "My Friend is coming on july 10 2018 or 10/07/2018"
anydate( stringr::str_extract_all( str1, "[[:alnum:]]+[ /]*\\d{2}[ /]*\\d{4}")[[1]] ) #[1] "2018-07-10" "2018-10-07"

str1 %>% stringr::str_extract_all( "[[:alnum:]]+[ /]*\\d{2}[ /]*\\d{4}" )  %>%  unlist()   %>% anydate()

# md
# four spaces (or one tab) is treated as verbatim text
# fenced code blocks > 3 tildes (~) and end same tildes # Use more if code itself contains tildes or backticks.
# attach attributes to code blocks using syntax: ~~~~ {#mycode .haskell .numberLines startFrom="100"}

# A line block is a sequence of lines beginning with a vertical bar (|) followed by a space

# Pandoc supports

#  task lists, using the syntax of GitHub-Flavored Markdown.
# - [ ] an unchecked task list item
# - [x] checked item

# definition lists, using the syntax of PHP Markdown Extra with some extensions.2
#
# Term 1
#
# :   Definition 1
#
# Term 1
# ~ Definition 1
#
# Term 2
# ~ Definition 2a
# ~ Definition 2b

# each new list using @ will take up where the last stopped. So, for example:
#
#     (@)  My first example will be numbered (1).
#     (@)  My second example will be numbered (2).
#
# Explanation of examples.
#
#    (@)  My third example will be numbered (3).
#
#
#     (@good) Numbered examples can be labeled and referred to elsewhere in the document:
#
# As (@good) illustrates, ...

# To “cut off” the list after item two, you can insert some non-indented content, like an HTML comment,

# Horizontal rules : a row of three or more *, -, or _ characters
# Tables Four kinds of tables

# ---
# title:  'This is the title: it contains a colon'
# author:
#     - Author One
#     - Author Two
# keywords: [nothing, nothingness]
# abstract: |
#     This is the abstract.
#
# It consists of two paragraphs.
# ...


# Only the following characters to be backslash-escaped:  \`*_{}[]()>#+-.!



remove.packages(bookdown)


library(ggplot2)
ggplot(mtcars, aes(sample=mpg))+stat_qq()+theme_bw()
# The result you got from Q-Q plot you can verify the same based on shapiro test.
shapiro.test(mtcars$mpg)
# car::qqPlot() provides the best option for routine visualization.

# generate data for this post
set.seed(20200825)
x <- sort(rnorm(20, 10, 3))
q <- (rank(x) - .5) / length(x)


band <- tibble(  z       = seq(-2.2, 2.2, length.out = 300),  n = length(d$x_sample),
               sample_sd = sd(d$x_sample),
               se = sample_sd * se_z(z, n),
               line = mean(d$x_sample) + sample_sd * z,
               upper = line + 2 * se,
               lower = line - 2 * se,

               robust_sd = IQR(d$x_sample) / 1.349,
               robust_line = median(d$x_sample) + z * robust_sd,
               robust_se =  robust_sd * se_z(z, n),
               robust_upper = robust_line + 2 * robust_se,
               robust_lower = robust_line - 2 * robust_se,
             )

ggplot(d) +  geom_point(aes(x = z_theoretical, y = x_sample)) +
            geom_abline( aes(intercept = mean, slope = sd, color = "Naive"),
                         data = tibble(sd = sd(d$x_sample), mean = mean(d$x_sample))  ) +
             geom_ribbon(  aes(x = z, ymax = upper, ymin = lower, color = "Naive"),
                          data = band,
                          fill = NA, show.legend = FALSE  ) +
             labs(  color = "Q-Q line", x = "theoretical quantiles",   y = "sample quantiles"  ) +
             guides(color = guide_legend(nrow = 1)) +
              theme(legend.position = "top", legend.justification =  "left")

# using CAR

par(mar = c(4, 2, 1, 2))# Set margins on Q-Q plots
library(patchwork)# Use patchwork to capture the plots and combine them
p1 <- wrap_elements(~ car::qqPlot(x))
p1
# p2 <- wrap_elements(~ {
#     car::qqPlot(x)
#     lines(band$z, band$robust_line, col = "black", lwd = 2)
#     lines(band$z, band$robust_upper, col = "black", lwd = 2)
#     lines(band$z, band$robust_lower, col = "black", lwd = 2)
# })
# p1 + p2   # into a side by side display.


# alternative to Q-Q plot: the worm plot “detrended” Q-Q plots

par(mar = c(4.5, 4.5, 1, 2))
# I don't know what's up with that error message.
# use scale() to transform in to z-score
gamlss::wp(
    resid = scale(d$x_sample),
    xlim.all = 2.5,
    line = FALSE )


###### cleaning janitor
library(sparkline)
library(janitor)
library(dplyr)
data<-read.csv("D:/RStudio/Website/FinData.csv",1)
clean <- janitor::clean_names(data)
clean_x<-clean %>% remove_empty(whic=c("rows"))  # "cols"
clean %>% get_dupes(first_name,certification)

tabyl(clean,employee_status) # easy tabulation
clean %>% tabyl(employee_status) %>% adorn_pct_formatting(digits =2,affix_sign=TRUE)
clean %>% tabyl(employee_status, full_time) %>% adorn_totals("row") %>% adorn_percentages("row") %>%
               adorn_pct_formatting() %>% adorn_ns("front")
# Remove empty column or rows
clean_x<-clean %>% remove_empty(whic=c("rows"))
clean_x<-clean %>% remove_empty(whic=c("cols"))
# duplicates
clean %>% get_dupes(first_name)
clean %>% get_dupes(first_name,certification)

excel_numeric_to_date(41103)




ggpubr::show_line_types()  # library(ggpubr)


library(tidyverse)

library(ggforce)
library(ggfx)
ggplot() +
    as_reference( geom_text(aes(x = 0, y = 0, label = 'RDSO'), size = 50, family = 'Fontania'),
                 id = 'text_layer') +
    with_blend( geom_circle(aes(x0 = 0, y0 = 0, r = seq_len(5) ), fill = NA, size = 8),
        bg_layer = 'text_layer',  blend_type = 'xor' ,
        id = 'blended') +   # filters can be turned ref by assigning id
    with_inner_glow( 'blended',  colour = 'red', sigma = 5) +
    coord_fixed()

# even rester image
ggfx_logo <- as.raster(magick::image_read( system.file('help', 'figures', 'logo.png', package = 'ggfx') ))
magick::image_read( "/Users/gk/Downloads/newyear2021.png")
ggfx_logo <- as.raster(magick::image_read( "/Users/gk/Downloads/newyear2021.png" ) )

ggplot(mpg) +   with_blend(  bg_layer = ras_fit(ggfx_logo, 'viewport'),
                             geom_point(aes(x = hwy, y = displ), size = 5),
                             blend_type = 'xor' )



# library(ggthemes) #install_github('cttobin/ggthemr') #library(themr) # ggthemr("<name>") #ggthemr_reset()
# library(plotly)   # plot_grid(gp1, gp2, NULL, gp1, labels = "AUTO")
library(googlesheets4) # gs_auth(new_user = TRUE) #
library("survival") ; library("survminer") ;
library(ggfortify); library(DataExplorer)

library(scales)   # to access breaks/formatting functions
library(gridExtra) # for arranging plots

library(timetk)
# summarise_by_time(), mutate_by_time() ,filter_by_time() , filter_period() , between_time() , pad_by_time()
# condense_period() , slidify() - Turn any function into a sliding (rolling) function

# dir to ensure R find file to import # setwd("working-dir-path-here")
library(magick)
raw_img <- magick::image_read("http://www.showbuzzdaily.com/wp-content/uploads/2021/02/Final-Cable-2021-Feb-03-WED.png")
magick::image_ggplot(raw_img)
chopped_image <- raw_img %>% image_crop(geometry_area(703, 1009, 0, 91)) ##crop width:703px ,height:1009px starting +91px from top
magick::image_ggplot(chopped_image)
processed_image <- chopped_image %>% image_negate() %>%   #  # Remove the Horizontal Lines
    image_morphology(method = "Thinning", kernel = "Rectangle:7x1") %>%
    image_negate() %>% image_quantize(colorspace = "gray") #   # Turn colors to greyscale
tesseract::ocr(processed_image) %>% str_sub(end = str_locate(., '\\n')[1])


image_ggplot(processed_image)



#########
library(summarytools) #devtools::install_github('dcomtois/summarytools', ref='0-8-9') #old version
summarytools::view(dfSummary(iris))
set.seed(2835)
Random_numbers <- sample(c(5e3, 5e4, 5e5), size = 1e4, replace = TRUE, prob = c(.12, .36, .52))
freq(Random_numbers)
print(freq(Random_numbers), big.mark = " ", decimal.mark = ".")
freq(iris$Species)
freq(tobacco$disease, order = "freq", rows = 1:5, headings = FALSE)
print(ctable(x = tobacco$smoker, y = tobacco$diseased, prop = "r"), method = "render")
tobacco  %$%  ctable(smoker, diseased, chisq = TRUE, OR = TRUE, RR = TRUE, headings = FALSE) %>% print(method = "render")
summarytools::descr(iris)
descr(iris, stats = c("mean", "sd"), transpose = TRUE, headings = FALSE)
summarytools::view(dfSummary(iris))
# in Rmd
summarytools::dfSummary(tobacco, plain.ascii = FALSE, style = "grid",  graph.magnif = 0.75, valid.col = FALSE, tmp.img.dir = "/tmp")



fill(data, ...,  .direction = c("down", "up", "downup", "updown"))  # it is .direction =
squirrels %>%
    dplyr::group_by(group) %>%
    fill(  n_squirrels,   .direction = "downup" ) %>%
    dplyr::ungroup()

separate_rows(data, ..., sep = "[^[:alnum:].]+", convert = FALSE)

#########

# Not evaluated
library(sparkline)
sparkline(0)

spk_dt <- data.frame( var = c("mpg", "wt"),
                     sparkline = c(spk_chr(mtcars$mpg), spk_chr(mtcars$wt))  )

kbl(spk_dt, escape = F) %>%    kable_paper(full_width = F)


