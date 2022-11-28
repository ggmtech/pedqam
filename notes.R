
#install.packages("exams", dependencies = TRUE)
library("exams")
exams_skeleton(markup = "markdown", encoding = "UTF-8",
               writer = c("exams2html", "exams2pdf", "exams2moodle"))
# This copies all R/Markdown files to the pwd along with demo script demo ofexams2html() and exams2pdf() for customizable HTML and PDF output, respectively, along with Moodle output via exams2moodle().
dir()
## [1] "demo-all.R"    "demo-html.R"   "demo-moodle.R" "demo-pdf.R"
## [5] "exercises"     "templates"
# Simply open the demo script demo-all.R for the first steps and then continue with more details in demo-html.R, demo-pdf.R, or demo-moodle.R, respectively. More information about all the exercises can be found in the exercise template gallery online.


#exams2html("swisscapital.Rmd")
exams2pdf("swisscapital.Rmd")

getwd()
dir()



getwd()
setwd("/Users/gk/gkgit")

usethis::create_project("~/gkgit/myppt")

###################
###################
###################

#install.packages("promotionImpact") #devtools::install_github("ncsoft/promotionImpact")
library(promotionImpact)
promotionImpact::sim.data  # daily simulated sales data
promotionImpact::sim.promotion # simulated promotion schedule data

# add the dummy variable of the first day of each month as shown below. as maney
sim.data <- promotionImpact::sim.data %>%
  dplyr::mutate(month_start = ifelse(substr(as.character(dt),9,10) == '01', 1, 0))

#Now, create the model as shown below.
pri1 <- promotionImpact::promotionImpact(data=promotionImpact::sim.data,
                                         promotion=promotionImpact::sim.promotion,
                        time.field = 'dt', target.field = 'simulated_sales',
                        dummy.field = 'month_start',
                        trend = T, period = 30.5, trend.param = 0.02, period.param = 2,
                        logged = TRUE, differencing = TRUE, synergy.promotion = FALSE,
                        synergy.var = NULL, allow.missing = TRUE)


###################
###################
library(tabxplor)
set_color_style(type = "text", theme = "light")
tab(forcats::gss_cat, marital, race)

gss  <- "Source: General social survey 2000-2014"

tab(forcats::gss_cat, marital, race, pct = "row", na = "drop", subtext = gss,
    rare_to_other = TRUE, n_min = 1000, other_level = "Custom_other_level_name")




# With several tab_vars, it makes a subtable for each combination of their levels.
# By default, with color = "diff", colors are based on the differences between a cell and it’s related total (
data <- forcats::gss_cat %>%
  dplyr::filter(year %in% c(2000, 2006, 2012),
                !marital %in% c("No answer", "Widowed") )

gss2 <- "Source: General social survey 2000, 2006 and 2012"

tab(data, race, marital, year, subtext = gss2, pct = "row", color = "diff")

tab(dplyr::storms, category, status, sup_cols = c("pressure", "wind")) # mean as supplementary column


tab(data, race, marital, year, subtext = gss2, pct = "row", color = "diff", comp = "all")# comp all
# diff = "first", each row (or column) is compared to the first row (or column), eg for last FY
data <- data %>% dplyr::mutate(year = as.factor(year))
tab(data, year, marital, race, pct = "row", color = "diff", diff = "first", tot = "col",
    totaltab = "table")

tab(forcats::gss_cat, race, marital, pct = "row", ci = "cell") # conf interval

tab(forcats::gss_cat, race, marital, pct = "row", color = "diff_ci")


###################
###################
library(tidyverse)
List <- list(X= 10:15, Y= 1:5, Z= LETTERS[1:5])
lapply(seq_along(List),
       function(i) paste("The List Number Starting From",
                         i,
                         "with",
                         names(my_list)[[i]],
                         "contains the values",
                         paste(my_list[[i]], collapse = ", "))
       )



###################
library(tidyverse)
#devtools::install_github("Harrison4192/presenter")
library(presenter) # wrapper functions using packages openxlsx, flextable, and officer to create highly formatted MS office friendly output of your data frames.
# make_simple_excel and make_powerpoint can accept single tables or lists of tables, which will be given separate sheets
#
make_excel(df = iris, header_word = c("Sepal", "Petal"),
last_id_col = NULL)
set.seed(1)

# make an excel workbook with multiple sheets
# Use the automated formatting paradigm and customize the id cols and header words for each sheet.

make_excel_wb(wb = NULL,
              object = iris,
              last_id_col = NULL,
              header_word = c("Sepal", "Petal")) %>%
  make_excel_wb(object = anscombe,
                last_id_col = NULL,
                header_word = NULL) %>%
  finish_excel_wb(wb_name = "data_workbook")


iris %>%  make_simple_excel() # simple excel


iris %>%
  relocate("Species") %>%
  sample_n(10) %>%
  arrange(Species) -> iris_slice

header_words <- c("Sepal", "Petal")
last_id_col <- "Species"

make_flextable(iris_slice, header_words = header_words, last_id_col = last_id_col) -> myflex

myflex


## Create a pivot table with flextable
tibble::tibble(my_letters = sample(letters[1:4], 100, T),
               my_numbers = sample(1:4, 100, T)) -> cross_table


#  theme = zebra_blue (default),vanilla, booktabs, alafoli, zebra_gold,     "tron" , vader,
cross_table %>%
  make_pivot_table(my_letters, my_numbers, theme = "zebra_gold") -> tron_cross_table

tron_cross_table

# good auto formated cross tab
iris %>%
  dplyr::mutate(Species1 = stringr::str_c(Species, " very good")) %>%
  make_pivot_table(Species1, Species, show_percentages = "none", tbl_nm = "gold table", theme = "zebra_gold") -> tbl

tbl

# export tables to powerpoint export_powerpoint() ? but  use make_powerpoint()
# make_powerpoint
myflex %>%  make_powerpoint() # a new ppt is created, named after the table

tron_cross_table %>%
  make_powerpoint("myflex.pptx") # append this slide to the previous ppt

# long format
sumr0 %>%   pivot_summary()
sumr1 %>%   pivot_summary(Species)
# pivot wide format
iris %>%  summarize(across(where(is.numeric), mean), .groups = "drop") -> sumr0
sumr0

# wide format

iris %>%  group_by(Species) %>%
  summarize(across(where(is.numeric), mean), .groups = "drop") -> sumr1
sumr1
sumr1 %>%  pivot_summary(Species)


###
library(presenter)
library(dplyr)
library(ggplot2)
library(stringr)
# Custom formatting
# create a header word using dplyr::rename_with
# format percentages with presenter::format_percent
diamonds %>%
  select(-clarity) %>%
  mutate(relative_price = price / max(price), .before = "x") %>%
  group_by(across(where(is.ordered))) %>%
  summarize(across(where(is.double), mean), .groups = "drop") %>%
  format_percent(relative_price) %>%
  rename_with(~str_c("category_", .), cut:color) %>%
  rename_with(~str_c("property_", .), carat:table) %>%
  rename_with(~str_c("dimension_", .), x:z) -> diamonds_summary

diamonds_summary %>% head

diamonds_summary %>%  make_flextable(last_id_col = 2,
                 # header_words = c("category", "property", "dimension"),
                 theme = "zebra_gold")

# Automatic coloring
tibble(x = letters[1:10], y = -5:4, z = -c(-.5, -.2, -.1, 0, .1, .2, .3, .4, .6, .9)) %>%
  format_percent(z) %>%
  rename_with(~str_c("sample_", .)) %>%
  make_flextable()

####################
####################
####################
#sweep package extends the broom tools (tidy, glance, and augment) for performing forecasts and time series analysis in the “tidyverse”.
#sweep forecastion
library(tidyverse)
library(tidyquant)
library(timetk)
library(sweep)
library(forecast)
# Forecasting Sales of Beer, Wine, and Distilled Alcohol Beverages
# tidyquant package to get the US alcohol sales from the FRED data base ( one of the 80+ data sources FRED connects to). The FRED code is “S4248SM144NCEN” and the data set  found here.
alcohol_sales_tbl <- tq_get("S4248SM144NCEN",
                            get  = "economic.data",
                            from = "2007-01-01",
                            to   = "2016-12-31")
alcohol_sales_tbl
# quickly visualise
alcohol_sales_tbl %>%
  ggplot(aes(x = date, y = price)) +
  geom_line(size = 1, color = palette_light()[[1]]) +
  geom_smooth(method = "loess") +
  labs(title = "US Alcohol Sales: Monthly", x = "", y = "Millions") +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_tq()

# Forecasting Workflow few basic steps:
# Step 1: Coerce to a ts object class.
# Step 2: Apply a model (or set of models)
# Step 3: Forecast the models (similar to predict)
# Step 4: Use sw_sweep() to tidy the forecast.
# Step 1  forecast package uses the ts data structure with tk_ts() from the timetk package.
# # The start and freq variables are required for the regularized time series (ts) class
alcohol_sales_ts <- tk_ts(alcohol_sales_tbl, start = 2007, freq = 12, silent = TRUE)
alcohol_sales_ts

has_timetk_idx(alcohol_sales_ts) # maintains a “timetk index”, help with forecasting dates

fit_ets <- alcohol_sales_ts %>%  ets()

# sw_tidy(): Returns a tibble of model parameters
# sw_glance(): Returns the model accuracy measurements
# sw_augment(): Returns the fitted and residuals of the model
# sw_tidy_decomp(): Returns a tidy decomposition from a model

augment_fit_ets <- sw_augment(fit_ets)
augment_fit_ets

augment_fit_ets %>%
  ggplot(aes(x = index, y = .resid)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  geom_smooth(method = "loess") +
  scale_x_yearmon(n = 10) +
  labs(title = "US Alcohol Sales: ETS Residuals", x = "") +
  theme_tq()












##################
# clean data
install.packages("dataPreparation")
#install_github("ELToulemonde/dataPreparation")
library(dataPreparation)
data(messy_adult)
head(messy_adult)

clean_adult <- prepare_set(messy_adult)
head(clean_adult)

###########
library(visdat)
glimpse(typical_data)
# visdat provides tools to create heatmap-like visualisations of an entire dataframe
vis_dat(typical_data)
vis_miss(typical_data) #  custom plot for missing data.

vis_dat(airquality)
vis_dat(typical_data)
vis_dat(typical_data,  sort_type = FALSE)
vis_miss(airquality)
vis_miss(airquality, sort_miss = TRUE) # sort col with most missingness
vis_miss(airquality, cluster = TRUE)
#  vis_compare() displays the differences in two dataframes of the same size.

chickwts_diff <- chickwts
chickwts_diff[sample(1:nrow(chickwts), 30),sample(1:ncol(chickwts), 2)] <- NA

vis_compare(chickwts_diff, chickwts)

###################
# interactionR is implemented in the R statistical software environment;

###################
# trackdown # offers different functions to manage this workflow:
library(trackdown)
upload_file() # uploads a file for the first time to Google Drive.
update_file() # updates content of an existing file in Google Drive with the contents of a local file.
download_file() # downloads the edited version of a file from Google Drive and updates the local version.
render_file() # downloads a file from Google Drive and renders it locally.

#  trackdown-package help page or vignette("trackdown-features") for a detailed description
#   vignette("trackdown-tech-notes") for technical details.




###################



##################
# JuliaConnectoR: A Functionally Oriented Interface for Integrating 'Julia' with R
# Allows to import functions and whole packages from 'Julia' in R



##################
##################

library(tmap) xx
tm_shape(coord_eu) + tm_borders()
it <- IT(data = popIT, unit = "provincia", year = "2018",colID = "ID")

tm_shape(it) + tm_borders() + tm_fill("totale")

####################
# scatterPlotMatrix - interactive scatter plot matrix,in R and js and based on d3.js.


library(scatterPlotMatrix)
# Basic usage (dataset uses factor type)
scatterPlotMatrix(iris)
scatterPlotMatrix(iris, zAxisDim = "Species")

# categoricalCS argument
scatterPlotMatrix(iris, zAxisDim = "Species", categoricalCS = "Set1")
# zAxisDim argument (referenced column is continuous)
scatterPlotMatrix(iris, zAxisDim = "Sepal.Length")
# continuousCS argument
scatterPlotMatrix(iris, zAxisDim = "Sepal.Length", continuousCS = "YlOrRd")
# corrPlotType argument
scatterPlotMatrix(iris, zAxisDim = "Species", corrPlotType = "Text", corrPlotCS = "YlGnBu")
# Basic usage (dataset doesn’t use factor type)
scatterPlotMatrix(mtcars)
#categorical argument
categorical <- list(NULL, c(4, 6, 8), NULL, NULL, NULL, NULL, NULL, c(0, 1), c(0, 1), 3:5, 1:8)
scatterPlotMatrix(mtcars, categorical = categorical, zAxisDim = "cyl")
# regressionType argument
scatterPlotMatrix(iris, zAxisDim = "Species", regressionType = 1)
# distribType argument
scatterPlotMatrix(iris, zAxisDim = "Species", distribType = 1)


#####################
#####################




# augmented Dickey-Fuller (ADF) test in R. The ADF Test is a common statistical test to determine whether a given time series is stationary or not.


# R code can be all in one file or split across many files, better small files, one fn each
# To make this code into an R package, we just need to do two things:
# Create an R subdirectory and move the code into that subdirectory.
# Create a text file called DESCRIPTION containing the following:
# Package: brocolors
# Version: 0.1
# ie,  just give  package a name, and a version number, like 0.1 in a file DESCRIPTION
#  proper R package also documentation,  NAMESPACE file etc
#  Then, to build the package, type devtools::build()
#
#
#  Start by opening a new .R file.
#  Make sure your default directory is clear by typing rm(list = ls())
#  Write the code for your functions in this .R files
#  install.packages('devtools'))   # or try devtools::build github devtools()
#  Rstudio ‘File’ -> ‘New Project.’ -> ‘New Directory,’ ->‘R Package’
#  Type name of new package, then upload the .R files  under
#  ‘Create package based on source files’. -> ‘Create project.’
#  “R documentation” files"  .Rd
#  If the ‘man’ folder already contains .Rd files, open each file, add a title heading-> save # needed to compile your package.
#  Roxygen2 package more useful for automating  generating .Rd (and even NAMESPACE) files.



# JS python HTML CSS  Java  SQL, NoSQL, Rust, pearl,go
#
#  bookdown::render_book("foo.Rmd", bookdown::pdf_book(keep_tex = TRUE))



# fusen package development
# install.packages("fusen", repos = "https://thinkr-open.r-universe.dev")
install.packages("fusen")
# {fusen} is all about correctly separating and naming chunks.
# File > New Project > New directory > Package using {fusen}
# Choose template "teaching" to see how {fusen} works, then use "full"
# .. description
# Run the following code to transform the flat Rmd as an inflated package
# This will open the vignette created and check the package
# fusen::inflate(
#            flat_file = "dev/flat_teaching.Rmd",
#            vignette_name = "Get started",   check = TRUE  )
#
#


# Fix dates
library(datefixR)
fixed.dates <- fix_dates(bad.dates, c("some.dates", "some.more.dates"))
knitr::kable(fixed.dates)

imputed.dates <- fix_dates(impute.dates,  "some.date",
                           day.impute = 1,
                           month.impute = 1)


remotes::install_github("rstudio/rticles")
rticles::journals()

stem(mtcars$cyl)
seq(from = 42, to = 54, by = 2)
bins <- cut(df$est, clases)
barplot(table(bins))
hist(df$est)
median(df$est)
sd(df$est)

#################
#################
library(vroom)
vroom() #which is used to read all types of delimited files.
# If the guessing fails supply it with the delim parameter. (e.g. delim = ",").
file <- vroom_example("mtcars.csv")
file

#################
#################

# {gtsummary} package summarizes data sets, regression models, and more, using sensible defaults with highly customizable capabilities.
# The default output from tbl_summary() is meant to be publication ready.

#install.packages("gtsummary")
#remotes::install_github("ddsjoberg/gtsummary")
library(gtsummary)
trial2 %>% tbl_summary()  # The default output from tbl_summary() is meant to be publication ready.
trial2 %>% tbl_summary(by = trt) %>% add_p()

# Modifying tbl_summary() function arguments
# The tbl_summary() function includes many input options for modifying the appearance.
# label=specify the variable labels printed in table
# type=specify the variable type (e.g. continuous, categorical, etc.)
# statistic=change the summary statistics presented
# digits=number of digits the summary statistics will be rounded to
# missing=whether to display a row with the number of missing observations
# missing_text=text label for the missing number row
# sort=change the sorting of categorical levels by frequency
# percent=print column, row, or cell percentages
# include= list of variables to include in summary table

trial2 <- trial %>% select(age, grade, response, trt) # make dataset with a few variables to summarize
gtsummary::tbl_summary(trial2)  # summarize the data with our package


# customisation
table2 <-  tbl_summary( trial2, by = trt, # split table by group
                           missing = "no" # don't list missing data separately
                       ) %>%
  add_n() %>% # add column with total number of non-missing observations
  add_p() %>% # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels()
table2

mod1 <- glm(response ~ trt + age + grade, trial, family = binomial)

t1 <- tbl_regression(mod1, exponentiate = TRUE)
t1
# Side-by-side Regression Models using tbl_merge()
library(survival)
# build survival model table
t2 <-  coxph(Surv(ttdeath, death) ~ trt + grade + age, trial) %>%
       tbl_regression(exponentiate = TRUE)

# merge tables
tbl_merge_ex1 <-  tbl_merge( tbls = list(t1, t2),
                      tab_spanner = c("**Tumor Response**", "**Time to Death**")
                          )
tbl_merge_ex1




#################
#install.packages("googledrive")
#install.packages("googledrive")
library("googledrive")

# prefix drive_. Unix file system utilities, e.g., find, ls, mv, cp, mkdir, and rm.
drive_find(n_max = 30)
drive_find(pattern = "qam")
drive_find(type = "spreadsheet")     ## Google Sheets!
drive_find(type = "csv")             ## MIME type = "text/csv"
drive_find(type = "application/pdf") ## MIME type = "application/pdf"

(files <- drive_find(q = c("starred = true", "visibility = 'anyoneWithLink'")))



##################
##################
remotes::install_github("DesiQuintans/librarian")
install.packages("librarian")

librarian::lib_startup(librarian, magrittr, lib = "C:/Dropbox/My R Library", global = TRUE) # startup
librarian::lib_startup()  # to back to defaul loadings

librarian::shelf( dplyr , DesiQuintans/desiderata, phyloseq)
# If package didn’t exist on CRAN, it might be a Bioconductor package and will install from Bioconductor only if Biobase is already installed in your library.

# unattach dplyr, tidyr, and all of their dependencies (if no other loaded package needs them)
unshelf(dplyr, tidyr, also_depends = TRUE)
# un attach with dependenies:  unshelf(dplyr, tidyr, also_depends = TRUE, safe = FALSE)


browse_cran() #to search package names and descriptions
browse_cran("colorbrewer")

##################
# What If Plots



##################
#install.packages("dbx")
library(dbx)

# install.packages("RPostgres")
db <- dbx::dbxConnect(adapter="postgres", dbname="mydb")

#install.packages("RMySQL")
db <- dbx::dbxConnect(adapter="mysql", dbname="mydb")

#install.packages("RSQLite")
db <- dbx::dbxConnect(adapter="sqlite", dbname=":memory:")


# SQL Server
install.packages("odbc")
db <- dbx::dbxConnect(adapter=odbc::odbc(), database="mydb")

records <- dbxSelect(db, "SELECT * FROM forecasts")
dbxSelect(db, "SELECT * FROM forecasts WHERE period = ? AND temperature > ?", params=list("hour", 27))
dbxSelect(db, "SELECT * FROM forecasts WHERE id IN (?)", params=list(1:3))
table <- "forecasts"
records <- data.frame(temperature=c(32, 25))
dbxInsert(db, table, records)
dbxInsert(db, table, records, returning=c("id"))
records <- data.frame(id=c(1, 2), temperature=c(16, 13))
dbxUpdate(db, table, records, where_cols=c("id"))
bad_records <- data.frame(id=c(1, 2))
dbxDelete(db, table, where=bad_records)
dbxExecute(db, "UPDATE forecasts SET temperature = temperature + 1")
dbxExecute(db, "UPDATE forecasts SET temperature = ? WHERE id IN (?)", params=list(27, 1:3))

#################
library(rgl)

t <- seq(0, 100, by = 0.7)^0.6
x <- t * c(sin(t), sin(t + pi))
y <- t * c(cos(t), cos(t + pi))
z <- -2 * c(t, t)
color <- rep(c("darkgreen", "gold"), each = length(t))

open3d(windowRect = c(100, 100, 600, 600), zoom = 0.9)
bg3d("black")
spheres3d(x, y, z, radius = 0.3, color = color)

# On screen (skip if export)
play3d(spin3d(axis = c(0, 0, 1), rpm = 4))

# Export (requires 3rd party tool "ImageMagick" resp. magick-package)
# movie3d( spin3d(axis = c(0, 0, 1), rpm = 4), duration = 30, dir = getwd())

#############


---
title: "Habits"
author: John Doe
date: March 22, 2005
#output: pdf_document
output:
  pdf_document:
    keep_tex: true
    includes:
      in_header: preamble.tex
      before_body: doc-prefix.tex
      after_body: doc-suffix.tex
    template: quarterly-report.tex
    toc: true
    toc_depth: 2
    number_sections: true
    fig_width: 7
    fig_height: 6
    fig_caption: true
    df_print: kable
    highlight: tango
    citation_package: natbib
    latex_engine: xelatex

fontsize: 11pt
geometry: margin=1in
lang:	Document language code
fontsize:	Font size (e.g., 10pt, 11pt, or 12pt)
documentclass:	LaTeX document class (e.g., article)
classoption:	Options for documentclass (e.g., oneside)
geometry:	Options for geometry class (e.g., margin=1in)
mainfont, sansfont, monofont, mathfont:	Document fonts (works only with xelatex and lualatex)
linkcolor, urlcolor, citecolor	Color for internal, external, and citation links

---









library(dataCompareR)
# run the comparison
compIris <- rCompare(iris, iris2)
summary(compIris) # Check the results
# Write the summary to a file
saveReport(compIris, reportName = 'compIris')

# We'll use the pressure dataset for comparison
head(pressure)
# Make a copy of pressure
pressure2 <- pressure
# And change it, first by randomising the row order
pressure2 <- pressure2[sample(nrow(pressure2)),]
# then changing just one element, so for temperature of
pressure2[5,1]
# run the comparison
compPressure <- rCompare(pressure, pressure2, keys = 'temperature')
# Check the results - use print for a quick summary
print(compPressure)
# We can also extract the mismatching data to explore futher using generateMismatchData which
#  generates a list containing two data frames, each having the missing rows from the comparison.

# use generateMismatchData to pull out the mismatching rows from each table
mismatches <- generateMismatchData(compPressure, pressure, pressure2)

mismatches








library(tidyverse)
library(vtree)

FakeData
vtree::vtree(FakeData,"Severity")

vtree::vtree(mtcars,"gear cyl ")




FakeData %>% rename("HowBad"=Severity) %>% vtree("HowBad")


myaluvialdata <- mtcars
vtree::vtree(myaluvialdata, "Directorate")
vtree::vtree(myaluvialdata, "CCA_done")
vtree::vtree(myaluvialdata, c("CCA_done", "Directorate" ) )
vtree::vtree(myaluvialdata, c( "Directorate" , "CCA_done") ,
             #horiz=FALSE,
             sameline=TRUE,   # % on same line
             prunesmaller=15 ,
             splitwidth=6,
             #prune=list(Severity=c("Mild","Moderate")),  # or
             #keep=list(Severity="Moderate"),
             #prunebelow=list(Severity=c("Mild","Moderate"))
             #targeted pruning, and the parameters tprune, tkeep, tprunebelow, and tfollow

             #labelnode=list(Sex=c(Male="M",Female="F"))
             ) # only show with node above value

vtree(myaluvialdata, summary="Directorate")  # Useful !!

vtree(myaluvialdata, "Directorate",
      summary="Directorate",
      horiz=FALSE
      )



# vtree package uses markdown-style codes for text formatting. I
# \n	insert a line break
# \n*l	make the preceding line left-justified and insert a line break
# *...*	 italics,  **...** bold,  ^...^	superscript ,  ~...~	subscript
# %%red ...%%	 text in [red] color etc


vtree(FakeData,"Severity Sex",sameline=T, ,showlegend=TRUE,shownodelabels=FALSE)

ToothGrowth
vtree::vtree(ToothGrowth,"supp dose",  summary="len>20 \n%pct% length > 20")

vtree::vtree(InsectSprays,"spray",splitwidth=80,sameline=TRUE,
             summary="count \ncounts: %list%%noroot%",cdigits=0)

####################################
library(tabxplor)
set_color_style(type = "text", theme = "light")
# options(tabxplor.print = "kable") # default to options(tabxplor.print = "console")

forcats::gss_cat %>% tabxplor::tab( marital, race)

gss  <- "Source: General social survey 2000-2014"

forcats::gss_cat %>% tabxplor::tab( marital, race,
                                    pct = "row",
                                    na = "drop",
                                    subtext = gss,
                                    rare_to_other = TRUE,
                                    n_min = 1000,
                                    other_level = "Custom_other_level_name",
                                    color = "diff" # diff betweencell and it’s total
                                    )

# When a third variable is provided, tab makes a table with as many subtables as it has levels


data <- forcats::gss_cat %>%
       dplyr::filter(year %in% c(2000, 2006, 2012), !marital %in% c("No answer", "Widowed"))
gss2 <- "Source: General social survey 2000, 2006 and 2012"

data %>% tabxplor::tab(race, marital, year,
                       subtext = gss2,
                       pct = "row",
                       color = "diff") %>%  tab_kable()  # for printing
# set options(tabxplor.print = "kable") # default options(tabxplor.print = "console")












# control over labeling, colors, legends, line wrapping, and text formatting;
# flexible pruning to remove parts of lesser interest
# display of info in each node, including a variety of summary statistics;
# special displays for indicator variables, patterns of values, and missingness;
# support for checkbox variables from REDCap databases;
# features for dichotomizing variables and checking for outliers;
# automatic generation of PNG image files and embedding in R Markdown documents; and
# interactive panning and zooming using the svtree function to launch a Shiny app.

vtree(FakeData,"Severity Sex",prune=list(Severity=c("Mild","Moderate")))
vtree(FakeData,"Severity Sex",keep=list(Severity="Moderate"))


vtree(FakeData,"Sex Severity",tkeep=list(list(Sex="M",Severity="Moderate")))

vtree(FakeData,"Severity Sex Age Category",sameline=TRUE) # hard to see
vtree(FakeData,"Severity Sex Age Category",sameline=TRUE,prunesmaller=3) #remove smaller <3

vtree(FakeData,"Group Sex",horiz=FALSE,labelnode=list(Sex=c(Male="M",Female="F")))

# formating like md
# \n	insert a line break
# \n*l	make the preceding line left-justified and insert a line break
# *...*	display text in italics,  **...**	display text in bold
# ^...^	display text in superscript (using 10 point font)
# ~...~	display text in subscript (using 10 point font)
# %%red ...%%	display text in red (or whichever color is specified)

#install.packages("paletteer")
library(paletteer)
paletteer_c("scico::berlin", n = 10)

ggplot2::ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point() +
  scale_color_paletteer_d("nord::aurora")
>>>>>>> b289e0b6ca49947a675168849f1f5431a59faa32

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

# You can embed one code chunk in another code chunk by enclosing its label in <<>>. Then knitr will automatically expand the string <<label>>

#If you want to use exactly the same code chunk two or more times, you may define the chunk with a label, and create more code chunks with the same label but leave the chunk content empty, e.g.,

```{r, chunk-one, eval=TRUE}
```
You can also read an external file:
```{r, code=xfun::read_utf8('your-script.R')}
```
```{r, code=read_files(c('one.R', 'two.R'))}
```
```{r, child=c('one.Rmd', 'two.Rmd')}
```
Conditionally include the appendix:
```{r, child=if (!BOSS_MODE) 'appendix.Rmd'}
```


```{r, setup, include=FALSE}
knitr::opts_knit$set(root.dir = '/tmp')
```

# install from CRAN
install.packages("trackdown")

# install the development version
remotes::install_github("claudiozandonella/trackdown", build_vignettes = TRUE)


```{r, setup, include=FALSE}
library(officedown)
```


title: |
  ![ ](logo.jpg){width=1in}
Adding a Logo to LaTeX Title


---
  author:
  - John Doe^[Institution One, john@example.org]
  - Jane Doe^[Institution Two, jane@example.org]
---

roxygen comment is an R comment that starts with #'

chunc header  #+  knitr chunk header. For example, knitr::spin() will translate the comment #+ label, fig.width=5 to the chunk header ```{r label, fig.width=5} in R Markdown.
R code of the form {{ code }} is translated to an inline R expression
Any text between /* and */ will be ignored


citations
bibliography: references.bib
csl: biomed-central.csl  # we recommend using the Zotero Style Repository,

---
  nocite: |
  @item1, @item2
---

With @R-base. To put citations in parentheses, use [@key].
To cite multiple entries, separate the keys by semicolons, e.g., [@key-1; @key-2; @key-3].
To suppress the mention of the author, add a minus sign before @, e.g., [-@R-base].



install.packages('blogdown',
                 repos = c( rstudio = 'https://rstudio.r-universe.dev',
                             CRAN = 'https://cloud.r-project.org'         ))

##

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


##################
##################
##################
# Decision Trees in R , also called Trees and CART. main goal behind classification tree is to classify or predict an outcome based on a set of predictors.
# Decision trees are mainly classification and regression types.
# Classification means Y variable is factor and regression type means Y variable is numeric.
# Method 1:- Classification Tree
library(DAAG)
library(party)
library(rpart)
library(rpart.plot)
library(mlbench)
library(caret)
library(pROC)
library(tree)
# Getting Data -Email Spam Detection
str(spam7)
mydata <- spam7
# Data Partition
set.seed(1234)
ind <- sample(2, nrow(mydata), replace = T, prob = c(0.5, 0.5))
train <- mydata[ind == 1,]
test <- mydata[ind == 2,]
# Tree Classification
tree <- rpart(yesno ~., data = train)

rpart.plot(tree)
printcp(tree)
plotcp(tree)
# You can change the cp value according to your data set. Please note lower cp value means bigger the tree. If you are using too lower cp that leads to overfitting also.
tree <- rpart(yesno ~., data = train,cp=0.07444)

# Confusion matrix -train
p <- predict(tree, train, type = 'class')
confusionMatrix(p, train$yesno, positive='y')
# ROC
p1 <- predict(tree, test, type = 'prob')
p1 <- p1[,2]
r <- multiclass.roc(test$yesno, p1, percent = TRUE)
roc <- r[['rocs']]
r1 <- roc[[1]]
plot.roc(r1,
         print.auc=TRUE,
         auc.polygon=TRUE,
         grid=c(0.1, 0.2),
         grid.col=c("green", "red"),
         max.auc.polygon=TRUE,
         auc.polygon.col="lightblue",
         print.thres=TRUE,
         main= 'ROC Curve')

# Method 2- Regression  Tree
data('BostonHousing')
mydata <- BostonHousing
# Data Partition
set.seed(1234)
ind <- sample(2, nrow(mydata), replace = T, prob = c(0.5, 0.5))
train <- mydata[ind == 1,]
test <- mydata[ind == 2,]
# Regression tree
tree <- rpart(medv ~., data = train)
rpart.plot(tree)
printcp(tree)
# Regression tree:
rpart(formula = medv ~ ., data = train)

rpart.rules(tree)
medv
plotcp(tree)

p <- predict(tree, train)
sqrt(mean((train$medv-p)^2))
(cor(train$medv,p))^2






#####################
alpha = .01
reps = 100000
true.mean = 0
true.var = 1
true.prop = .25
raw = replicate(reps, rnorm(100,true.mean,true.var))
# Calculate the mean and standard error for each of the replicates
raw.mean = apply(raw, 2, mean)
raw.se = apply(raw, 2, sd)/sqrt( nrow(raw) )
# Calculate the margin of error
raw.moe = raw.se * qnorm(1-alpha/2)
# Set up upper and lower bound matrix. This format is useful for the graphs
raw.moe.mat = rbind(raw.mean+raw.moe, raw.mean-raw.moe)
row.names(raw.moe.mat) = c(alpha/2, 1-alpha/2)
# Calculate the confidence level
( raw.CI = (1-sum(
  as.numeric( apply(raw.moe.mat, 2, min) > 0 | apply(raw.moe.mat, 2, max) < 0 )
)/reps)*100 )
# Try some binomial distribution data
raw.bin.mean = rbinom(reps,50, prob=true.prop)/50
raw.bin.moe = sqrt(raw.bin.mean*(1-raw.bin.mean)/50)*qnorm(1-alpha/2)
raw.bin.moe.mat = rbind(raw.bin.mean+raw.bin.moe, raw.bin.mean-raw.bin.moe)
row.names(raw.bin.moe.mat) = c(alpha/2, 1-alpha/2)
( raw.bin.CI = (1-sum(
  as.numeric( apply(raw.bin.moe.mat, 2, min) > true.prop | apply(raw.bin.moe.mat, 2, max) <= true.prop )
)/reps)*100 )
par(mfrow=c(1,1))
ind = 1:100
ind.odd = seq(1,100, by=2)
ind.even = seq(2,100, by=2)
matplot(rbind(ind,ind),raw.moe.mat[,1:100],type="l",lty=1,col=1,
        xlab="Sample Identifier",ylab="Response Value",
        main=expression(paste("Confidence Intervals with ",alpha,"=.01")),
        sub=paste("Simulated confidence Level: ",raw.CI,"%", sep="")
        , xaxt='n')
axis(side=1, at=ind.odd, tcl = -1.0, lty = 1, lwd = 0.5, labels=ind.odd, cex.axis=.75)
axis(side=1, at=ind.even, tcl = -0.7, lty = 1, lwd = 0.5, labels=rep("",length(ind.even)), cex.axis=.75)
points(ind,raw.mean[1:100],pch=19, cex=.4)
abline(h=0, col="#0000FF")
size.seq = seq(0, 10000, by=500)[-1]
moe.seq = sqrt( (.5*(1-.5))/size.seq ) * qnorm(1-alpha/2)
plot(size.seq, moe.seq, xaxt='n', yaxt='n',
     main='Margin of Error and Sample Size',
     ylab='Margin of Error', xlab='Sample Size',
     sub='Based on 50% Proportion')
lines(size.seq, moe.seq)
axis(side=1, at=size.seq, tcl = -1.0, lty = 1, lwd = 0.5, labels=size.seq, cex.axis=.75)
axis(side=2, at=seq(0,15, by=.005), tcl = -0.7, lty = 1, lwd = 0.5, labels=seq(0,15, by=.005), cex.axis=.75)
abline(h=seq(0,15,by=.005), col='#CCCCCC')
abline(v=size.seq, col='#CCCCCC')
size.seq = seq(0,1, by=.01)
moe.seq = sqrt( (size.seq*(1-size.seq))/1000 ) * qnorm(1-alpha/2)

plot(size.seq, moe.seq, xaxt='n', yaxt='n',
     main='Margin of Error and Sample Size',
     ylab='Margin of Error', xlab='Proportion',
     sub='Based on 50% Proportion')
lines(size.seq, moe.seq)
axis(side=1, at=size.seq, tcl = -1.0, lty = 1, lwd = 0.5, labels=size.seq, cex.axis=.75)
axis(side=2, at=seq(0,15, by=.005), tcl = -0.7, lty = 1, lwd = 0.5, labels=seq(0,15, by=.005), cex.axis=.75)
abline(h=seq(0,15,by=.005), col='#CCCCCC')
abline(v=.5, col="#CCCCCC")

