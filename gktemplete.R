#gktemplete
library(here)
load(here(), loadinglibs.R)


# loadingkubs.R for loading inital packages various means
# rnotes.R  small learning notes
# webscraptesting.R   webscraping functions and notes
# dataparsing.R
# plottinglibraries.R
# approximatedatamatch
# opencv  computer vision libraires
# forecasting.R  time data series forcating


# Parametrising Rmd
# params:
#   animal_name:
#   value: Dog
# choices:
#   - Dog
# - Cat
# - Rabbit
# years_of_study:
#   input: slider
# min: 2000
# max: 2019
# step: 1
# round: 1
# sep: ''
# value: [2010, 2017]
# Now write these variables into the R code  as params$animal_name and params$years_of_study

# Sys.setenv(RETICULATE_PYTHON = "path_to_env/bin/python3")
# Now .Rprofile  run every time
# ```{python}
# # write python code
# ```


#code_folding: in the YAML #  will hide code chunks  but provide click-down boxes to view code,

# code as per doc
if (knitr::is_latex_output()) {
  # insert code for static graphic
} else {
  # insert code for interactive graphic
}


library(ggplot2)
library(patchwork)
p1 <- ggplot2::ggplot(mtcars) + ggplot2::geom_point(aes(mpg, disp))
p2 <- ggplot2::ggplot(mtcars) + ggplot2::geom_boxplot(aes(gear, disp, group = gear))
p3 <- ggplot2::ggplot(mtcars) + ggplot2::geom_smooth(aes(disp, qsec)) 
p4 <- ggplot2::ggplot(mtcars) + ggplot2::geom_bar(aes(carb)) 

(p1 | p2 | p3) / 
  p4


# Use interactive graphics in your documents by embedding plotly
# ![](path/to/image), and not ![]("path/to/image"). There are no quote marks!


# https://github.com/EmilHvitfeldt/r-color-palettes
# interactive Color Picker at https://emilhvitfeldt.github.io/r-color-palettes/
# showcase the palettes in R (packages). 
# AndreaCirilloAC/paletter; jolars/qualpalr ;ronammar/randomcoloR
# johnbaums/hues ;ColorPalette ;  ; oaColors; earthtones ; leeper/colourlovers, etc

# Bookdown : Multiple output formats: PDF, LaTeX, HTML, EPUB, Word. in R, C/C++, Python, and SQL, etc.
# remotes::install_github('rstudio/bookdown')
# start a new Bookdown project from within RStudio IDE.
# Go to File > New Project > New Directory > Book project using bookdown.
# 
# This will create a new directory with an example book as template. 
# Go into the Build Pane in the RStudio IDE
# Click on Build Book > bookdown::gitbook
# You can also run bookdown::render_book() in the R console.

# sample _bookdown.yml:
#   
# book_filename: "my-book.Rmd"
# delete_merged_file: true
# before_chapter_script: ["script1.R", "script2.R"]
# after_chapter_script: "script3.R"
# view: https://github.com/rstudio/bookdown-demo/blob/master/%s
# edit: https://github.com/rstudio/bookdown-demo/edit/master/%s
# output_dir: "book-output"
# clean: ["my-book.bbl", "R-packages.bib"]


# The chunk option child is then used in index.Rmd to include other *.Rmd:
#   
#   ---
#   title: "Introduction to R Markdown"
# output:
#   xaringan::moon_reader:
#   nature:
#   beforeInit: ["addons/macros.js", "https://platform.twitter.com/widgets.js"]
# css: [default, default-fonts, addons/custom.css]
# ---
#   ```{r setup, include=FALSE}
# knitr::opts_chunk$set(echo = FALSE)
# options(knitr.duplicate.label = 'allow')
# ```
# ```{r child='opening.Rmd'}
# ```
# ---
#   ```{r child='intro-github.Rmd'}
# ```
# ---
#   ```{r child='contact.Rmd'}
# 
# 


# To make the source document cleaner,  put code of these kinds in separate text files
#  and include by knitr::asis_output(readLines('html-table.txt'))2 and R Markdown inline code `r `:
  
# A table generated from
# [TablesGenerator.com](https://www.tablesgenerator.com/html_tables#):
#`r knitr::asis_output(readLines('html-table.txt'))`




