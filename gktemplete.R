#gktemplete

rmarkdown::render('/Users/gk/gkgit/pedqam/example-beamer.Rmd',  encoding = 'UTF-8')

install.packages("webshot")
webshot::install_phantomjs()



setwd("/Users/gk/gkgit/pedqam")
library(here)
here()

list.files()
load( here(), "loadinglibs.R" )


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

p <- ggplot(iris, aes(Petal.Width, fill=Species))
p
p + geom_blank()
p + geom_dotplot()
p + geom_histogram()
p + geom_density()  # + scale_fill_brewer(palette="Set3")
p + geom_freqpoly(aes(color=Species))
p + geom_boxplot()  # + scale_fill_brewer(palette="Set3")
p + geom_violin()

p + geom_density() +  
   scale_fill_manual(
    values=c("red","yellow", "blue"),
    labels=c("setosa", "versicolor", "virginica"))
# scale_color_distiller(palette="YlGnBu") # continous colour
# scale_color_brewer(palette="Set1") # descreat var

g + geom_label(data=maxh_df, size=4, aes(T4, height, label=year) )
g + ggrepel::geom_label_repel(data=maxh_df, size=4,aes(T4, height, label=year))
g + annotate("text", x=12, y=100,  label="1974", size=12)
g + annotate("rect", xmin=15, xmax=18,  ymin=-Inf, ymax=Inf, alpha=0.2, fill="red")

g <- ggplot(iris,  aes(Sepal.Length, Sepal.Width, color=Species)) + geom_point()
g
plotly::ggplotly(g)


p + geom_abline(intercept=-0.4,slope=0.4) # y= a +mx
p + geom_smooth(method="lm")
p + geom_hline(yintercept=0)
p + geom_vline(xintercept=0)
p + geom	#Description
p + geom_abline	#Reference lines: horizontal, vertical, and diagonal
geom_bar	    #Bar charts
geom_bin2d	  #Heatmap of 2d bin counts
geom_blank	  #Draw nothing
geom_boxplot	#A box and whiskers plot (in the style of Tukey)
geom_contour	#2d contours of a 3d surface
geom_count	  #Count overlapping points
geom_density	#Smoothed density estimates
geom_density_2d	#Contours of a 2d density estimate
geom_dotplot	#Dot plot
geom_errorbarh#Horizontal error bars
geom_hex	    #Hexagonal heatmap of 2d bin counts
geom_freqpoly	#Histograms and frequency polygons
geom_jitter	  #Jittered points
geom_crossbar	#Vertical intervals: lines, crossbars & errorbars
geom_map	    #Polygons from a reference map
geom_path	    #Connect observations
geom_point	  #Points
geom_polygon	#Polygons
geom_qq_line	#A quantile-quantile plot
geom_quantile	#Quantile regression
geom_ribbon	  #Ribbons and area plots
geom_rug	    #Rug plots in the margins
geom_segment	#Line segments and curves
geom_smooth	  #Smoothed conditional means
geom_spoke	  #Line segments parameterised by location, direction and distance
geom_label	  #Text
geom_raster	  #Rectangles
geom_violin	  #Violin plot




library(patchwork)
p1 <- ggplot2::ggplot(mtcars) + ggplot2::geom_point(aes(mpg, disp))
p2 <- ggplot2::ggplot(mtcars) + ggplot2::geom_boxplot(aes(gear, disp, group = gear))
p3 <- ggplot2::ggplot(mtcars) + ggplot2::geom_smooth(aes(disp, qsec)) 
p4 <- ggplot2::ggplot(mtcars) + ggplot2::geom_bar(aes(carb)) 

(p1 | p2 | p3) / 
  p4

# ear8 + ear9 + ear10 + ear11 + plot_layout(ncol = 2)
 
# 
# 
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




