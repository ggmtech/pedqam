## eiler vann diagram   incomplete

#install.packages("rJava")
install.packages("UpSetR")
# install.packages("tidyverse")
install.packages("venneuler")
# install.packages("grid")

library(rJava)
library(UpSetR)
library(tidyverse)
library(venneuler)
library(grid)
 
# Set the chart data
expressionInput <- c(`#rstats` = 5, memes = 5, `#rstats&memes` = 3)

# Create the Venn diagram
# note on set up for java v11 jdk (v12 does not work with this)
myExpVenn <- venneuler(expressionInput)
par(cex=1.2)
plot(myExpVenn, main = "Why I Love Twitter")
grid.text(
    "@littlemissdata",
    x = 0.52,
    y = 0.2,
    gp = gpar(
        fontsize = 10,
        fontface = 3
    )
)





# Create an UpsetR Plot
upset(fromExpression(expressionInput), order.by = "freq")
grid.text(
    "Why I Love Twitter  @littlemissdata",
    x = 0.80,
    y = 0.05,
    gp = gpar(
        fontsize = 10,
        fontface = 3
    )
)


# more complicted
# https://www.r-bloggers.com/set-analysis-a-face-off-between-venn-diagrams-and-upset-plots/

install.packages("usethis")
library(usethis)
use_course("https://github.com/lgellis/MiscTutorial/archive/master.zip")


