# Install the relevant libraries - do this one time
#install.packages("data.table")
#data()

#install.packages("formattable")

#Load the libraries
library(tidyverse)
library(data.table)

library(formattable)

#Set a few color variables to make our table more visually appealing
customGreen0 = "#DeF7E9"
customGreen  = "#71CA97"
customRed    = "#ff7f7f"


#Download the Austin indicator data set
austinData= fread('https://raw.githubusercontent.com/lgellis/MiscTutorial/master/Austin/Imagine_Austin_Indicators.csv', data.table=FALSE, header = TRUE, stringsAsFactors = FALSE)
head(austinData)
attach(austinData)

####
#data()

iris
i1 <- iris
i1
#0) Throw it in the formattable function
formattable(i1)

#1)  First Data Table  with spruce up
formattable(i1, 
            align =c("l","c","c","c","c", "c", "c", "c", "r"), 
            list(
                `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold") ) 
               )
            )


#2) Add the color mapping for all 2011 to 2016.
formattable( i1[1:6,], 
     align =c("l","c","c","c","c", "c", "c", "c", "r"), 
     list(
    `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
    `2011`= color_tile(customGreen, customGreen0),
    #`Sepal.Width`= color_tile(customGreen, customGreen0),
    `Sepal.Width` = formatter("span",      # formating as per formula
                              style = x ~ style(
                                  font.weight = "bold", 
                                  color = ifelse(x > 3.2, customGreen, ifelse(x < 3.1, customRed, "black")))),
    `2013`= color_tile(customGreen, customRed),
    `Petal.Length`= color_tile(customGreen, customRed),
    `2016`= color_tile(customGreen, customGreen0),
    `Sepal.Length` = color_bar(customRed),  # width proportional to figure
    #`Petal.Width`= color_tile(customRed, customGreen),
    `Petal.Width` = formatter("span",      # formating as per formula
                    style = x ~ style(
                     font.weight = "bold", 
                     color = ifelse(x > 0.2, customGreen, ifelse(x < 0.3, customRed, "black"))))
))


######
formatter("span", 
          style = x ~ style(font.weight = "bold", 
                            color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))), 
                            x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"), x)
)

######


library(formattable)
formattable(prevalence)
data()
prevalence <- rep(3, 10)
prevalence$rand <- rand(3, 0.3)

library(formattable)
library(sparkline)
prevalence$mycol = c(4.1, -.3, .5, 1.4)
prevalence$`2012` = apply(prevalence[, 2:7], 1, FUN = function(x) as.character(htmltools::as.tags(sparkline(as.numeric(x), type = "line"))))
names(prevalence)[3] = "  "
new.prevalance = prevalence[, c(1, 2, 3, 7, 10)]                          
out = as.htmlwidget(formattable(new.prevalance,
                                align = c("l",rep("r", NCOL(prevalence) - 1)), 
                                list(`Indicator Name` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")),
                                     " " = formatter("span", 
                                                     style = ~ style(color = ifelse(`2016` > `2011`, "green", "red")),                                    
                                                     ~ icontext(sapply(` `, function(x) if (x < -1.96) "arrow-down" else if (x> 1.96) "arrow-up" else ""))))))                          
out$dependencies = c(out$dependencies, htmlwidgets:::widget_dependencies("sparkline", "sparkline"))
out

