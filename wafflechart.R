# wafflecharrt
(spec <- table(iris$Species))
waffle::waffle(spec)
waffle::waffle(spec, rows = 3, legend_pos = "bottom")
waffle::waffle(spec, rows = 15, colors = c("lightgrey", "darkgrey", "red"))
waffle::waffle(spec / 10, rows = 5, xlab = "1 square = 10 flowers")# do not forget to annotate 1 sq = 10 units!

waffle::iron( waffle::waffle(spec / 5, rows = 5, title = "iron() combines waffles"),
              waffle::waffle(spec / 10, rows = 5, xlab = "1 square = 10 flowers")  )  # combiing waffles with waffle::iron

#  replace the tiles by pictures from the extrafont package. Plus ggwaffle developed now



#  swarmplot, or beeswarmplot, and is already hosted on CRAN in the form of the ggbeeswarm package
# install.packages("ggbeeswarm")
library(ggbeeswarm)  # geom_quasirandom and geom_beeswarm, official ggplot2
#### theme_set(theme_light()) # sets a default ggplot theme
ggplot(iris, aes(Species, Sepal.Length)) + ggbeeswarm::geom_beeswarm()
ggplot(iris, aes(Species, Sepal.Length, col = Species)) + geom_beeswarm(size = 2)
ggplot(iris, aes(Species, Sepal.Length, col = Species)) + geom_beeswarm(size = 3, cex = 3)  # cex to adjust spacing for large data
#  geom_beeswarm(groupOnX = FALSE)  to turn off separation
# For another grouping variable besides those on the axis,  consider using the dodge.width
ggplot(iris, aes(Species, Sepal.Length, col = Sepal.Length > 5)) + geom_beeswarm(dodge.width = 0.5)
# geom_quasirandom, alternative geom_jitter
ggplot(iris, aes(Species, Sepal.Length, col = Species)) + geom_quasirandom()
ggplot(iris, aes(Species, Sepal.Length, col = Species)) + geom_quasirandom(size = 2, method = "smiley") # !! it allows many patterns!

# Personograph
install.packages("devtools")
devtools::install_github("joelkuiper/personograph")

library(personograph)
colorlist <- list(FN = "darkred", TP = "red", FP = "darkgreen", TN = "green")

TPR <- 0.95 # true positive rate = sensitivity
TNR <- 0.95 # true negative rate = specificity
IR <- 0.05 # infection rate
data <- list(FN = (1-TPR)*IR, TP = TPR*IR, FP = (1-TNR)*(1-IR), TN = TNR*(1-IR))
NPV <- round(100 * data$TN / (data$TN + data$FN), 1) # negative predictive value
PPV <- round(100 * data$TP / (data$TP + data$FP), 1) # positive predictive value
personograph(data, 
             colors = colorlist, 
             fig.title = paste0("PPV = ", PPV, "%, NPV = ", NPV, "%"), 
             n.icons = 500, 
             dimensions = c(20, 25)  )


#  paulinelemenkova/  37-R-Waffle-Chart/ Public
# pauline.lemenkova@gmail.com http://www.linkedin.com/in/paulinelemenkova
# Рисуем вафельный график через библиотеку waffle
# шаг-1 загружаем таблицу
MDF <- read.csv("Morphology.csv", header=TRUE, sep = ",")
MDF <- read_csv("https://github.com/paulinelemenkova/37-R-Waffle-Chart/raw/master/Morphology.csv" )
MDF
MDF <- na.omit(MDF) 
row.has.na <- apply(MDF, 1, function(x){any(is.na(x))}) 
sum(row.has.na) 

head(MDF)

library(waffle)   # шаг-2, вариант-1 для класса аспекта (стороны света угла)
var <- MDF$aspect_class  
nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
categ_table <- round(table(var) * ((nrows*nrows)/(length(var))))
categ_table


aspect <- c("East"=8, "North" =24, "North-East" = 8, "North-West" = 16,
            "South" = 4, "South-East" = 12, "South-West" = 16, "West" = 12 )
aspect

wa<- waffle(aspect, rows=10, size=0.5, 
            colors=c("aquamarine", "cadetblue1", "darkseagreen1", "papayawhip", "thistle1",   "lavender", "lightsteelblue1", "mistyrose"), 
            title=list("Aspect degree in basement. \nMariana Trench, bathymetric profiles 1:25", size=0.3), 
            xlab="total square: 100%")
wa


# шаг-2, вариант-2 для класса морфологии (крутизна угла)
var <- MDF$morph_class  
nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
categ_table <- round(table(var) * ((nrows*nrows)/(length(var))))
categ_table

library(waffle)
morph <- c("Extreme Slope"=24, "Steep Slope" =20, "Strong Slope" = 16, "Very Steep Slope" = 40)
morph

wm<- waffle(morph, rows=10, size=0.5, 
            title=list("Steepness angle morphology class. \nMariana Trench, bathymetric profiles 1:25", size=0.3), 
            xlab="total square: 100%")
wm

# заутюжим их вместе на 1 рисунок
iron(wa, wm)

##############################
library(personograph)
# https://github.com/joelkuiper/personograph
# https://cran.r-project.org/web/packages/personograph/index.html

# the data is supplied in a list and is plotted in order. 
data <- list(first=0.06, second=0.94)
personograph(data,  
             colors=list(first="black", second="#efefef"),
             fig.title = "100 people who do not eat bacon",
             draw.legend = FALSE, 
             dimensions=c(5,20)   )
data_2 <- list(first=0.06, second=0.01, third=0.83, fourth=0.1)
personograph(data_2, colors=list(first="black", second="red", third="#efefef", fourth ="blue"),
             fig.title = "100 people who eat bacon every day",
             draw.legend = FALSE,  dimensions=c(5,20))
# in waffle
library(waffle)
dont_eat_bacon <- c('Cancer' = 6, 'No cancer' = 94)  # out of hundred
waffle(dont_eat_bacon, rows = 5, colors = c("#000000", "#efefef"),
       legend_pos = "bottom", title = "100 people who do not eat bacon")

eat_bacon <- c('Cancer' = 6, 'Extra case' = 1, 'No cancer' = 93)
waffle(eat_bacon, rows = 5, colors = c("#000000", "#f90000","#efefef"),
       legend_pos = "bottom", title = "100 people who eat bacon every day")


# Try a third package - ggwaffle
library(readr)
library(ggwaffle)
library(ggpubr)
library(ggimage)
theme_set(theme_pubr())

# in this case, we basically encode every point as a graph. 
# download the data
link <- ("https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/ggwaffledata_mf.csv")
bacon_waf <- read_csv(link)
View(bacon_waf) # have a look at data
bacon_waf
# basic plot with geom_waffle() from ggwaffle package
ggplot(bacon_waf, aes(x, y, fill = bacon)) + 
  geom_waffle()
# above from https://rforbiochemists.blogspot.com/2019/04/an-icon-plot-inspired-by-bacon-and-new.html

# https://rforbiochemists.blogspot.com/



#########################
# http://www.ehes.info/rc/tools/EHES_reporting_options_using_R_publish.html
library(personograph)
country <- "Netherlands"
sex <- "women"
# selecting proportions and formatting those to (0-1)
val <- round(indicators[indicators$COUNTRY_NAME==country & indicators$SEX==sex, catvar]/100, digits=3)
# val2 includes the value above
val2 <- round(indicators[indicators$COUNTRY_NAME==country & indicators$SEX==sex, catvar2]/100, digits=3)
# 3 classes that must sum up to 1
values <- list(obese=val, overweight=val2-val, normal=1-val2)
# plot with colors, 100 icons and defined colors
personograph(values, n.icons=100, colors=list(obese="red", overweight="orange", normal="lightgreen"), draw.legend=TRUE, fig.cap = "", icon.style=3)


library(ggplot2)
sex <- "men" 
inds <- indicators[indicators$SEX==sex, ]
inds <- indicators[indicators$SEX==sex, ]  
inds <- inds[order(inds[, catvar]), ] # ordering data
inds$COUNTRY <- factor(inds$COUNTRY_NAME, levels = inds$COUNTRY_NAME, ordered = TRUE) #  factor 
grid_col <- "grey50"
grid_size <- 0.6
major_grid <- 0:10 * 5
p <- ggplot(inds, aes_string(x="COUNTRY", y=catvar)) + 
  fill_images(inds, catvar) + 
  clip_images(inds, catvar) +
  geom_bar(fill=NA, colour=NA, size=0.2, alpha=1, stat="identity") + 
  coord_flip() + 
  scale_y_continuous(breaks=seq(0, 100, 10)) + 
  scale_x_discrete() + 
  theme_bw() + 
  theme(axis.title.x  = element_blank(), axis.title.y  = element_blank(),
        panel.grid.major.x = element_line(colour = grid_col, size = grid_size),
        panel.grid.major.y = element_line(colour = NA))
p

##############################
getwd()
library(png)
man<-readPNG("flowery.png")
pictogram(icon=man,
          n=c(12,35,52),
          grouplabels=c("dudes","chaps","lads"))


##############################
# devtools::install_github("hrbrmstr/waffle")
library(waffle)
x <- sample(c("A","B"),100,replace = T)
x <- c(A=sum(x=="A"), B=sum(x=="B"))
waffle(x, rows=10, colors=c("red", "green"))


#############
#############
devtools::install_github("rubenarslan/formr")
library(formr)
qplot_waffle(rep(1:3,each=40,length.out=90))


library(ggplot2)

qplot_waffle_text(rep(1, each = 30), symbol = "", rows = 3) + ggtitle("Number of travellers in 2008")










