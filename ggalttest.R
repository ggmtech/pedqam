
# conflicts(where = search(), detail = FALSE)

## bee haney  graph
# https://buzzrbeeline.blog

library(ggplot2)
library(png)
library(grid)
library(ggimage)

setwd("/Users/gk/Google Drive")

bees <- data.frame(distance = c(0.5, 0.5, 1.5, 2, 2.5, 3),
                   number   = c(40, 34, 32, 22,18, 10))

img <- readPNG("honeycomb.png")
img
ggplot(data = bees, aes(x = distance, y = number)) +
    annotation_custom(rasterGrob(img, 
                                 width = unit(1,"npc"),
                                 height = unit(1,"npc")), 
                      -Inf, Inf, -Inf, Inf) +
    geom_point() +
    xlab("Distance (km)") +
    ylab("Number of Bees") +
    ylim(0, 45)

# To use bees as markers we need to add the bee image file name to every row of the data frame:
# Now edit your graph code again by removing the geom_point() and adding a geom_image() i  


bees$beeimage <- "honeybee.png"
    
ggplot(data = bees, aes(x = distance, y = number)) +
    annotation_custom(rasterGrob(img, 
                                 width = unit(1,"npc"),
                                 height = unit(1,"npc") ), 
                                 -Inf, Inf, -Inf, Inf) +
    geom_image( aes(image = beeimage),  size = 0.15 ) +
    xlab("Distance (km)" ) +
    ylab("Number of Bees") +
    ylim(0, 45)  +
    theme_bw()

bees$beeimage <- "WDP4.png"
ggplot(data = bees, aes(x = distance, y = number)) +
    annotation_custom(rasterGrob(img, width = unit(1,"npc"), height = unit(1,"npc") ), -Inf, Inf, -Inf, Inf) +
    geom_image( data = NULL, inherit.aes = TRUE, na.rm = FALSE,
                aes(image = beeimage),  
                size = 0.25 , by =  "width", # "height", 
                color = "red", nudge_x = 0.1, angle = 30, position = "identity"
                ) +
    xlab("Distance (km)" ) +
    ylab("Number of Bees") +
    ylim(0, 45)  +
    theme_bw()  + coord_flip()

##############


library(ggplot2)
library(cowplot)  

plot.mpg <- ggplot(mpg,  aes(x = cty, y = hwy, colour = factor(cyl)  ) ) +  geom_point(size = 2.5)
plot.mpg    # ~ theme_classic for grey  theme_set(theme_gray())

# use save_plot(fname, plot, base_aspect_ration= 4:9) instead of ggsave()
# save_plot(filename, plot, ncol = 1, nrow = 1, base_height = 4, base_aspect_ratio = 1.1, base_width = NULL, ..., cols = NULL, rows = NULL)
save_plot( "mpg.png",  plot.mpg,  base_aspect_ratio = 1.3 )  # make room for figure legend

plot.mpg + background_grid(major = "xy", minor = "none")   # gridlines with background_grid(): 
plot.mpg

draw( plot.mpg ) 
ggdraw( plot.mpg ) +   draw_label( "DRAFT!" ,  size = 40, angle = 30,  alpha = .2) # adding draw layers !!

plot.diamonds <- ggplot(diamonds, aes(clarity, fill = cut)) + geom_bar() +  theme(axis.text.x = element_text(angle=70, vjust=0.5))

plot_grid(  plot.mpg,   plot.diamonds,    labels = c("A", "B") , nrow = 1, align = "h"   )  # combine graphs with plot_grid(), h align:

plot2by2  <- plot_grid(  plot.mpg,   NULL,   NULL,   plot.diamonds,   labels = c("A", "B", "C", "D"),  ncol = 2 )

# save_plot(), with grid plot_grid(), eg 2-by-2 figure:
save_plot( "plot2by2.png",  plot2by2,   ncol = 2,  nrow = 2,  base_aspect_ratio = 1.3  )  # each  subplot  aspect 1.3

ggdraw(plot2by2)
ggdraw(plot.mpg)

#  ggdraw() sets up the drawing layer, and functions on this drawing layer all start with draw_. 
#  ggdraw() produces a standard ggplot2 object, and you can do whatever like ggplot2
ggdraw(plot.mpg) +   draw_label( "DRAFT!"        ,  size = 40, angle =  45,  alpha = .2) +
                     draw_label( "add anothor !" ,  size = 20, angle = -45,  alpha = .2) +
                     draw_plot_label("A mix drawing", size = 24,angle = -5,  alpha = .4 )

t <- (0:1000)/1000   # good simple power examiple
spiral <- data.frame( x = .45 + .55*t*cos(t*15),  y = .55-.55*t*sin(t*15),  t)
spiral
ggdraw(plot.mpg) +  geom_path( data = spiral, aes(x = x, y = y, colour = t), size = 6, alpha = .4) 
                
# Ordering layers, you can initialize an empty drawing canvas by calling ggdraw() without any parameters. 

# draw_plot() function also allows us to place graphs at arbitrary locations 

# image or plot  inset plot.mpg and plot.diamonds were defined earlier
library(viridis)
ggdraw() +
            draw_plot(plot.diamonds  + theme(legend.justification = "bottom"), 0, 0, 1, 1 )   +  # The main chart
            draw_plot(plot.mpg + scale_color_viridis(discrete = TRUE) + 
                                   theme(legend.justification = "top"), 0.6, 0.6, 0.4, 0.4 )  +  # the inset chart
            draw_plot_label(c("A", "B"), c(0, 0.3), c(1, 0.8), size = 5)

# with image
p    <-    ggplot(iris, aes(x=Sepal.Length, fill=Species)) + geom_density(alpha = 0.7)

ggdraw()  +    
            # draw_image("http://jeroen.github.io/images/tiger.svg") +
            draw_image( "plot2by2.png"  , scale = 0.80  ) +     # used image !!
            draw_plot(plot.mpg)   +
            draw_plot(p)

# side by side with image
p <- ggplot(iris, aes(x = Sepal.Length, fill = Species)) + geom_density(alpha = 0.7)
p2 <- ggdraw() + draw_image("http://jeroen.github.io/images/tiger.svg", scale = 0.9)  # or
p2 <- ggdraw() + draw_image("plot2by2.png", scale = 0.9)
plot_grid( p,  p2,  p2, p , labels = "AUTO")


######## autoplot  with  ggfortify  ######
#install.packages("ggfortify")
library("ggfortify")

# Plotting matrix using autoplot.matrix()   
# autoplot(object, geom = "tile")  object (of class matrix) and geom are “tile” (for heatmap) or “point” (scatter plot)

df <- mtcars[, c("mpg", "disp", "hp", "drat", "wt")]
df <- as.matrix(df)
df

scale(df)   # applied to matrix objects
autoplot(  scale( df )   )      # Heatmap, pre sort would be better?

# Plot a scatter plot: The data  be a matrix with 2 columns named V1 and V2. 

df2  <-  df[  ,  c("wt", "mpg")  ]  #  plots mpg by wt
colnames(df2) <- c("V1", "V2")
autoplot(df2, geom = 'point') +     labs(x = "mpg", y = "wt")     # Scatter plot

# autoplot.lm() draw diagnostic plots for LM and GLM [in stats package].
# autoplot(object, which = c(1:3, 5))  #  object: stats::lm instance
#    which: subset of the plots if  required, specify  subset 1:6.
# ncol and nrow allows you to specify the number of subplot columns and rows.

m <- lm(Petal.Width ~ Petal.Length, data = iris)     # Compute a linear model

autoplot(m, which = 1:6, ncol = 2, label.size = 3)   # Create the plot
autoplot(m, which = 1:6,           label.size = 3, data = iris, colour = 'Species') # Change the color by groups (species)

# Diagnostic plots with Generalized Linear Models (GLM)  # USArrests data

m <- glm(Murder ~ Assault + UrbanPop + Rape, family = gaussian, data = USArrests)  # Compute a generalized linear model

autoplot(m, which = 1:6, ncol = 2, label.size = 3, colour = "steelblue") + theme_bw() # Create the plot # Change the theme and colour

# Plotting time series
# R Function: autoplot.ts()
autoplot(AirPassengers)

# autoplot() handles time-series-likes packages, 
#  zoo::zooreg(), xts::xts(), timeSeries::timSeries(), tseries::irts(), forecast::forecast(), vars:vars()


library(changepoint)    # for identifying shifts in mean and/or variance in a time series.
autoplot(  cpt.meanvar(AirPassengers)  )

library(strucchange)    # struc change detecting jumps in data.  
autoplot(  breakpoints(Nile ~ 1) )  #Data set: Nile

# Plotting PCA (Principal Component Analysis)  : Data set: iris, Function: autoplot.prcomp()
df <- iris[, -5]
df
pca <- prcomp(df, scale. = TRUE)    # Principal component analysis
autoplot(pca, loadings = TRUE, loadings.label = TRUE, data = iris, colour = 'Species')   # Plot

# Plotting K-means   Data set: USArrests,  Function: autoplot.kmeans() , original data  required as kmeans object doesn’t store original data. 
# Samples will be colored by groups (clusters).

autoplot(  kmeans(USArrests, 5),  data = USArrests,  label = TRUE, label.size = 3, frame = TRUE)  # pca 5

# Plotting cluster package
# ggfortify supports cluster::clara, cluster::fanny and cluster::pam classes, aand return object containing original data, so not reqd explicitly.
library(cluster)
autoplot(  pam(iris[-5], 4),  frame = TRUE,  frame.type = 'norm' )  # clusters 4


# Plotting Local Fisher Discriminant Analysis
library(lfda)       # Local Fisher Discriminant Analysis (LFDA)
model <- lfda(iris[,-5], iris[, 5], 4, metric="plain")
autoplot(model, data = iris, frame = TRUE, frame.colour = 'Species')


# Plotting survival curves
library(survival)
lung
fit <- survfit(  Surv(time, status)  ~  sex ,   data = lung   )
fit
autoplot(fit)

################
library(tidyverse)
library(hrbrthemes)
library(ggalt)

df <- data.frame(   trt=LETTERS[1:5] , 
                    l=c(20, 40, 10, 30, 50), 
                    r=c(70, 50, 30, 60, 80)  )

# ???  
x <- read.table(file = "clipboard" )



ggplot(df, aes(y=trt, x=l, xend=r)) + 
    geom_dumbbell(size     =3,   color="#e3e2e1", 
                  colour_x  = "#5b8124", colour_xend = "#bad744",
                  dot_guide = TRUE, dot_guide_size=0.25 ) +
    labs(x=NULL, y=NULL, title="ggplot2 geom_dumbbell with dot guide") +
    #theme_ipsum_rc(grid="X") +
    theme(panel.grid.major.x=element_line(size=0.05)) +
    labs(x=NULL, y=NULL, 
         title="SUNY Cortland Multicultural Alumni survey results",
         subtitle="Ranked by race, ethnicity, home land and orientation\namong the top areas of concern",
         caption="Data from http://stephanieevergreen.com/lollipop/") +
         theme_minimal(base_family="Arial Narrow") +
         theme(panel.grid.major.y=element_blank()) +
         theme(panel.grid.minor=element_blank()) +
         theme(axis.line.y=element_line(color="#2b2b2b", size=0.15)) +
         theme(axis.text.y=element_text(margin=margin(r=0, l=0))) +
         theme(plot.margin=unit(rep(30, 4), "pt")) +
         theme(plot.title=element_text(face="bold")) + 
         theme(plot.subtitle=element_text(margin=margin(b=10))) +
         theme(plot.caption=element_text(size=8, margin=margin(t=10)))  +# or +    
         # theme_ipsum()    #library(hrbrthemes)
         geom_label(aes(x = 3.5, y = 2.5, label = "I'm an annotation!"), 
             hjust = 0,  vjust = 0.5,    colour = "#555555",   fill = "white",  label.size = NA, family="Helvetica", 
             size = 3)


