######### plotly interactive #########
# picturplots.R ffor hchart

# Libraries
library(ggplot2)
library(highcharter)
library(dplyr)
library(plotly)
library(viridis)
library(rayshader)

#hrbrthemes::import_roboto_condensed()
library(hrbrthemes)
# or 
# geom_point() + scale_x_continuous("Item Visibility", breaks = seq(0,0.35,0.05))+ scale_y_continuous("Item MRP", breaks = seq(0,270,by = 30))
# geom_histogram(binwidth = 2)
# geom_bar(fill = "red")  + coord_flip()
# geom_boxplot(fill = "red") + scale_y_continuous("Item Outlet Sales", breaks= seq(0,15000, by=500)) +
# geom_area(stat = "bin", bins = 30, fill = "steelblue") + scale_x_continuous(breaks = seq(0,11000,1000))
# heatmap geom_raster(aes(fill = Item_MRP))+
# scale_x_continuous("Item Visibility", breaks = seq(0,0.35,0.05))+
# scale_y_continuous("Item MRP", breaks = seq(0,270,by = 30))+
# facet_wrap( ~ Item_Type)
# library(corrgram)  corrgram(train, order=NULL, panel=panel.shade, text.panel=panel.txt,    main="Correlogram") 


# pairs plot,
data<-dplyr::select(mtcars,mpg,disp,hp)
graphics::pairs(data)
GGally::ggpairs(data)  +  theme_bw()
GGally::ggpairs(data,
        upper = list(continuous = "density", combo = "box_no_facet"),
        lower = list(continuous = "points", combo = "dot_no_facet")    )


data(tips, package = "reshape")
GGally::ggpairs( tips[, c(1, 3, 4, 2)],
        upper = list(continuous = "density", combo = "box_no_facet"),
        lower = list(continuous = "points", combo = "dot_no_facet")    )

data(flea)
GGally::ggpairs(flea, columns = 2:4, ggplot2::aes(colour=species))

GGally::ggpairs(
   tips[, c(1, 3, 4, 2)],
   upper = list(continuous = "density", combo = "box_no_facet"),
   lower = list(continuous = "points",  combo = "dot_no_facet"))

###################

library(gapminder)
data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)

# Interactive version
p <- data %>%
  mutate(gdpPercap=round(gdpPercap,0)) %>%
  mutate(pop=round(pop/1000000,2)) %>%
  mutate(lifeExp=round(lifeExp,1)) %>%
  
  # Reorder countries to having big bubbles on top
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  
  # prepare text for tooltip
  mutate(text = paste("Country: ", country, 
                      "\nPopulation (M): ", pop, 
                      "\nLife Expectancy: ", lifeExp, 
                      "\nGdp per capita: ", gdpPercap, sep="" ) ) %>%
  
  # Classic ggplot
  ggplot( aes(x=gdpPercap, y=lifeExp, size = pop, color = continent, text=text ) ) +
  geom_point( alpha=0.7 ) +
  scale_size( range = c(1.4, 19), name="Population (M)") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  theme_ipsum() +
  theme(legend.position="none")
p
# turn ggplot interactive with plotly
pp <- ggplotly(p,  tooltip="text")
pp


# base_fig +   theme(text = element_text(family = "Times New Roman")) # change fonts  or
# base_fig +   theme(plot.title    = element_text(family = "mono"),
#         plot.subtitle = element_text(family = "sans"),
#         axis.title.x  = element_text(family = "Comic Sans MS"),
#         axis.title.y  = element_text(family = "AppleGothic"),
#         axis.text.x   = element_text(family = "Optima"),
#         axis.text.y   = element_text(family = "Luminari"))

# save the widget
# library(htmlwidgets)
# saveWidget(pp, file=paste0( getwd(), "/HtmlWidget/ggplotlyBubblechart.html"))








# Add marginal rugs
mtcars %>% ggplot(aes(x=wt, y=mpg, color=cyl)) +
          geom_point()    + 
          geom_rug()      +  # Add marginal rugs
          theme_bw()

# Scatter plot with the 2d density estimation
sp <- ggplot(faithful, aes(x=eruptions, y=waiting))  +    geom_point()

sp + geom_density_2d()
sp + stat_density_2d(   aes(fill = ..level..), geom="polygon"     )     # Gradient color
sp + stat_density_2d(   aes(fill = ..level..), geom="polygon")  +  # Change the gradient color
     scale_fill_gradient(low="blue", high="red")

# One ellipse arround all points
faithful %>% ggplot( aes(waiting, eruptions)) +  
             geom_point() +
             stat_ellipse()   # One ellipse arround all points
# Ellipse by groups
p <- ggplot(faithful, aes(waiting, eruptions, color = eruptions > 3)) +      geom_point()
p
p + stat_ellipse()
p + stat_ellipse(type = "norm") # Change the type of ellipses: possible values are "t", "norm", "euclid"

# scatter plot of x and y variables
set.seed(1234)
x <- c(rnorm(500, mean = -1), rnorm(500, mean = 1.5))
y <- c(rnorm(500, mean = 1), rnorm(500, mean = 1.7))
group <- as.factor(rep(c(1,2), each=500))
df <- data.frame(x, y, group)
scatterPlot <- ggplot(df,aes(x, y, color=group)) + # color by groups
               geom_point() + 
               scale_color_manual(values = c('#999999','#E69F00')) + 
               theme(legend.position=c(0,1), legend.justification=c(0,1))
scatterPlot

xdensity <- ggplot(df, aes(x, fill=group)) + 
            geom_density(alpha=.5)         +             # Marginal density plot of x (top panel)
            scale_fill_manual(values = c('#999999','#E69F00')) + 
            theme(legend.position = "none")
xdensity

ydensity <- ggplot(df, aes(y, fill=group)) + 
            geom_density(alpha=.5) +                   # Marginal density plot of y (right panel)
            scale_fill_manual(values = c('#999999','#E69F00')) + 
            theme(legend.position = "none")
ydensity

library("gridExtra")
blankPlot <- ggplot() + geom_blank(aes(1,1))
grid.arrange( xdensity, 
              blankPlot, 
              scatterPlot, 
              ydensity,  
              ncol=2, nrow=2, 
              widths=c(4, 1.4), heights=c(1.4, 4))

#### bubble charts

data("mtcars")
df <- mtcars
# QQ-plots in R: Quantile-Quantile Plots-Quick Start Guide 
df$cyl <- as.factor(df$cyl)
ggplot(df, aes(x = wt, y = disp)) +
  geom_point(aes(color = cyl, size = qsec), alpha = 0.5) +
  scale_color_manual(values = c("#AA4371", "#E7B800", "#FC4E07")) +
  scale_size(range = c(1, 13)) + # Adjust the range of points size
  theme_set(theme_bw() +theme(legend.position = "bottom"))

## using plotely
library(plotly)
plot_ly( df, 
                      x    = ~wt, 
                      y    = ~disp,
                      text = ~cyl, 
                      size = ~qsec,
                      color = ~cyl, 
                      sizes = c(10, 50),
                      marker =  list( opacity  = 0.7,   sizemode = "diameter" )
                      ) 

bubbleplot
bubbleplot <- bubbleplot %>% layout
bubbleplot



# ggpubr Key features:
# Wrapper around the ggplot2 package for beginners easy publication-ready plots.
# box plots, bar plots, line plots, and more., arrange and annotate multiple plots on the same page.
# install.packages("ggpubr")   
# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/ggpubr")
library(ggpubr)

# basic
df <- ToothGrowth
df$dose <- as.factor(df$dose)
p <- ggdensity(df, x = "len", fill = "dose", 
               palette = "jco", 
               ggtheme = theme_light(), legend = "top")
p

# facet
facet(p, facet.by = "supp")
facet(p, facet.by = "supp", ncol = 1)

# Divide with "supp" vertical, "dose" horizontal
facet(p,  facet.by = c("supp", "dose"),  short.panel.labs = TRUE)
# short.panel.labs: create short labels by omitting variable names;
# panel.labs: a list of one or two character vectors to modify facet label text. eg panel.labs = list(sex = c(“Male”, “Female”)) for “sex” variable. 
# panel.labs.background:   color, linetype, size: background line color, type and size
# fill: background fill color. eg panel.labs.background = list(color = “blue”, fill = “pink”).
# panel.labs.font: a list e.g.: “plain”, “bold”, “italic”, “bold.italic”) 
# and the color (e.g.: “red”) , orientation angle , panel.labs.font.x and panel.labs.font.y ,in x direction and y direction, respectively.


# gridExtra R package, 
gridExtra::grid.arrange() # but no attempt at aligning the plot panels
gridExtra::arrangeGrob()  # to arrange multiple ggplots on one page
gridExtra::marrangeGrob() # for arranging multiple ggplots over multiple pages.
# cowplot package::plot_grid()  argument alignbut doesn’t contain any solution for multi-pages layout. 
# ggpubr::ggarrange() #  wrapper on plot_grid() to arrange multiple ggplots over multiple pages.

data("ToothGrowth")
head(ToothGrowth)
data("mtcars")
mtcars$name <- rownames(mtcars)
mtcars$cyl <- as.factor(mtcars$cyl)
# Create a box plot and a dot plot:
bp <- ggboxplot(ToothGrowth, x = "dose", y = "len", color = "dose", palette = "jco")
dp  <- ggdotplot(ToothGrowth, x = "dose", y = "len", color = "dose", palette = "jco", binwidth = 1)

# Bar plot (bp)
bp <- ggbarplot(mtcars, x = "name", y = "mpg",
                fill = "cyl",               # change fill color by cyl
                color = "white",            # Set bar border colors to white
                palette = "jco",            # jco journal color palett. see ?ggpar
                sort.val = "asc",           # Sort the value in ascending order
                sort.by.groups = TRUE,      # Sort inside each group
                x.text.angle = 90           # Rotate vertically x axis texts
              )

bp + font("x.text", size = 8)

# Scatter plots (sp)
xxxsp <- ggscatter(mtcars, 
                x = "wt", 
                y = "mpg",
                add = "reg.line",               # Add regression line
                conf.int = TRUE,                # Add confidence interval
                color = "cyl", 
                palette = "jco",                # Color by groups "cyl"
                shape = "cyl"                   # Change point shape by groups "cyl"
              ) +
        stat_cor(aes(color = cyl), label.x = 3)       # Add correlation coefficient
sp




cowplot::plot_grid(bp, dp, bp + rremove("x.text"), 
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)


gridExtra::grid.arrange(bp, dp, bp + rremove("x.text"), 
                     # no fn   labels = c("A", "B", "C"),         
             ncol = 2, nrow = 2)



ggpubr::ggarrange(bp, dp, 
                  bp + rremove("x.text"), 
                  labels = c("A", "B", "C"),
                  ncol = 2, nrow = 2) 

figure <- ggarrange(dp, bp + font("x.text", size = 10), ncol = 1, nrow = 2)  
annotate_figure(figure,
                top = text_grob("Visualizing mpg", color = "red", face = "bold", size = 14),
                bottom = text_grob("Data source: \n mtcars data set", color = "blue",
                                   hjust = 1, x = 1, face = "italic", size = 10),
                left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
                right = "I'm done, thanks :-)!",
                fig.lab = "Figure 1", fig.lab.face = "bold"
)



data("mtcars")

dfm      <- mtcars
dfm$cyl  <- as.factor(dfm$cyl)  # Convert the cyl variable to a factor
dfm$name <- rownames(dfm)       # Add the name colums
head(   dfm[  , c("name", "wt", "mpg", "cyl") ]    )  # Inspect the data

# Calculate the z-score of the mpg data
dfm$mpg_z   <- ( dfm$mpg - mean(dfm$mpg) ) / sd( dfm$mpg )
dfm$mpg_grp <- factor( ifelse( dfm$mpg_z < 0, "low",  "high"),   levels = c("low", "high"))

ggpubr::ggbarplot(dfm, x = "name", y = "mpg_z",
          fill = "mpg_grp",           # change fill color by mpg_level
          color = "white",            # Set bar border colors to white
          palette = "jco",            # jco journal color palett. see ?ggpar
          sort.val = "desc",          # Sort the value in descending order
          sort.by.groups = FALSE,     # Don't sort inside each group
          x.text.angle = 90,          # Rotate vertically x axis texts
          ylab = "MPG z-score",
          legend.title = "MPG Group",
          rotate = TRUE,
          ggtheme = theme_minimal()
       )

# lollipup

ggpubr::ggdotchart(dfm, 
           x = "name", 
           y = "mpg",
           color = "cyl",                                # Color by groups
           palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
           sorting = "descending",                       # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           rotate = TRUE,                                # Rotate vertically
           group = "cyl",                                # Order by groups
           dot.size = 6,                                 # Large dot size
           label = round(dfm$mpg),                        # Add mpg values as dot labels
           font.label = list(color = "white", 
                             size  = 9, 
                             vjust = 0.5),               # Adjust label parameters
           ggtheme = theme_pubr()                        # ggplot2 theme
         )


# Deviation graph:   Use y = “mpg_z” and  Change segment color and size : add.params = list(color = “lightgray”, size = 2)
ggpubr::ggdotchart(dfm, x = "name", y = "mpg_z",
           color = "cyl",                                # Color by groups
           palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
           sorting = "descending",                       # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           add.params = list(color = "lightgray", size = 2), # Change segment color and size
           group = "cyl",                                # Order by groups
           dot.size = 6,                                 # Large dot size
           label = round(dfm$mpg_z,1),                                   # Add mpg values as dot labels
           font.label = list(color = "white", size = 9,  vjust = 0.5),   # Adjust label parameters
           # rotate = TRUE,                              # rotate axis
           ggtheme = theme_pubr()                        # ggplot2 theme
        )    +
       geom_hline(yintercept = 0, linetype = 2, color = "lightgray")

# Cleveland’s dot plot  # Color y text by groups. Use y.text.col = TRUE.
ggpubr::ggdotchart(dfm, x = "name", y = "mpg",
           color = "cyl",                                # Color by groups
           palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
           sorting = "descending",                       # Sort value in descending order
           rotate = TRUE,                                # Rotate vertically
           dot.size = 4,                                 # Large dot size
           y.text.col = TRUE,                            # Color y text by groups
           ggtheme = theme_pubr()                        # ggplot2 theme
          )  +
          theme_cleveland()                              # Add dashed grids

# bar plot , 
data(gene_citation)
head(gene_citation)
ggbarplot(gene_citation, 
          x    = "gene", 
          y    = "citation_index",
          fill = "lightgray", 
          xlab = "Gene name", 
          ylab = "Citation index",
          sort.val = "desc",      # Sort in descending order
          top      = 20,          # select top 20 most citated genes
          x.text.angle = 45       # x axis text rotation angle
        )


# Some key genes of interest to be highlighted
key.gns <- c("MYC", "PRDM1", "CD69", "IRF4", "CASP3",
             "BCL2L1", "MYB",  "BACH2", "BIM1",  "PTEN",
             "KRAS", "FOXP1", "IGF1R", "KLF4", "CDK6", "CCND2",
             "IGF1", "TNFAIP3", "SMAD3", "SMAD7",
             "BMPR2", "RB1", "IGF2R", "ARNT")

# Histogram distribution
gghistogram(gene_citation,   # or
# ggdensity(gene_citation, 
            x = "citation_index", 
            y = "..count..",
            xlab = "Number of citation",
            ylab = "Number of genes",
            binwidth = 5, 
            fill = "lightgray", 
            color = "black",
            label = "gene", 
            label.select = key.gns, repel = TRUE,
            font.label = list(color= "citation_index"),
            xticks.by = 20, # Break x ticks by 20
            gradient.cols = c("blue", "red"),
            legend = c(0.7, 0.6),                                 
            legend.title = ""       # Hide legend title
        
            )


# align plots

library(survival)  # Fit survival curves
fit <- survfit( Surv(time, status) ~ adhere, data = colon )


library(survminer) # Plot survival curves
ggsurv <- ggsurvplot(fit, data = colon, 
                     palette = "jco",                              # jco palette
                     pval = TRUE, pval.coord = c(500, 0.4),        # Add p-value
                     risk.table = TRUE                            # Add risk table
                    )
names(ggsurv)  # ggsurv is a list including the following components: plot: survival curves and table: the risk table plot

ggarrange(ggsurv$plot, ggsurv$table, heights = c(2, 0.7),  ncol = 1, nrow = 2) # not v aligned
ggarrange(ggsurv$plot, ggsurv$table, heights = c(2, 0.7),  ncol = 1, nrow = 2, align = "v")







mypngfile <- download.file("https://upload.wikimedia.org/wikipedia/commons/thumb/e/e4/France_Flag_Map.svg/612px-France_Flag_Map.svg.png", 
                           destfile = "france.png", mode = 'wb') 
img <- png::readPNG('france.png') 
plot(img)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  background_image(img) +   # img read by png:readPNG
  geom_point(aes(color = Species), alpha = 0.6, size = 5) +
  color_palette("jco") +
  theme(legend.position = "top")


# further see http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/


##  3dplots #########
#install.packages(rgl)?
library(rgl)
# Allowed plot include "p", "l", "h", "s", meaning points, lines, segments from z=0, and spheres. 

with(iris, plot3d(Sepal.Length,Sepal.Width, Petal.Length, type="s", col=as.numeric(Species)))#works
iris %>% plot3d(Sepal.Length,  Sepal.Width, Petal.Length, type="s", col=as.numeric(Species) ) # object 'Sepal.Length' not found

# To open a new graphics window, use open3d.
methods(plot3d)
methods(persp3d)
# native OpenGL shapes primitives. Each of the above functions takes arguments x, y and z, again using xyz.coords for flexibility.
# points3d:	adds points
# lines3d:	adds lines
# segments3d:	adds line segments
# triangles3d:	adds triangles
# quads3d:	adds quadrilaterals

# Constructed shapes rgl also has a number of objects
# text3d, texts3d:	adds text
# abclines3d:	adds straight lines to plot (like abline)
# arc3d:	adds spherical arcs or spirals to plot
# planes3d:	adds planes to plot
# clipplanes3d:	add clipping planes to plot
# sprites3d, particles3d:	add sprites (fixed shapes or images) to plot
# spheres3d:	adds spheres
# surface3d, terrain3d:	a surface (as used in persp3d)
# arrow3d:	add an arrow to a scene
# pch3d:	draw base-style plotting symbols
# plotmath3d:	used by text3d for math text

# The following low-level functions control the look of the graph:
# axes3d, axis3d:	add axes to plot
# box3d, bbox3d:	add box around plot
# title3d:	add title to plot
# mtext3d:	add marginal text to plot
# decorate3d:	add multiple “decorations” (scales, etc.) to plot
# aspect3d:	set the aspect ratios for the plot
# bg3d, bgplot3d:	set the background of the scene
# show2d:	show a 2D plot or image in a 3D scene
# legend3d:	set a legend for the scene
# grid3d:	add a reference grid to a graph
# thigmophobe3d:	choose label positions to avoid overlap

triangles3d(cbind(x=rnorm(9), y=rnorm(9), z=rnorm(9)), col = "green")
decorate3d()
bg3d("lightgray")
aspect3d(1,1,1)
# Use the light3d function to specify the position and characteristics of a light. 
#  material3d function, or by arguments to other functions  for matrials

# color	white	vector of surface colors to apply to successive vertices for diffuse light
# alpha	1	transparency: 0 is invisible, 1 is opaque
# lit	TRUE	whether lighting calculations should be done
# ambient	black	color in ambient light
# specular	white	color in specular light
# emission	black	color emitted by the surface
# shininess	50	controls the specular lighting: high values look shiny
# smooth	TRUE	whether shading should be interpolated between vertices
# texture	NULL	optional path to a “texture” bitmap to be displayed on the surface
# front, back	fill	should polygons be filled, or outlined?
#   size	3	size of points in pixels
# lwd	1	width of lines in pixels
# 
# surface3d that take matrices for each vertex coordinate accept texture coordinates as matrices as well, in arguments texture_s and texture_t.
# 
# par3d: Miscellaneous graphical parameters
# The r3dDefaults list and the getr3dDefaults function control defaults i
# These functions generate specific shapes. tetrahedron3d, cube3d, octahedron3d, dodecahedron3d, icosahedron3d:	Platonic solids
# cuboctahedron3d, oh3d:	other solids

cols <- rainbow(7)
layout3d(matrix(1:16, 4,4), heights=c(1,3,1,3))
text3d(0,0,0,"tetrahedron3d"); next3d()
shade3d(tetrahedron3d(col=cols[1])); next3d()
text3d(0,0,0,"cube3d"); next3d()
shade3d(cube3d(col=cols[2])); next3d()
text3d(0,0,0,"octahedron3d"); next3d()
shade3d(octahedron3d(col=cols[3])); next3d()
text3d(0,0,0,"dodecahedron3d"); next3d()
shade3d(dodecahedron3d(col=cols[4])); next3d()
text3d(0,0,0,"icosahedron3d"); next3d()
shade3d(icosahedron3d(col=cols[5])); next3d()
text3d(0,0,0,"cuboctahedron3d"); next3d()
shade3d(cuboctahedron3d(col=cols[6])); next3d()
text3d(0,0,0,"oh3d"); next3d()
shade3d(oh3d(col=cols[7]))


# misc3d package

# contour3d     Uses rgl, standard, or grid graphics to render isosurfaces,
# or three-dimensional contours, computed by the marching
# cubes algorithm.
# 
# image3d       Crude 3d analog of image() using rgl to plot points on a
# three dimensional grid representing values in a three
# dimensional array. Assumes high values are inside and
# uses alpha blending to make outside points more
# transparent.
# 
# parametric3d  Plots a two-parameter surface in three dimensions.  Based
# on Mathematica's Param3D
# 
#   slices3d      Uses tkrplot to create an interactive slice view of three or
#                 four dimensional volume data, such as MRI data.
#"shape3d", "mesh3d" or "shapelist3d".
####################################

# plots libraries

## Time series ###
walmart_sales_weekly <- timetk::walmart_sales_weekly

g1 <- timetk::walmart_sales_weekly %>%
timetk::plot_time_series(
    #.data = walmart_sales_weekly,
    .date_var =  Date,
    .value    = Weekly_Sales,
    .color_var = id,
    .smooth  = TRUE,
    .facet_ncol = 3,
    .interactive = FALSE
  )
g1



g1 <- mtcars %>%
  ggplot(aes(disp, mpg, color = cyl)) +
  geom_line(size=2) + 
  scale_color_continuous(limits=c(0,8)) 
g1 

g1 %>% 
  rayshader::plot_gg(
    height = 3,
    width  = 3.5,
    multicore = TRUE,
    pointcontract = 0.7,
    soliddepth   = -200
  )



## 
library(tidyr, tidyverse, tibble)
volcano_tble <- volcano %>%
  as_tibble(.name_repair = "minimal") %>%
  set_names(stringr::str_c("V", seq_along(names(.) ))) %>%
  tibble::rowid_to_column(var = "x") %>%
  pivot_longer(
    cols = contains("V"),
    names_to = "y",
    values_to = "value"
  ) %>%
  mutate(y =stringr::str_remove(y, "^V") %>% as.numeric() )


volcano_tble
# ggplot visualitsiotn 
g2 <- volcano_tble %>%
  ggplot(aes(x =x , y = y, fill = value )) +
  geom_tile() +
  geom_contour(aes(z = value) , color = "black") +
  scale_x_continuous("X", expand = c(0,0 )) +
  scale_y_continuous("Y", expand = c(0,0 )) +
  scale_fill_gradientn("Z", colours = terrain.colors(10)) +
  coord_fixed()
g2

# rayshader
#library(rayshader)
g2 %>%
  rayshader::plot_gg(
    multicore = TRUE,
    raytrace = TRUE,
    width = 7,
    height =4,
    scale = 300,
    widowsize = c(1400,866),
    zoom = 0.6,
    phi = 30,
    theta = 30
  )  # excellent !!
# Above from https://www.r-bloggers.com/2021/01/how-to-make-3d-plots-in-r/



###############################
# WAFFLE The following functions are implemented:
# waffle:             Make waffle (square pie) charts
# draw_key_pictogram: Legend builder for pictograms
# fa_grep:            Search Font Awesome glyph names for a pattern
# fa_list:            List all Font Awesome glyphs
# geom_pictogram:     Pictogram Geom
# geom_waffle:        Waffle (Square pie chart) Geom
# install_fa_fonts: Install Font Awesome 5 Fonts
# iron: Veritical, left-aligned layout for waffle plots
# scale_label_pictogram: Used with geom_pictogram() to map Font Awesome fonts to labels
# theme_enhance_waffle: Waffle chart theme cruft remover that can be used with any other theme
# Installation
# install.packages("waffle", repos = "https://cinc.rud.is")
# # or
# devtools::install_git("https://git.rud.is/hrbrmstr/waffle.git")

##  Basic example ##  https://github.com/hrbrmstr/waffle

library(waffle)
library(magrittr)
library(hrbrthemes)
library(ggplot2)
library(dplyr)


data.frame(
  parts = factor(rep(month.abb[1:3], 3), levels=month.abb[1:3]),
  vals = c(10, 20, 30, 6, 14, 40, 30, 20, 10),
  col = rep(c("blue", "black", "red"), 3),
  fct = c(rep("Thing 1", 3),
          rep("Thing 2", 3),
          rep("Thing 3", 3))
) -> xdf

xdf %>%
  count(parts, wt = vals) %>%
  ggplot(aes(fill = parts, values = n)) +
 # ?geom_waffle(n_rows = 20, size = 0.33, colour = "white", flip = TRUE) +
  scale_fill_manual(
    name = NULL,
    values = c("#a40000", "#c68958", "#ae6056"),
    labels = c("Fruit", "Sammich", "Pizza")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() 

##### done
xdf %>%
  count(parts, wt = vals) %>%
  ggplot(aes(label = parts, values = n)) +
  geom_pictogram(n_rows = 10, aes(colour = parts), flip = TRUE, make_proportional = TRUE) +
  scale_color_manual(
    name = NULL,
    values = c("#a40000", "#c68958", "#ae6056"),
    labels = c("Fruit", "Sammich", "Pizza")
  ) +
  scale_label_pictogram(
    name = NULL,
    values = c("apple-alt", "bread-slice", "pizza-slice"),
    labels = c("Fruit", "Sammich", "Pizza")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  theme(legend.key.height = unit(2.25, "line")) +
  theme(legend.text = element_text(size = 10, hjust = 0, vjust = 0.75))

xdf %>%
  count(parts, wt = vals) %>%
  ggplot(aes(label = parts, values = n)) +
  geom_pictogram(
    n_rows = 20, size = 6, aes(colour = parts), flip = TRUE,
    family = "FontAwesome5Brands-Regular"
  ) +
  scale_color_manual(
    name = NULL,
    values = c("#073f9c", "black", "#f34323"),
    labels = c("BitBucket", "GitHub", "Other")
  ) +
  scale_label_pictogram(
    name = NULL,
    values = c("bitbucket", "github", "git-alt"),
    labels = c("BitBucket", "GitHub", "Other")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  theme(legend.text = element_text(hjust = 0, vjust = 1))

#### Geoms!

library(hrbrthemes)
library(waffle)
library(tidyverse)

tibble(
  parts = factor(rep(month.abb[1:3], 3), levels=month.abb[1:3]),
  values = c(10, 20, 30, 6, 14, 40, 30, 20, 10),
  fct = c(rep("Thing 1", 3), rep("Thing 2", 3), rep("Thing 3", 3))
) -> xdf

ggplot(xdf, aes(fill=parts, values=values)) +
  geom_waffle(color = "white", size=1.125, n_rows = 6) +
  facet_wrap(~fct, ncol=1) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  ggthemes::scale_fill_tableau(name=NULL) +
  coord_equal() +
  labs(
    title = "Faceted Waffle Geoms"
  ) +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle()


########  Waffle Bar Charts with scales! ######
Sys.time()
#remotes::install_github("liamgilbey/ggwaffle") # That package has a working geom_waffle
#remotes::install_github("hrbrmstr/waffle")
library(waffle)


# also working 
# install.packages("waffle", repos = "https://cinc.rud.is")
library(tidyverse)
library(waffle)
library(ggthemes)

storms %>%  filter(year >= 2010) %>%  count(year, status)   ->   storms_df

ggplot(storms_df, aes(fill = status, values = n)) +
  waffle::geom_waffle(color = "white", size = .25, rows = 10, flip = TRUE) +
  facet_wrap(~year, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  ggthemes::scale_fill_tableau(name=NULL) +
  coord_equal() +
  labs(
    title = "Faceted Waffle Bar Chart",
    subtitle = "Created by Anil Goyal",
    x = "Year",
    y = "Count"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
  guides(fill = guide_legend(reverse = TRUE))



storms %>% filter(year >= 2010) %>% 
           count(year, status) -> storms_df

ggplot(storms_df, aes(fill = status, values = n)) +
  ggwaffle::geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(~year, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  ggthemes::scale_fill_tableau(name=NULL) +
  coord_equal() +
  labs(
    title = "Faceted Waffle Bar Chart",
    subtitle = "{dplyr} storms data",
    x = "Year",
    y = "Count"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
  guides(fill = guide_legend(reverse = TRUE))

ggplot(data = waffle_iron(mpg, aes_d(group = class)), aes(x, y, fill = group)) +
  geom_waffle() +
  coord_equal()
####################################

(spec <- table(iris$Species))
waffle::waffle(spec)

waffle::waffle(spec, rows = 3, legend_pos = "bottom")
waffle::waffle(spec, rows = 15, colors = c("lightgrey", "darkgrey", "red") )
waffle::waffle(spec / 10, rows = 5, xlab = "1 square = 10 flowers")   # but annotate 1 sq = 10 units!

waffle::iron( waffle::waffle(spec / 5, rows = 5, title = "iron() combines waffles"),
              waffle::waffle(spec / 10, rows = 5, xlab = "1 square = 10 flowers")  )  # combiing waffles with waffle::iron
?waffle::iron()
parts <- c(80, 30, 20, 10)
w1 <- waffle(parts, rows=8)
w2 <- waffle(parts, rows=8)
w3 <- waffle(parts, rows=8)

iron(w1, w2, w3)   # print charts vertically
fa_list()

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


##################################33

savings <- c(  `Mortgage\n($84,911)` = 84911, 
               `Auto and\ntuition loans\n($14,414)` = 14414,
               `Home equity loans\n($10,062)` = 10062, 
               `Credit Cards\n($8,565)` = 8565    )

waffle(       savings / 392,  
              rows = 7, size = 0.5, legend_pos = "bottom",
              colors = c("#c7d4b6", "#a3aabd", "#a0d0de", "#97b5cf")
)



# install.packages("devtools")
#devtools::install_github("liamgilbey/ggwaffle")
library(ggwaffle)

waffle_data <- waffle_iron(mpg, aes_d(group = class))
waffle_data
ggplot() +   geom_waffle(data =  waffle_data, aes(x, y, fill = group))

# Icons The best way to implement icons into waffle charts is to use Guangchuang YU’s emojifont package.

library(emojifont)  
library(dplyr)

fa_list()

iris$Species <- as.character(iris$Species)
waffle_data  <- waffle_iron(iris, aes_d(group = Species)) %>% 
                 mutate(label = fontawesome('fa-twitter'))

ggplot(waffle_data, aes(x, y, colour = group)) + 
  geom_text(aes(label=label), family='fontawesome-webfont', size=4) +
  coord_equal() + 
  scale_colour_waffle() + 
  theme_waffle()  


library(waffle)
parts <- c(80, 30, 20, 10)
waffle::waffle(parts, rows = 8)

parts <- data.frame( names =  LETTERS[1:4]     ,
                      vals = c(80, 30, 20, 10)          )
parts
waffle::waffle(parts, rows = 8)


# Slightly more complex example
parts <- c(   `Un-breached\nUS Population` = (318 - 11 - 79), `   Premera` = 11, `Anthem` = 79)

parts
waffle(   parts,     rows = 8,   size = 1,  
                     colors = c("#969696", "#1879bf", "#009bda"), legend_pos = "bottom"
      )

#Health records breaches as fraction of US Population ; One square == 1m ppl

waffle(  parts / 10, rows = 3,    colors = c("#969696", "#1879bf", "#009bda") )


# Other Usage
#library(magrittr) library(ggplot2) library(dplyr)
library(tidyverse)
library(hrbrthemes)
library(waffle)
packageVersion("waffle")# current verison


## [1] '1.0.1'
xdf <- data.frame(   parts = factor(rep(month.abb[1:3], 3), levels=month.abb[1:3]),
                     vals = c(10, 20, 30, 6, 14, 40, 30, 20, 10),
                     col = rep(c("blue", "black", "red"), 3),
                     fct = c(rep("Thing 1", 3),
                     rep("Thing 2", 3),
                     rep("Thing 3", 3))  ) 
xdf
xdf %>%
  count(parts, wt = vals) %>%
  ggplot(aes(fill = parts, values = n)) +
  geom_waffle(n_rows = 20, size = 0.33, colour = "white", flip = TRUE) +
  scale_fill_manual(
    name = NULL,
    values = c("#a40000", "#c68958", "#ae6056"),
    labels = c("Fruit", "Sammich", "Pizza")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="") + theme_enhance_waffle()


xdf %>%
  count(parts, wt = vals) %>%
  ggplot(aes(label = parts, values = n)) +
  geom_pictogram(n_rows = 10, aes(colour = parts), flip = TRUE, make_proportional = TRUE) +
  scale_color_manual(
    name = NULL,
    values = c("#a40000", "#c68958", "#ae6056"),
    labels = c("Fruit", "Sammich", "Pizza")
  ) +
  scale_label_pictogram(
    name = NULL,
    values = c("apple-alt", "bread-slice", "pizza-slice"),
    labels = c("Fruit", "Sammich", "Pizza")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  theme(legend.key.height = unit(2.25, "line")) +
  theme(legend.text = element_text(size = 10, hjust = 0, vjust = 0.75))


xdf %>%
  count(parts, wt = vals) %>%
  ggplot(aes(label = parts, values = n)) +
  geom_pictogram(
    n_rows = 20, size = 6, aes(colour = parts), flip = TRUE,
    family = "FontAwesome5Brands-Regular"
  ) +
  scale_color_manual(
    name = NULL,
    values = c("#073f9c", "black", "#f34323"),
    labels = c("BitBucket", "GitHub", "Other")
  ) +
  scale_label_pictogram(
    name = NULL,
    values = c("bitbucket", "github", "git-alt"),
    labels = c("BitBucket", "GitHub", "Other")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  theme(legend.text = element_text(hjust = 0, vjust = 1))


library(hrbrthemes)
library(waffle)
library(tidyverse)

tibble(
  parts = factor(rep(month.abb[1:3], 3), levels=month.abb[1:3]),
  values = c(10, 20, 30, 6, 14, 40, 30, 20, 10),
  fct = c(rep("Thing 1", 3), rep("Thing 2", 3), rep("Thing 3", 3))
) -> xdf

ggplot(xdf, aes(fill=parts, values=values)) +
  geom_waffle(color = "white", size=1.125, n_rows = 6) +
  facet_wrap(~fct, ncol=1) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  ggthemes::scale_fill_tableau(name=NULL) +
  coord_equal() +
  labs(
    title = "Faceted Waffle Geoms"
  ) +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle()


# Waffle Bar Charts with scales!
library(dplyr)
library(waffle)

storms %>% 
  filter(year >= 2010) %>% 
  count(year, status) -> storms_df

ggplot(storms_df, aes(fill = status, values = n)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(~year, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  ggthemes::scale_fill_tableau(name=NULL) +
  coord_equal() +
  labs(
    title = "Faceted Waffle Bar Chart",
    subtitle = "{dplyr} storms data",
    x = "Year",
    y = "Count"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
  guides(fill = guide_legend(reverse = TRUE))


###### chatterplot #######
library(bigrquery)  # google authentication
project <- "YOUR_PROJECT"    # put your project ID here

# get row count of full bigquery table  
sql <- "SELECT COUNT(*)   FROM `bigquery-public-data.hacker_news.comments`" 
        query_exec(sql, project = project,  use_legacy_sql = F)

# randomly sample 0.1% of 8 million+ records
sql <- "SELECT * 
        FROM `bigquery-public-data.hacker_news.comments`
        WHERE RAND() <= .001"

hn_comments <- query_exec(sql, project = project,   use_legacy_sql = F,   max_pages = Inf)
hn_comments %>% str(nchar.max = 20)# inspect size & data types of results
hn_comments$text[1] # print first full text comment


### misc #############################3
library(dplyr)
library(ggplot2)

#' @title Convert line sizes measured as points to ggplot line sizes
#' @description Converts line sizes measured as points (as given by most programs such as Adobe Illustrator etc.) to ggplot font sizes
#' @param x numeric vector giving the lines sizes in points
#' @return Returns a numeric vector of lenght \code{x} of ggplot line sizes
#' @keywords internal
#' @export
#'
LS <- function(x) x/2.13

#' @title Round values preserving total sums
#' @description The function rounds values preserving total sums
#' @param x numeric vector of values to be rounded
#' @param digits integer indicating the number of decimal places. See \code{\link[base]{round}}.
#' @return Returns a numeric vector.
#' @author The function is written as a communal effort. Main authors are \href{https://stackoverflow.com/questions/32544646/round-vector-of-numerics-to-integer-while-preserving-their-sum}{josliber} and \href{https://www.r-bloggers.com/round-values-while-preserve-their-rounded-sum-in-r/}{BioStatMatt}.
#' @keywords internal
#' @family waffle
#' @export

round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up }  # from https://www.r-bloggers.com/round-values-while-preserve-their-rounded-sum-in-r/

#' @title Prepare data for waffle plots
#' @description Manipulates a data frame ready for plotting with the \code{\link{waffle_chart}} function.
#' @param dt data frame containing the data which should be transformed
#' @param fill character specifying the column name which should be used as fill for the waffle plot.
#' @param value character specifying the column name which contains values of the \code{fill} variable.
#' @param composition logical indicating whether a compositional waffle (i.e. fill adds up to 100\%) should be created. If \code{FALSE}, waffle cells will be scaled to \code{max_value} and missing cells filled with an "empty" category.
#' @param max_value numerical giving the value to which waffle cells should be scaled to, if \code{composition = FALSE}.
#' @param digits integer indicating the number of decimal places to be used in rounding of the waffle cells. 
#' @return returns a \link[tibble]{tibble} data frame containing the minimum and maximum extent of each \code{fill} level.
#' @author Mikko Vihtakari 
#' @keywords internal
#' @family waffle
#' @import dplyr
#' @export

waffleize <- function(dt, fill, value, composition = TRUE, max_value = NULL, digits = 3) {
  x <- dt[c(fill, value)]
  names(x) <- c("variable", "value")
  if(composition) {    x$value <- round_preserve_sum(10^digits*x$value/sum(x$value))  } else {
    if(is.null(max_value)) stop("max_value has to be given, if composition = FALSE")
    if(max_value < sum(x$value)) stop("max_value has to be larger than the sum of 'value' column. Use composition = TRUE, if you want a compositional waffle chart")
    x <- rbind(x, data.frame(variable = "empty", value = max_value - sum(x$value)))
    x$value <- round_preserve_sum(10^digits*x$value/max_value)
  }
  
  if(!is.factor(x$variable)) x$variable <- factor(x$variable, levels = c(sort(unique(x$variable)[unique(x$variable) != "empty"]), "empty"))
  x <- x[order(x$variable),]
  #tmp <- data.frame(X = 1:100, ymin = rep(c(0, (1:9)*10), each = 100), ymax = rep((1:10)*10, each = 100), variable = rep(dt[[fill]], dt[[value]]))
  tmp <- data.frame(X = 1:10^(digits-1), ymin = rep(c(0, (1:9)*10^(digits-2)), each = 10^(digits-1)), ymax = rep((1:10)*10^(digits-2), each = 10^(digits-1)), variable = rep(x$variable, x$value))
  out <- tmp %>% group_by(variable, ymin, ymax) %>% summarise(xmin = min(X)-1, xmax = max(X))
## Remove the empty category
  out[out$variable != "empty",]
}

#' @title Plot waffle charts
#' @description The function uses \link[ggplot2]{ggplot2} to create waffle charts from data.
#' @param data data frame to be plotted
#' @param fill character specifying the column name which should be used as fill for the waffle plot.
#' @param value character specifying the column name which contains values of the \code{fill} variable. Will be used to fill the waffle cells.
#' @param facet character specifying the column name which should be used to \code{\link[ggplot2]{facet_wrap}} waffle charts.
#' @param ncol number of columns to be used in facetting. See \code{\link[ggplot2]{facet_wrap}}.
#' @param composition logical indicating whether a compositional waffle (i.e. fill adds up to 100\%) should be created. If \code{FALSE}, waffle cells will be scaled to \code{max_value} and missing cells filled with an "empty" category.
#' @param max_value numerical giving the value to which waffle cells should be scaled to, if \code{composition = FALSE}.
#' @param digits integer indicating the number of decimal places to be used in rounding of the waffle cells. The value 3 indicates percentages, while 4 permilles. 
#' @param fill_colors named character vector giving the colors for \code{fill} levels. See \code{\link[ggplot2]{scale_fill_manual}}.
#' @param fill_title character giving the title for the color legend.
#' @param base_size numeric giving the base size for the plot. See \code{\link[ggplot2]{theme_void}}.
#' @param legend.position character specifying the position of the legend. See \code{\link[ggplot2]{ggtheme}}.
#' @details The waffle charts are read from left to right (like text) and from bottom upwards (like water glass). The cells indicate 1\% of the maximum value (100% if \code{composition = TRUE} else \code{max_value}). The cells are divided vertically to fractions specifies by the \code{digits} argument. 
#' @return Returns a \link[ggplot2]{ggplot2} waffle plot
#' @import ggplot2 dplyr
#' @family waffle
#' @author Mikko Vihtakari with code ideas from \href{https://github.com/hrbrmstr/waffle}{hrbrmstr} and \href{https://github.com/liamgilbey/ggwaffle}{Liam Gilbey}
#' @export

# data = dt; fill = "variable"; value = "value"; facet = NULL; composition = TRUE; max_value = NULL; digits = 3; fill_colors = NULL; fill_title = NULL; ncol = NULL; base_size = 12; legend.position = "bottom"
waffle_chart <- function(data, fill, value = "value", facet = NULL, composition = TRUE, max_value = NULL, digits = 3, fill_colors = NULL, fill_title = NULL, ncol = NULL, base_size = 12, legend.position = "bottom") {
grid_data <- data.frame(xmin = c(0,(1:9)*10^(digits-2)), xmax = (1:10)*10^(digits-2), ymin = rep(c(0,(1:9)*10^(digits-2)), each = 10^(digits-2)), ymax = rep((1:10)*10^(digits-2), each = 10^(digits-2)))
  if(is.null(facet)) { ## No facetting
    if(any(duplicated(data[[fill]]))) stop("data contains duplicated entries in fill column. Use the facet argument or summarize data before plotting.")
    waffle_data <- waffleize(dt = data, fill = fill, value = value, composition = composition, max_value = max_value, digits = digits)
    p <- ggplot() + 
      geom_rect(data = waffle_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = variable)) + 
      geom_rect(data = grid_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = NA, color = "white") +
      coord_equal(expand = FALSE) + 
      theme_void()
  } else { ## Facetting
    waffle_data <- data %>% group_by_(facet) %>% do(waffleize(dt = ., fill = fill, value = value, composition = composition, max_value = max_value, digits = digits))
    p <- ggplot() + 
      geom_rect(data = waffle_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = variable)) + 
      geom_rect(data = grid_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = NA, color = "white") +
      facet_wrap(facet, ncol = ncol) + 
      coord_equal(expand = FALSE) + 
      theme_void()
}
if(is.null(fill_title)) fill_title <- "Variable"
  if(!is.null(fill_colors)) {
    p <- p + scale_fill_manual(name = fill_title, values = fill_colors)
  } else {
    p <- p + scale_fill_viridis_d(name = fill_title)
  }   ## Final theme manipulation
p
p <- p + theme(
    legend.position = legend.position,
    aspect.ratio = 1, 
    panel.border = element_rect(color = "black", size = LS(1), fill = NA),
    strip.background = element_rect(fill = alpha("white", 0.4), color = NA),
    strip.text.x = element_text(size = base_size*0.8, margin = margin(t = 2, r = 0, b = 3, l = 0, unit = "pt")),
    plot.title = element_text(size = base_size, hjust = 0.5, face = 2),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.title = element_text(size = base_size),
    legend.text = element_text(size = base_size),
    plot.background = element_blank(),
    panel.spacing = unit(0.2, units = "line"),
    legend.box.margin = margin(t = 0, r = 0, b = 3, l = 0, unit = "pt"),
    plot.margin = unit(c(0.2, 0.5, 0.1, 0.1), units = "line")
  )
## Return the plot
  
  
}

# Manipulate the dataset first to make sure that there are no replicate 
# entries of factors used for the waffles

data("mtcars")

mtcars$gear_vs <- paste(mtcars$gear, mtcars$vs, sep = "-")
mtcars$carb <- factor(mtcars$carb)
x <- mtcars %>% group_by(gear_vs, carb) %>% summarise(value = sum(hp))

waffle_chart(x, fill = "carb", facet = "gear_vs", value = "value")
## You can also scale the waffles to a maximum hp in gear_vs

y <- x %>% group_by(gear_vs) %>% summarise(value = sum(value))

waffle_chart(x, fill = "carb", facet = "gear_vs", value = "value", composition = FALSE, max_value = max(y$value))



# personograph plots (also called Kuiper-Marshall plots) 
# older subset 
library(personograph)   ## Loading required package: grImport , grid, XML

n <- 2000
probl_wo_alc <- 18 / n
probl_w_alc <- 1 / n

data <- list(first = probl_wo_alc, 
             second = 1-probl_wo_alc)
personograph(data,  
             colors = list(first = "black", second = "#efefef"),
             fig.title = "18 of 2000 people with health problems",
             draw.legend = FALSE, n.icons = n, dimensions = c(20, 100), 
             plot.width = 0.97)

data_2 <- list(first = probl_wo_alc, 
               second = probl_w_alc, 
               third = 1-(probl_wo_alc+probl_w_alc) )

personograph(data_2, 
             colors = list(first = "black", second = "red", third = "#efefef"),
             fig.title = "About 1 additional case with half a litre of beer per day",
             draw.legend = FALSE, n.icons = n, dimensions = c(20, 100),
             plot.width = 0.97)


#  definitions of sensitivity and  Understanding ROC Curves
# Sensitivity or True Positive Rate (TPR): the proportion of actual (= true) positives (TP) that are correctly identified as such (e.g. the percentage of sick people who are correctly identified as having (had) the condition).
# Specificity or True Negative Rate (TNR): the proportion of actual (= true) negatives (TN) that are correctly identified as such (e.g. the percentage of healthy people who are correctly identified as not having (had) the condition).
# Positive predictive value (PPV): the probability that subjects with a positive screening test truly have (had) the disease.
# Negative predictive value (NPV): the probability that subjects with a negative screening test truly don’t have (have not had) the disease.
# all four figures are conditional probabilities,

library(personograph)
colorlist <- list(FN = "darkred", TP = "red", FP = "darkgreen", TN = "green")
TPR <- 0.95 # true positive rate = sensitivity
TNR <- 0.95 # true negative rate = specificity
IR <- 0.05 # infection rate
data <- list(FN = (1-TPR)*IR, TP = TPR*IR, FP = (1-TNR)*(1-IR), TN = TNR*(1-IR))
NPV <- round(100 * data$TN / (data$TN + data$FN), 1) # negative predictive value
PPV <- round(100 * data$TP / (data$TP + data$FP), 1) # positive predictive value
personograph(data, colors = colorlist, fig.title = paste0("PPV = ", PPV, "%, NPV = ", NPV, "%"), n.icons = 500, dimensions = c(20, 25))

IR <- 0.25 # infection rate
data <- list(FN = (1-TPR)*IR, TP = TPR*IR, FP = (1-TNR)*(1-IR), TN = TNR*(1-IR))
NPV <- round(100 * data$TN / (data$TN + data$FN), 1) # negative predictive value
PPV <- round(100 * data$TP / (data$TP + data$FP), 1) # positive predictive value
personograph(data, colors = colorlist, fig.title = paste0("PPV = ", PPV, "%, NPV = ", NPV, "%"), n.icons = 500, dimensions = c(20, 25))



# Efficacy 
# Primary efficacy analysis demonstrates BNT162b2 to be 95% effective against COVID-19 beginning 28 days after the first dose;
# 170 confirmed cases of COVID-19 were evaluated, with 162 observed in the placebo group versus 8 in the vaccine group
# To get to the 95%-number the following calculation was performed:   (1 - 8/162) * 100 = 95%
# So the 95% is the *relative risk-reduction* in infections, and no *absolute probability* of not getting infected despite being vaccinated!
# relative vs. absolute risk reductions 
# illustrating the confirmed COVID-19 cases in the placebo (= control) group for a better manageable group size of 2,500…

library(personograph) # first install from CRAN
n <- 2500
inf_wo_vac <- 20 / n
data <- list(first = inf_wo_vac, second = 1-inf_wo_vac)
personograph(data,  colors = list(first = "red", second = "lightgrey"),
             fig.title = "20 of 2500 infected without vaccine",
             draw.legend = FALSE, n.icons = n, dimensions = c(25, 100), 
             plot.width = 0.97)
# and now for the vaccine group of the same size:
inf_w_vac <- 1 / n
data <- list(first = inf_w_vac, second = 1-inf_w_vac)
personograph(data,  colors = list(first = "red", second = "lightgrey"),
             fig.title = "1 of 2500 infected despite of vaccine",
             draw.legend = FALSE, n.icons = n, dimensions = c(25, 100), 
             plot.width = 0.97)
# The main problem is that vaccine studies cannot directly measure what we really want to know: the effectiveness of the vaccine in the real world, i.e. how well it protects us from contracting the disease. 
# Therefore they use efficacy as a proxy instead, i.e. relative risk-reduction of infections in the two study groups.


################################ 
# shenkey by panta rhei everything flows
library(PantaRhei)
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
  ~ID,     ~label,   ~x,   ~y,      ~dir,     ~label_pos,
  "in",    "Input",   0,   "0",     "right",   "left",
  "out",   "Output",  4,   "in",    "right",   "right",
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
colors <- tibble::tribble( ~substance, ~color,
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







###############
###############
###############
###############
###############
###############
###############
# Plot Differences in Two Measurements-Bland-Altman Plot in R
# Jhon  Tukey mean-difference plot or
# Bland–Altman plot (difference plot) in analytical chemistry or biomedicine i
# Bland–Altman plot compare two clinical measurements each of which produced some error in their measures
# Bland–Altman plot showing likely proportional bias
# Bland–Altman plots allow identification of any systematic difference between the measurements (i.e., fixed bias) or possible outliers. 
# sample data
data <- data.frame(A=c(6, 5, 3, 5, 6, 6, 5, 4, 7, 8, 9, 10, 11, 13, 10, 4, 15, 8, 22, 5),
                   B=c(5, 4, 3, 5, 5, 6, 8, 6, 4, 7, 7, 11, 13, 5, 10, 11, 14, 8, 9, 4))

# Calculate mean and diffrence (MAD) 
data$avg <- rowMeans(data)
data$diff <- data$A - data$B
# Step 3: Calculate the Confidence Interval
mean_diff <- mean(data$diff)
# find lower 95% confidence interval limits
lower <- mean_diff - 1.96*sd(data$diff)
#find upper 95% confidence interval limits
upper <- mean_diff + 1.96*sd(data$diff)
# Step 4: Create the Bland-Altman Plot

library(ggplot2)
ggplot(data, aes(x = avg, y = diff)) +
  geom_point(size=2) +
  geom_hline(yintercept = mean_diff) +
  geom_hline(yintercept = lower, color = "red", linetype="dashed") +
  geom_hline(yintercept = upper, color = "red", linetype="dashed") +
  ggtitle("Bland-Altman / John  Tukey mean-difference plot  ") +
  ylab("Difference Between Instruments") +
  xlab("Average")+theme_bw()





# R TIPS ---- ggalt: dumbbell plots ----
library(tidyverse)
library(tidyquant)
library(ggalt)
mpg   # DATA ----
mpg_by_year_tbl <- mpg %>% select(hwy, year, model, class) %>%
  pivot_wider(
    names_from   = year,
    values_from  = hwy,
    id_cols      = c(class, model),
    values_fn    = function(x) mean(x, na.rm = TRUE),
    names_prefix = "year_"
  ) %>%
  mutate( model = fct_reorder(model, year_2008) ) %>%    drop_na()

mpg_by_year_tbl

# 2.0 VISUALIZATION (Dumbell Plots) ---- Basic Dumbbell Plot with ggalt ----
g1 <- mpg_by_year_tbl %>%
  #mutate( model = fct_reorder(model, year_2008) ) %>%    drop_na() %>% # already done above
  ggplot2::ggplot(  aes(x    = year_1999, 
                        xend = year_2008, 
                        y = model, 
                        group = model)  ) +
  ggalt::geom_dumbbell( colour      ="#a3c4dc",
                        colour_xend ="#0e668b",
                        size        =4.0,
                        dot_guide   =TRUE,
                        dot_guide_size=0.15,
                        dot_guide_colour = "grey60"
  )

g1

g2 <- g1 +         # * Customize Theme with tidyquant ----
labs(
  title = "Change Vehicle Fuel Economy between 1999 and 2008",
  x="Fuel Economy (MPG)", y = "Vehicle Model"
) +
  theme_tq() +
  theme(
    panel.grid.minor=element_blank(),
    panel.grid.major.y=element_blank(),
    panel.grid.major.x=element_line(),
    axis.ticks=element_blank(),
    panel.border=element_blank()
    
  )

g2


################
################
# Calander plot
#install.packages(calendR)
library(calendR)

# Data
set.seed(2)
data <- rnorm(365)

# Vertical calendar
calendR(year = 2021,
        special.days = data,
        
        low.col = "#FCFFDD",
        special.col = "#00AAAE",
        gradient = TRUE,
        legend.pos = "right",
        orientation = "portrait")

