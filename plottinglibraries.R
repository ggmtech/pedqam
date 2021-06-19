######### plotly interactive #########
# Libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(viridis)
library(hrbrthemes)
library(rayshader)



# geom_point() + scale_x_continuous("Item Visibility", breaks = seq(0,0.35,0.05))+ scale_y_continuous("Item MRP", breaks = seq(0,270,by = 30))
# geom_histogram(binwidth = 2)
# geom_bar(fill = "red")  + coord_flip()
#  geom_boxplot(fill = "red")+ scale_y_continuous("Item Outlet Sales", breaks= seq(0,15000, by=500))+
# geom_area(stat = "bin", bins = 30, fill = "steelblue") + scale_x_continuous(breaks = seq(0,11000,1000))
# heatmap geom_raster(aes(fill = Item_MRP))+
#  scale_x_continuous("Item Visibility", breaks = seq(0,0.35,0.05))+
# scale_y_continuous("Item MRP", breaks = seq(0,270,by = 30))+
#  facet_wrap( ~ Item_Type)
# library(corrgram)  corrgram(train, order=NULL, panel=panel.shade, text.panel=panel.txt,    main="Correlogram") 



# The dataset is provided in the gapminder library
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
  mutate(text = paste("Country: ", country, "\nPopulation (M): ", pop, "\nLife Expectancy: ", lifeExp, "\nGdp per capita: ", gdpPercap, sep="")) %>%
  
  # Classic ggplot
  ggplot( aes(x=gdpPercap, y=lifeExp, size = pop, color = continent, text=text)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1.4, 19), name="Population (M)") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  theme_ipsum() +
  theme(legend.position="none")

# turn ggplot interactive with plotly
pp <- ggplotly(p, tooltip="text")
pp

# save the widget
# library(htmlwidgets)
# saveWidget(pp, file=paste0( getwd(), "/HtmlWidget/ggplotlyBubblechart.html"))


# Add marginal rugs
ggplot(mtcars, aes(x=wt, y=mpg, color=cyl)) +
          geom_point()    + 
          geom_rug()   # Add marginal rugs

# Scatter plot with the 2d density estimation
sp <- ggplot(faithful, aes(x=eruptions, y=waiting))  +  
      geom_point()

sp + geom_density_2d()

sp + stat_density_2d(aes(fill = ..level..), geom="polygon")     # Gradient color

sp + stat_density_2d(aes(fill = ..level..), geom="polygon") +   # Change the gradient color
        scale_fill_gradient(low="blue", high="red")

# One ellipse arround all points
ggplot(faithful, aes(waiting, eruptions))+
        geom_point()+
       stat_ellipse()   # One ellipse arround all points
# Ellipse by groups
p <- ggplot(faithful, aes(waiting, eruptions, color = eruptions > 3))+      geom_point()
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
grid.arrange(xdensity, blankPlot, scatterPlot, ydensity,  ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))


##  3dplots #########
install.packages(rgl)
library(rgl)
# Allowed plot types include "p", "l", "h", "s", meaning points, lines, segments from z=0, and spheres. 

with(iris, plot3d(Sepal.Length, Sepal.Width, Petal.Length,  type="s", col=as.numeric(Species)))  # works

library(magrittr)
iris %>% plot3d(  Sepal.Length,   Sepal.Width,   Petal.Length,   type="s",   col=as.numeric(Species)    ) # object 'Sepal.Length' not found

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
    height =3,
    width = 3.5,
    multicore = TRUE,
    pointcontract = 0.7,
    soliddepth = -200
  )



## 

volcano_tble <- volcano %>%
  as_tibble(.name_repair = "minimal") %>%
  set_names(str_c("V", seq_along(names(.) ))) %>%
  rowid_to_column(var = "x") %>%
  pivot_longer(
    cols = contains("V"),
    names_to = "y",
    values_to = "value"
  ) %>%
  mutate(y =str_remove(y, "^V") %>% as.numeric() )


volcano_tble
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
  
# waffle: Make waffle (square pie) charts
# draw_key_pictogram: Legend builder for pictograms
# fa_grep: Search Font Awesome glyph names for a pattern
# fa_list: List all Font Awesome glyphs
# geom_pictogram: Pictogram Geom
# geom_waffle: Waffle (Square pie chart) Geom
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
library(waffle)

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
  geom_waffle(n_rows = 20, size = 0.33, colour = "white", flip = TRUE) +
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
####################################

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


savings <- c(  `Mortgage\n($84,911)` = 84911, `Auto and\ntuition loans\n($14,414)` = 14414,
               `Home equity loans\n($10,062)` = 10062, `Credit Cards\n($8,565)` = 8565    )
waffle(       savings / 392, rows = 7, size = 0.5, legend_pos = "bottom",
              colors = c("#c7d4b6", "#a3aabd", "#a0d0de", "#97b5cf")
           )
##################################33

# install.packages("devtools")
devtools::install_github("liamgilbey/ggwaffle")
library(ggwaffle)


waffle_data <- waffle_iron(mpg, aes_d(group = class))
waffle_data
ggplot() +   geom_waffle(data =  waffle_data, aes(x, y, fill = group))

# Icons The best way to implement icons into waffle charts is to use Guangchuang YU’s emojifont package.

library(emojifont)  
library(dplyr)

iris$Species <- as.character(iris$Species)
waffle_data  <- waffle_iron(iris, aes_d(group = Species)) %>% mutate(label = fontawesome('fa-twitter'))

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
