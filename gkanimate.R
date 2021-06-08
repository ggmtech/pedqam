# load libraries
library(gganimate)
library(tidyverse)
# the data file format is indicated above
data <- read.csv('Data.csv')
# convert date to proper format
data$Ord <- as.Date(data$OrdDat, format='%m/%d/%Y')
# specify the animation length and rate
options(gganimate.nframes = 30)
options(gganimate.fps = 10)
# loop the animation
options(gganimate.end_pause = 0)
# specify the data source and the X and Y variables
ggplot(data, aes(X, Y)) +
  # specify the plot format
  theme(panel.background = element_rect(fill = 'white'))+
  theme(axis.line = element_line()) +
  theme(axis.text = element_blank())+
  theme(axis.ticks = element_blank())+
  theme(axis.title = element_blank()) +
  theme(plot.title = element_text(size = 20)) +
  theme(plot.margin = margin(25, 25, 25, 25)) +
  theme(legend.position = 'none') +
  # create a scatter plot
  geom_point(aes(color = Cat), size = 5) +
  # indicate the fill color scale
  scale_fill_viridis_d(option = "D", begin = 0, end = 1) +
  # apply the fill to the 'Cat' variable
  aes(group = Cat) +
  # animate the plot on the basis of the 'Ord' variable
  transition_time(Ord) +
  # the ease_aes() function
  ease_aes('cubic-in') +
  # title the plot
  labs(title = "'ease_aes(cubic-in)'")
