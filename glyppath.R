

#install.packages("string2path") # Or # install.packages("devtools")
#devtools::install_github("yutannihilation/string2path")
library(string2path)
library(ggplot2)

systemfonts::system_fonts()  %>% View()# installed fonts

# /System/Library/Fonts/Supplemental/ITFDevanagari.ttc

# to lookup the path.
#d <- string2path("カラテが\n高まる。", "./fonts/ipaexg.ttf")
d <- string2path("A B C", "/System/Library/Fonts/Helvetica.ttc")
d <- string2path("क ख ग", "/System/Library/Fonts/Supplemental/ITFDevanagari.ttc") # good
d <- tibble::rowid_to_column(d)

ggplot(d) +
  geom_path(aes(x, y, group = path_id, colour = factor(glyph_id)), size = 1.5) +
  theme_minimal() +
  coord_equal() +
  theme(legend.position = "top") +
  scale_colour_viridis_d(option = "H")

################################################# Good annimation
library(gganimate)

#d <- string2path("蹴", "./fonts/ipaexg.ttf")
#d <- tibble::rowid_to_column(d)

ggplot(d) +
  geom_path(aes(x, y, group = path_id), size = 2, colour = "purple2", lineend = "round") +
  theme_minimal() +
  coord_equal() +
  transition_reveal(rowid)


string2fill()
# Sorry for my laziness, please replace the font path to the appropriate location in your system...
ttf_file <- "./fonts/iosevka-heavyitalic.ttf"
ttf_file <- "/System/Library/Fonts/Supplemental/ITFDevanagari.ttc"
d <- string2fill("क ख ग", ttf_file) 

ggplot(d) +
  geom_polygon(aes(x, y, group = triangle_id, fill = factor(triangle_id %% 7)), colour = "grey", size = 0.1) +
  theme_minimal() +
  coord_equal() +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option = "H")


string2stroke()
for (w in 1:9 * 0.01) {
  d <- string2stroke("क ख ग", ttf_file, line_width = w)
  
  p <- ggplot(d) +
    geom_polygon(aes(x, y, group = triangle_id, fill = factor(triangle_id %% 2)), colour = "grey", size = 0.1) +
    theme_minimal() +
    coord_equal() +
    theme(legend.position = "none") +
    scale_fill_manual(values = c("purple", "pink"))
  plot(p)
}


# tolerance : controls resolution of tessellation. reduce tolerance to get higher resolutions.

for (tolerance in c(1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7)) {
  d <- string2fill("क ख ग", ttf_file, tolerance = tolerance)
  
  p <- ggplot(d) +
    geom_polygon(aes(x, y, group = triangle_id), fill = "transparent", colour = "black", size = 0.5) +
    theme_minimal() +
    coord_equal() +
    ggtitle(paste0("tolerance: ", tolerance))
  plot(p)
}


# Note that tolerance parameter behaves a bit differently on string2fill() and string2stroke(). But, in either case, 1e-5 ~ 1e-6 should be enough.

for (tolerance in c(1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7)) {
  d <- string2path("क ख ग", ttf_file, tolerance = tolerance)
  
  p <- ggplot(d) +
    geom_path(aes(x, y, group = path_id), colour = "black", size = 0.5) +
    geom_point(aes(x, y, group = path_id), colour = "black", size = 1.5) +
    theme_minimal() +
    coord_equal() +
    ggtitle(paste0("tolerance: ", tolerance))
  plot(p)
}






