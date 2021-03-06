#notes

library(ggplot2)
ggplot(mtcars, aes(sample=mpg))+stat_qq()+theme_bw()
# The result you got from Q-Q plot you can verify the same based on shapiro test.
shapiro.test(mtcars$mpg)
# car::qqPlot() provides the best option for routine visualization.

# generate data for this post
set.seed(20200825)
x <- sort(rnorm(20, 10, 3))
q <- (rank(x) - .5) / length(x)


band <- tibble(  z       = seq(-2.2, 2.2, length.out = 300),  n = length(d$x_sample),
               sample_sd = sd(d$x_sample),
               se = sample_sd * se_z(z, n),
               line = mean(d$x_sample) + sample_sd * z,
               upper = line + 2 * se,
               lower = line - 2 * se,
    
               robust_sd = IQR(d$x_sample) / 1.349,
               robust_line = median(d$x_sample) + z * robust_sd,
               robust_se =  robust_sd * se_z(z, n),
               robust_upper = robust_line + 2 * robust_se,
               robust_lower = robust_line - 2 * robust_se,
             )


ggplot(d) +  geom_point(aes(x = z_theoretical, y = x_sample)) + 
            geom_abline( aes(intercept = mean, slope = sd, color = "Naive"),
                         data = tibble(sd = sd(d$x_sample), mean = mean(d$x_sample))  ) +
             geom_ribbon(  aes(x = z, ymax = upper, ymin = lower, color = "Naive"), 
                          data = band, 
                          fill = NA, show.legend = FALSE  ) +
             labs(  color = "Q-Q line", x = "theoretical quantiles",   y = "sample quantiles"  ) + 
             guides(color = guide_legend(nrow = 1)) +
              theme(legend.position = "top", legend.justification =  "left")

# using CAR

par(mar = c(4, 2, 1, 2))# Set margins on Q-Q plots
library(patchwork)# Use patchwork to capture the plots and combine them 
p1 <- wrap_elements(~ car::qqPlot(x))
p1
# p2 <- wrap_elements(~ {
#     car::qqPlot(x)
#     lines(band$z, band$robust_line, col = "black", lwd = 2)
#     lines(band$z, band$robust_upper, col = "black", lwd = 2)
#     lines(band$z, band$robust_lower, col = "black", lwd = 2)
# })
# p1 + p2   # into a side by side display.


# alternative to Q-Q plot: the worm plot “detrended” Q-Q plots 

par(mar = c(4.5, 4.5, 1, 2))
# I don't know what's up with that error message.
# use scale() to transform in to z-score
gamlss::wp(
    resid = scale(d$x_sample), 
    xlim.all = 2.5, 
    line = FALSE )


###### cleaning janitor
library(sparkline)
library(janitor)
library(dplyr)
data<-read.csv("D:/RStudio/Website/FinData.csv",1)
clean <- janitor::clean_names(data)
clean_x<-clean %>% remove_empty(whic=c("rows"))  # "cols"
clean %>% get_dupes(first_name,certification)

tabyl(clean,employee_status) # easy tabulation
clean %>% tabyl(employee_status) %>% adorn_pct_formatting(digits =2,affix_sign=TRUE)
clean %>% tabyl(employee_status, full_time) %>% adorn_totals("row") %>% adorn_percentages("row") %>%
               adorn_pct_formatting() %>% adorn_ns("front")
# Remove empty column or rows
clean_x<-clean %>% remove_empty(whic=c("rows"))
clean_x<-clean %>% remove_empty(whic=c("cols"))
# duplicates
clean %>% get_dupes(first_name)
clean %>% get_dupes(first_name,certification)

excel_numeric_to_date(41103)




ggpubr::show_line_types()  # library(ggpubr)


library(tidyverse)

library(ggforce)
library(ggfx)
ggplot() + 
    as_reference( geom_text(aes(x = 0, y = 0, label = 'RDSO'), size = 40, family = 'Fontania'),
                 id = 'text_layer') + 
    with_blend( geom_circle(aes(x0 = 0, y0 = 0, r = seq_len(5) ), fill = NA, size = 8),
        bg_layer = 'text_layer',  blend_type = 'xor' ,
        id = 'blended') +   # filters can be turned ref by assigning id
    with_inner_glow( 'blended',  colour = 'red', sigma = 5) +
    coord_fixed()

# even rester image
ggfx_logo <- as.raster(magick::image_read( system.file('help', 'figures', 'logo.png', package = 'ggfx') ))
magick::image_read( "/Users/gk/Downloads/newyear2021.png")
ggfx_logo <- as.raster(magick::image_read( "/Users/gk/Downloads/newyear2021.png" ) ) 
ggplot(mpg) +   with_blend(  bg_layer = ras_fit(ggfx_logo, 'viewport'),
                             geom_point(aes(x = hwy, y = displ), size = 5), 
                             blend_type = 'xor' )



# library(ggthemes)   ;   # or install_github('cttobin/ggthemr') #library(themr) # ggthemr("<theme name>") #ggthemr_reset()
# library(plotly) ;       # plot_grid(gp1, gp2, NULL, gp1, labels = "AUTO")
library(googlesheets4) ;   # gs_auth(new_user = TRUE) #
library("survival") ; library("survminer") ; 
library(ggfortify); library(DataExplorer)

library(scales)   # to access breaks/formatting functions
library(gridExtra) # for arranging plots

library(timetk)
# summarise_by_time(), mutate_by_time() ,filter_by_time() , filter_period() , between_time() , pad_by_time() 
# condense_period() , slidify() - Turn any function into a sliding (rolling) function

# dir to ensure R find file to import # setwd("working-dir-path-here")
library(magick)
raw_img <- magick::image_read("http://www.showbuzzdaily.com/wp-content/uploads/2021/02/Final-Cable-2021-Feb-03-WED.png")
magick::image_ggplot(raw_img)
chopped_image <- raw_img %>% image_crop(geometry_area(703, 1009, 0, 91)) ##crop width:703px ,height:1009px starting +91px from top
magick::image_ggplot(chopped_image)
processed_image <- chopped_image %>% image_negate() %>%   #  # Remove the Horizontal Lines
    image_morphology(method = "Thinning", kernel = "Rectangle:7x1") %>% 
    image_negate() %>% image_quantize(colorspace = "gray") #   # Turn colors to greyscale
tesseract::ocr(processed_image) %>% str_sub(end = str_locate(., '\\n')[1])


image_ggplot(processed_image)



#########
library(summarytools) #devtools::install_github('dcomtois/summarytools', ref='0-8-9') #old version
summarytools::view(dfSummary(iris))
set.seed(2835)
Random_numbers <- sample(c(5e3, 5e4, 5e5), size = 1e4, replace = TRUE, prob = c(.12, .36, .52))
freq(Random_numbers)
print(freq(Random_numbers), big.mark = " ", decimal.mark = ".")
freq(iris$Species)
freq(tobacco$disease, order = "freq", rows = 1:5, headings = FALSE)
print(ctable(x = tobacco$smoker, y = tobacco$diseased, prop = "r"), method = "render")
tobacco  %$%  ctable(smoker, diseased, chisq = TRUE, OR = TRUE, RR = TRUE, headings = FALSE) %>% print(method = "render")
summarytools::descr(iris)
descr(iris, stats = c("mean", "sd"), transpose = TRUE, headings = FALSE)
summarytools::view(dfSummary(iris))
# in Rmd
summarytools::dfSummary(tobacco, plain.ascii = FALSE, style = "grid",  graph.magnif = 0.75, valid.col = FALSE, tmp.img.dir = "/tmp")



fill(data, ...,  .direction = c("down", "up", "downup", "updown"))  # it is .direction =
squirrels %>%
    dplyr::group_by(group) %>%
    fill(  n_squirrels,   .direction = "downup" ) %>%
    dplyr::ungroup()

separate_rows(data, ..., sep = "[^[:alnum:].]+", convert = FALSE)

#########

# Not evaluated
library(sparkline)
sparkline(0)

spk_dt <- data.frame( var      = c("mpg", "wt"),
                     sparkline = c(spk_chr(mtcars$mpg), spk_chr(mtcars$wt))  )

kbl(spk_dt, escape = F) %>%    kable_paper(full_width = F)


