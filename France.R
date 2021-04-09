#######################

#install.packages("av")
library(av)
av::av_demo()

# Create some PNG images
png("input%03d.png", width = 1280, height = 720, res = 108)
for(i in 1:10){    print(   ggplot2::qplot(   rnorm(100)   )   )    }
dev.off()
png_files <- sprintf("input%03d.png", 1:10)
av::av_encode_video(png_files, 'output.mp4', framerate = 1)
utils::browseURL('output.mp4')


#For generating a video from the R graphics, av includes av_capture_graphics() – a convenient wrapper
# which automatically opens and closes the graphics device and then encodes the video:

library(gapminder)
library(ggplot2)
makeplot <- function(){
    datalist <- split(gapminder, gapminder$year)
    lapply(datalist, function(data){
        p <- ggplot(data, aes(gdpPercap, lifeExp, size = pop, color = continent)) +
            scale_size("population", limits = range(gapminder$pop)) + geom_point() + ylim(20, 90) +
            scale_x_log10(limits = range(gapminder$gdpPercap)) + ggtitle(data$year) + theme_classic()
        print(p)
    })
}

# Play 1 plot per sec
video_file <- file.path(tempdir(), 'output.mp4')
av::av_capture_graphics(makeplot(), video_file, 1280, 720, res = 144)
utils::browseURL(video_file)





