
################
library(magick)  # sudo apt-get install libmagick++-dev
str(magick::magick_config())
library(gganimate)


tiger <- magick::image_read_svg('http://jeroen.github.io/images/tiger.svg', width = 350)
tiger
image_write(tiger, path = "tiger.png", format = "png")

tiger_png <- image_convert(tiger, "png")
image_info(tiger_png)

img   <- magick::image_read("http://jeroen.github.io/images/testocr.png")
img   <- magick::image_read("/home/pedqam/Desktop/img/container-printing.jpg")

frink <- image_read("https://jeroen.github.io/images/frink.png")
print(frink)

image_display(tiger)# X11 only
image_browse(tiger)# System dependent


image_crop(image, "100x150+50") #: crop out width:100px and height:150px starting +50px from the left
image_scale(image, "200")       #: resize proportionally to width: 200px
image_scale(image, "x200")      #: resize proportionally to height: 200px
image_fill(image, "blue", "+100+200") #: flood fill with blue starting at the point at x:100, y:200
image_border(frink, "red", "20x10")  #: adds a border of 20px left+right and 10px top+bottom

# Add 20px left/right and 10px top/bottom
image_border(image_background(frink, "hotpink"), "#000080", "20x10")

image_trim(frink)# Trim margins

image_crop(frink, "100x150+50")# Passport pica

image_scale(frink, "300") # width: 300px  # Resize
image_scale(frink, "x300") # height: 300px
image_rotate(frink, 45)# Rotate or mirror
image_flip(frink)
image_flop(frink)

image_modulate(frink, brightness = 80, saturation = 120, hue = 90)# Brightness, Saturation, Hue

image_fill(frink, "orange", point = "+100+200", fuzz = 20)# Paint the shirt orange # fuzz parameter  to cross for  similarish colors pixels

image_blur(frink, 10, 5)# Add randomness
image_noise(frink)

image_charcoal(frink)# Silly filters
image_oilpaint(frink)
image_negate(frink)

# image_convolve()  Kernel convolution  each pixel value is recalculated using the weighted neighborhood sum defined in the kernel matrix.
kern <- matrix(0, ncol = 3, nrow = 3)
kern[1, 2] <- 0.25

################
library(magick)  # sudo apt-get install libmagick++-dev
# Documentation :
# • analysis   - metrics and calculations: compare, fft
# • animation  - manipulate or combine multiple frames: animate, morph, mosaic, montage, average, append, apply
# • attributes - image properties: comment, info
# • color - contrast, brightness, colors: modulate, quantize, map, transparent, background,colorize, contrast, normalize, enhance, equalize, median
# • composite  - advanced joining: composite, border, frame
# • device     - creating graphics and drawing on images
# • editing    - basic image IO: read, write, convert, join, display, brose
# • effects    - fun effects: despecle, reducenoise, noise, blur, charcoal, edge, oilpaint, emboss, implode, negate
# • geometry   - specify points, areas and sizes using geometry syntax
# • ocr        - extract text from image using tesseract package
# • options    - list option types and values supported in your version of ImageMagick
# • painting   - flood fill and annotating text
# • transform  - shape operations: trim, chop, rotate, resize, scale, sample crop, flip, flop, deskew, page

str(magick::magick_config())

tiger <- magick::image_read_svg('http://jeroen.github.io/images/tiger.svg', width = 350)
tiger
image_write(tiger, path = "tiger.png", format = "png")

tiger_png <- image_convert(tiger, "png")
image_info(tiger_png)

img   <- magick::image_read("http://jeroen.github.io/images/testocr.png")
img   <- magick::image_read("/home/pedqam/Desktop/img/container-printing.jpg")

frink <- image_read("https://jeroen.github.io/images/frink.png")
print(frink)

image_display(tiger)# X11 only
image_browse(tiger)# System dependent


image_crop(image, "100x150+50") #: crop out width:100px and height:150px starting +50px from the left
image_scale(image, "200")       #: resize proportionally to width: 200px
image_scale(image, "x200")      #: resize proportionally to height: 200px
image_fill(image, "blue", "+100+200") #: flood fill with blue starting at the point at x:100, y:200
image_border(frink, "red", "20x10")  #: adds a border of 20px left+right and 10px top+bottom

# Add 20px left/right and 10px top/bottom
image_border(image_background(frink, "hotpink"), "#000080", "20x10")

image_trim(frink)# Trim margins

image_crop(frink, "100x150+50")# Passport pica

image_scale(frink, "300") # width: 300px  # Resize
image_scale(frink, "x300") # height: 300px
image_rotate(frink, 45)# Rotate or mirror
image_flip(frink)
image_flop(frink)

image_modulate(frink, brightness = 80, saturation = 120, hue = 90)# Brightness, Saturation, Hue

image_fill(frink, "orange", point = "+100+200", fuzz = 20)# Paint the shirt orange # fuzz parameter  to cross for  similarish colors pixels

image_blur(frink, 10, 5)# Add randomness
image_noise(frink)

image_charcoal(frink)# Silly filters
image_oilpaint(frink)
image_negate(frink)

# image_convolve()  Kernel convolution  each pixel value is recalculated using the weighted neighborhood sum defined in the kernel matrix.
kern <- matrix(0, ncol = 3, nrow = 3)
kern[1, 2] <- 0.25
kern[2, c(1, 3)] <- 0.25
kern[3, 2] <- 0.25
kern
img <- image_resize(logo, "300x300")
img_blurred <- image_convolve(img, kern)
image_append(c(img, img_blurred))
# Or use any of the standard kernels
img %>% image_convolve('Sobel') %>% image_negate()
img %>% image_convolve('DoG:0,0,2') %>% image_negate()
# Add some text
image_annotate(frink, "I like R!", size = 70, gravity = "southwest", color = "green") #gravity = "southwest"
# Customize text
image_annotate(frink, "CONFIDENTIAL", size = 30, color = "red", boxcolor = "pink", degrees = 60, location = "+50+100")


# Fonts may require ImageMagick has fontconfig # Fonts supported include "sans", "mono", "serif", "Times", "Helvetica", "Trebuchet", "Georgia", "Palatino"or "Comic Sans".


image_annotate(frink, "The quick brown fox", font = 'Times', size = 30)

image_read("https://jeroen.github.io/images/frink.png")            %>%
  image_rotate(270)                        %>%
  image_background("blue", flatten = TRUE) %>%
  image_border("red", "10x10")             %>%
  image_annotate("The same thing with pipes", color = "white", size = 30)

# The standard base methods [ [[, c() and length() are used to manipulate vectors of images which can then be treated as layers or frames.

# Download earth gif and make it a bit smaller for vignette
earth <- image_read("https://jeroen.github.io/images/earth.gif") %>%
  image_scale("200x") %>%
  image_quantize(128)

length(earth)

earth

rev(earth) %>% 
  image_flip() %>% 
  image_annotate("meanwhile in Australia", size = 20, color = "white")



# We can stack layers on top of each other as we would in Photoshop:

bigdata <- image_read('https://jeroen.github.io/images/bigdata.jpg')
frink <- image_read("https://jeroen.github.io/images/frink.png")
logo <- image_read("https://jeroen.github.io/images/Rlogo.png")
img <- c(bigdata, logo, frink)
img <- image_scale(img, "300x300")
image_info(img)

#A mosaic prints images on top of one another, expanding the output canvas such that that everything fits:
image_mosaic(img)
image_flatten(img)# Flattening combines the layers into a single image which has the size of the first image:
image_flatten(img, 'Add')# Flattening and mosaic allow for specifying alternative composite operators:
image_flatten(img, 'Modulate')
image_flatten(img, 'Minus')
image_append(image_scale(img, "x200")) #Appending means simply putting the frames next to each other:horizonatlly
image_append(image_scale(img, "100"), stack = TRUE) # Use stack = TRUE to position them on top of each other, vertically:

#Composing allows for combining two images on a specific position:
bigdatafrink <- image_scale(image_rotate(image_background(frink, "none"), 300), "x200")
image_composite(image_scale(bigdata, "x400"), bigdatafrink, offset = "+180+100")

#PDF document, each page becomes an element of the vector. Note that PDF gets rendered while reading so  density = 72 etc immediately.
manual <- image_read_pdf('https://cloud.r-project.org/web/packages/magick/magick.pdf', density = 72)
image_info(manual)

image_animate(image_scale(img, "200x200"), fps = 1, dispose = "previous") # make them frames in an animation!

# Morphing creates a sequence of n images that gradually morph one image into another. It makes animations
newlogo <- image_scale(image_read("https://jeroen.github.io/images/Rlogo.png"))
oldlogo <- image_scale(image_read("https://jeroen.github.io/images/Rlogo-old.png"))
image_resize(c(oldlogo, newlogo), '200x150!') %>%
  image_background('white') %>%
  image_morph()             %>%
  image_animate(optimize = TRUE)

# If you read in an existing GIF or Video file, each frame becomes a layer:

banana <- image_read("https://jeroen.github.io/images/banana.gif")# Foreground image
banana <- image_scale(banana, "150")
image_info(banana)

#Manipulate the individual frames and put them back into an animation:
background <- image_background(image_scale(logo, "200"), "white", flatten = TRUE)# Background image
frames <- image_composite(background, banana, offset = "+70+30")# Combine and flatten frames
animation <- image_animate(frames, fps = 10, optimize = TRUE)# Turn frames into animation
print(animation)


image_write(animation, "Rlogo-banana.gif")  # Animations can be saved as GIF of MPEG files:

#image_graph() function opens a new graphics device similar to e.g. png() or x11()

# Produce image using graphics device
fig <- image_graph(width = 400, height = 400, res = 96)
ggplot2::qplot(mpg, wt, data = mtcars, colour = cyl)
dev.off()

# Combine, post-process the figure using regular image operations.
out <- image_composite(fig, frink, offset = "+70+30")
print(out)


###### Animated Graphics ########################################
library(gganimate)   # gganimate package using the magick graphics device.

library(gapminder)
library(ggplot2)
img      <- image_graph(600, 340, res = 96)
datalist <- split(gapminder, gapminder$year)
out <- lapply(datalist, function(data){
  p <- ggplot(data, aes(gdpPercap, lifeExp, size = pop, color = continent))          +
    scale_size("population", limits = range(gapminder$pop)) + geom_point() + ylim(20, 90) + 
    scale_x_log10(limits = range(gapminder$gdpPercap)) + ggtitle(data$year) + theme_classic()
  print(p)
}  )
dev.off()

animation <-  image_animate(img, fps = 2, optimize = TRUE)
print(animation)

image_write(animation, "gapminder.gif")

# Rester image,  R’s graphics device is very slow and has a very different coordinate system which reduces the quality of the image.
plot(as.raster(frink))  # base
# Print over another graphic
plot(cars)
rasterImage(frink, 21, 0, 25, 80)

## The grid package :  easier to overlay a raster on the graphics device without having to adjust for the x/y coordinates of the plot.

library(ggplot2)
library(grid)
qplot(speed, dist, data = cars, geom = c("point", "smooth"))
grid.raster(frink)

# editings
# image_read(path, density = NULL, depth = NULL, strip = FALSE, defines = NULL)
# image_read_svg(path, width = NULL, height = NULL)
# image_read_pdf(path, pages = NULL, density = 300, password = "")
# image_read_video(path, fps = 1, format = "png")
# image_write(  image,  path = NULL, format = NULL, quality = NULL, depth = NULL, density = NULL, comment = NULL, flatten = FALSE,  defines = NULL, compression = NULL )
# image_convert(image,  format = NULL,  type = NULL,  colorspace = NULL,  depth = NULL,  antialias = NULL,  matte = NULL,  interlace = NULL)
# image_data(image, channels = NULL, frame = 1)
# image_raster(image, frame = 1, tidy = TRUE)
# image_display(image, animate = TRUE)
# image_browse(image, browser = getOption("browser"))
# image_strip(image)
# image_blank(width, height, color = "none", pseudo_image = "", defines = NULL)
# image_destroy(image)
# image_join(...)
# image_attributes(image)
# image_get_artifact(image, artifact = "")
# demo_image(path)

# Description : High level effects applied to an entire image. These are mostly just for fun.
# image_despeckle(image, times = 1L)
# image_reducenoise(image, radius = 1L)
# image_noise(image, noisetype = "gaussian")
# image_blur(image, radius = 1, sigma = 0.5)
# image_charcoal(image, radius = 1, sigma = 0.5)
# image_oilpaint(image, radius = 1)
# image_emboss(image, radius = 1, sigma = 0.5)
# image_implode(image, factor = 0.5)
# image_negate(image)

# "500x300" – Resize image keeping aspect ratio, such that width does not exceed 500 and the height does not exceed 300.
# • "500x300!" – aspect ratio 
# • "500x" – Resize width to 500 keep aspect ratio # • "x300" – Resize height to 300 keep aspect ratio
# • "50%x20%" – Resize width to 50 percent and height to 20 percent of original
# • "500x300+10+20" – Crop image to 500 by 300 at position 10,20


########## ocr OCR text extraction ################
# recent addition to the package is to extract text from images using OCR. needs tesseract package:
install.packages("tesseract")
img <- image_read("http://jeroen.github.io/images/testocr.png")
print(img)
magick::image_ocr(img)

#  gganimate package using the magick graphics device.

# An overlay image
logo <- image_read('rlogo.png') %>% image_scale('50x50')
# Background image
magick::image_read('heart.dcm') %>%
  image_annotate("This is a heart", gravity = 'northeast', color = 'white', size = 20) %>%
  image_composite(logo, offset = '+200+210') %>%
  image_animate(fps = 10)


