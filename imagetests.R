
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





########################################
magick::image_ocr(img)


# An overlay image
logo <- image_read('rlogo.png') %>% image_scale('50x50')
# Background image
magick::image_read('heart.dcm') %>%
    image_annotate("This is a heart", gravity = 'northeast', color = 'white', size = 20) %>%
  image_composite(logo, offset = '+200+210') %>%
  image_animate(fps = 10)

