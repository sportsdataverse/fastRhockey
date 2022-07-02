library(showtext)
library(hexSticker)
library(jpeg)
library(magick)
library(extrafont)
font_import()

font_import(path = "data-raw/electrical-font")
loadfonts(device = "win", quiet = TRUE) 

fonts()
m <- png::readPNG("data-raw/blue-lighting-black.png")
img <- matrix(rgb(m[,,1],m[,,2],m[,,3]), nrow=dim(m)[1]) #0.2 is alpha
rast <- grid::rasterGrob(img, interpolate = T)
font <- "Electric"

filename0 <- paste0("data-raw/",font,".png")


## use the ggplot2 example
sticker(rast, package="fastRhockey",
        p_size = 20,  p_x = 1, p_y = 1.45,
        s_x = 1, s_y = 0.80, s_width = 1.0, s_height = 1.0,
        h_fill = '#000000', h_color = '#000000',
        p_family = font,filename=filename0)



## Automatically use showtext to render text for future devices
showtext_auto()
font_add_google("Fjalla One")
font_add_google("Playfair Display")
font_add_google("Great Vibes")
font_add_google("Zilla Slab")
font_add_google("Orbitron")
font_add_google("Abril Fatface")
font_add_google("Barlow")

font_add_google("Bree Serif")
font_add_google("Bonbon")
font_add_google("Amiri")
font_add_google("Ropa Sans")
font_add_google("Cantata One")
font_add_google("Roboto Slab")

font_add_google("Cardo")
font_add_google("Poppins")
font_add_google("Montserrat")
font_add_google("EB Garamond")
font_add_google("Neuton")
font_add_google("Josefin Slab")
font_add_google("Unna")
font_add_google("")
font_add_google("")
font_add_google("")
font_add_google("")
font_add_google("")
font_add_google("")
font_add_google("")
font_add_google("")



