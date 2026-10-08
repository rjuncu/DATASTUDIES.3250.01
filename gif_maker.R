install.packages("readr")
install.packages("magick")
install.packages("dplyr")
library(readr)
library(magick)
library(dplyr)

getwd()
list.files()

# Get all PNG files, replace foo with foldername 
all_images <- list.files(
  path = "./foo/",
  pattern = "\\.png$",
  full.names = TRUE
)

# Read all images
imgs <- image_read(all_images)

# Resize images
imgs <- image_scale(imgs, "x800")

# Make all images the same size
# Put every image on an 800 x 800 canvas
imgs <- image_extent(
  imgs,
  geometry = "1500x1000",
  gravity = "center",
  color = "black"
)

# Add a 20-pixel black border
imgs <- image_border(imgs, "black", "20x20")

# Animate
gif <- image_animate(
  imgs,
  fps = 1,
  loop = 0
)

# Save GIF
image_write(gif, "Foo.gif")










