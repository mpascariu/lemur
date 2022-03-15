# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Tue Mar 15 17:18:53 2022
# --------------------------------------------------- #
remove(list = ls())
library(tidyverse)
library(magick)

# Remove the white background from logos

path <- paste0(getwd(), "/inst/app/www/")

image_read(path = paste0(path, "LogoANU.png")) %>% 
  image_transparent(color = "white") %>% 
  image_write(path = paste0(path, "logo_anu.png"), format = "png")
  
image_read(path = paste0(path, "LogoOxford.png")) %>% 
  image_transparent(color = "white") %>% 
  image_write(path = paste0(path, "logo_oxford.png"), format = "png")

image_read(path = paste0(path, "LogoSDU.png")) %>% 
  image_transparent(color = "white") %>% 
  image_write(path = paste0(path, "logo_sdu.png"), format = "png")
  
