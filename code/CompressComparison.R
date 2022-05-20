#########################
## Compress Comparison ##
#########################
rm(list=ls())


## 1. LOAD PACKAGES
pacman::p_load(magick, purrr, tidyverse, exiftoolr)

## 2. SET MAIN FOLDER
Directory_Folder <- "./Data/Trial"  ## MAKE SURE TO INCLUDE THE FINAL FORWARD SLASH
Folder_Name <- "Original"

## Set values
CompressValues <- seq(100,500, 100)

## 3. SET NEW LOCATION
New_Directories_scale <- c(
  paste0("rescale_" , CompressValues),
  paste0("resize_", CompressValues)
)

## 4. LOOP THROUGH THE ALL FOLDERS AND SUBFOLDERS TO CREATE THEM
for (i in 1:length(New_Directories_scale)){
  if(!dir.exists(path = paste0(Directory_Folder, "/", New_Directories_scale[i]))) 
    dir.create(path = paste0(Directory_Folder, "/", New_Directories_scale[i]))
}

## 5. READ IN Original IMAGES
Original.names <- list.files(paste0(Directory_Folder, "/Original"))


## 6. RUN FUNCTION ON ALL FILES
  map(Original.names, function(list_img) {
    map(CompressValues, function(list_val) {
      img <- image_read(path = paste0(Directory_Folder, "/Original/", list_img))
      img %>%
        image_scale(list_val) %>%
        image_write(path = paste0(Directory_Folder, "/rescale_", list_val, "/rescale", list_val, list_img))
    })
  })
  
  
  ## 7. REMOVE EXIF DATA
  Original.names <- list.files(paste0(Directory_Folder, "/Original"))
  map(Original.names, function(list_img) {
      img <- image_read(path = paste0(Directory_Folder, "/Original/", list_img))
      img %>%
        image_orient(orientation = "Undefined") %>%
        image_rotate(degrees = 180) %>%
        image_write(path = paste0(Directory_Folder, "/Rotated/", list_img))
  })
  
## Trial Data
  Data <- read_csv("./Data/Trial/TrailData.csv")

DataSum <- Data %>%
  count(CompressionValue, CompressionType, Time, Correct)

ggplot(data = DataSum, aes(x=CompressionValue, y=n, fill = Correct)) +
  ggtitle("Classification result for various compression values and methods",
          subtitle = "Split by day-night. 0/none respresents the original images") +
  geom_bar(position = "dodge", stat="identity", colour = "black") + 
  scale_y_continuous("N images") +
  scale_x_continuous("Compression value (low values = more compression)", n.breaks = 6) +
  facet_grid(CompressionType~Time) +
  theme_bw()

