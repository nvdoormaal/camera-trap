###########################
## CORRECTING TIMESTAMPS ##
## IN IMAGE METADATA     ##
###########################

## IMPORTANT!
## Make sure the pictures are ordered by time with the last picture taken at the end.

##Set correct time of last image. Use the format YYYY-MM-DD HH:MM, eg "2012-09-26 12:32"
CorrectTime <- "2021-08-25 12:08"

##Load pacages
pacman::p_load(tidyverse, fs, lubridate, exifr, purrr, furrr)

##Set image path
image_path <- "./Data/Correcting Timestamps"

##Create a time variable for the new time
CorrectTime <- ymd_hm(CorrectTime)

# reading in all the different images we need to treat in batch
files <- dir_ls(image_path, recurse = TRUE)

# get the last image to calculate the time difference
last_image <- length(files)

WrongTime <- read_exif(path = files[last_image],
          tags = c("DateTimeOriginal")) %>%
  pull(DateTimeOriginal) %>% 
  parse_date_time("Ymd HMS")

# calculate time difference between the wrong and correct time
TimeDifference <- difftime(CorrectTime, WrongTime, units = "sec")


# create the funcion to change the date-time in the metadata
change_file_create_date <- function(img){

# get correct exif date
WrongTime <- read_exif(path = img,
                       tags = c("DateTimeOriginal")) %>%
  pull(DateTimeOriginal) %>% 
  parse_date_time("Ymd HMS")

correct_time <- parse_date_time(WrongTime + TimeDifference, "Ymd HMS", tz = "Africa/Johannesburg")

correct_time <- format(correct_time, format = "%Y:%m:%d %H:%M:%S")

exif_call(path = img, args = paste0("-DateTimeOriginal=", correct_time))
}

## SET UP MULTI-CORES
no_cores <- availableCores() - 1
future::plan(multisession, workers = no_cores)

## RUN FUNCTION ON ALL FILES
furrr::future_walk(files, change_file_create_date) ## THIS MIGHT TAKE A WHILE

## Select old files
# files_original <- dir_ls(image_path, recurse = TRUE, regexp = "[.]JPG_original$")
# 
# ## Move to bin
# base::file.remove(files_original)
