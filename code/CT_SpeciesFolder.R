################################
###  SCRIPT CAMERA TRAPPING  ###
##  Read photos from folders ###
################################
rm(list=ls())

## LOAD PACKAGES
pacman::p_load(tidyverse, exiftoolr, lubridate, purrr, furrr) #exifr

## SET WORKING DIRECTORY
setwd("C:/Users/Daikoro/Sync/Databases/Camera trap data/Photos/Michelle/2020")

## Get all camera trap locations
CameraLocations <- list.dirs(recursive = FALSE, full.names = FALSE)

## Start with the 2021-5-14
CameraLocations <- CameraLocations[1]

## Get list of all observed species
SpeciesList <- unique(
  list.dirs(paste0("./", CameraLocations), recursive = FALSE, full.names = FALSE)
  )

## Get all images for that species and camera
Imagelist <- unlist(
  map(SpeciesList, function(x) list.files(paste0("./", CameraLocations, "/", x), full.names = TRUE))
  )

## Remove location from species list
SpeciesList_trim <- unique(gsub('^([^/]*/)*(.*)', '\\1', Imagelist))

## Function to read images and extract meta data
get_image_to_tibble <- function (x) {
  CT_overview <- tibble(
    Camera = CameraLocations[str_which(x, CameraLocations)],
    Species = SpeciesList[str_which(x, SpeciesList_trim)],
    FileName = basename(x),
    DateTime = as.character(exif_read(x)$DateTimeOriginal)
  )
  CT_overview <- CT_overview %>% 
    mutate(
      DateTime = if_else(is_null(DateTime), "NA", DateTime)
      )
}

safer_get_image_to_tibble <- possibly(get_image_to_tibble, otherwise = "Error in file")

## SET UP MULTI-CORES
no_cores <- availableCores() - 1
future::plan(multisession, workers = no_cores)

Sys.time()
Start <- Sys.time()
## RUN FUNCTION ON ALL FILES
#CT.Data <- future_map(Imagelist, get_image_to_tibble)
CT.Data <- future_map(Imagelist, safer_get_image_to_tibble)   ## THIS WILL TAKE A WHILE

End <- Sys.time() - Start
End

CT.Merge <- CT.Data %>% 
  keep(~ is_tibble(.x)) %>% 
  bind_rows()

write_csv(CT.Merge, paste0(CameraLocations, ".csv"))

