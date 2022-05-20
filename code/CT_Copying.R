##############################
## CREATING SPECIES FOLDERS ##
##############################

rm(list = ls())

## Load packages
pacman::p_load(tidyverse, lubridate)

## Read data
Data <- read_csv(file = "C:/Users/Daikoro/Documents/Transfrontier Africa/Research projects/Camera traps/WildID_ImageList_2021-04-06.csv")

names(Data)[1] <- "ImageName"

## Remove unintersting labels
Data.sub <- Data %>% 
  subset(!(Labels %in% c("No Animal", "Unknown", "Human", "Vehicle", "Bird (of Prey)", "Bird (Other)")) & 
           year(Taken) != 2017
         ) %>% 
  distinct(ImageName, .keep_all = TRUE)

## Remove datat with too few pictures (<30)
Data.count <- Data.sub %>%
  count(Labels) %>% 
  arrange(n) %>% 
  subset(n>=30)

Data.sub <- Data.sub %>% 
  subset(Labels %in% Data.count$Labels)

## Define CT locations
Waterholes <- c("Tamboti", "Coerts", "Nyari")

### 2020 set
## Get all file names
FileNames <- list.files(path = "C:/Users/Daikoro/Sync/Databases/Camera trap data/Photos/2020", recursive = TRUE, full.names = TRUE)

Data.2020 <- Data.sub %>% 
  subset(year(Taken)==2020)

for (i in 1:length(unique(Data.sub$Labels))){
  species.sub <- subset(Data.2020, Labels == unique(Data.sub$Labels)[i])
  speciesmatches <- str_subset(FileNames, paste(species.sub$ImageName, collapse = "|"))
  file.copy(speciesmatches, paste0("C:/Users/Daikoro/Desktop/", unique(Data.sub$Labels)[i]))
}  
  
  

# Create folders
# for (i in 1:length(unique(Data.sub$Labels))){
#   folder <- dir.create(
#     paste0("C:/Users/Daikoro/Desktop/", unique(Data.sub$Labels)[i])
#     )
# }
