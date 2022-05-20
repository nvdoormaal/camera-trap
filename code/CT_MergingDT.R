rm(list=ls())

#######################
## MERGE CAMERA DATA ##
#######################

pacman::p_load(tidyverse, lubridate, readxl)

#############
## DEFINE TIME CUTOFF
TimeCutoff <- 3600 #60 min = 3600 sec

######################################
####### Camera trapping database #####
####### Excel file

# Second sheet
Michelle2019 <- read_excel("./Data/Camera trapping database.xlsx", sheet = "Michelle")

Michelle2019.clean <- Michelle2019 %>% 
  distinct() %>%
  group_by(Camera, Labels) %>% 
  mutate(
    gap = 1,
    IndepEvent = paste(Labels, formatC(cumsum(gap), width = 3, flag = "0"), sep="."),
    Duration = difftime(LastPhoto, FirstPhoto, units = "secs")
  ) %>% 
  ungroup() %>% 
  select(Camera, IndepEvent, Labels, maxN, FirstPhoto, LastPhoto, Duration)

# First sheet
Pictures <- read_excel("./Data/Camera trapping database.xlsx", sheet = "Pictures")

Pictures.sub <- Pictures %>% 
  rename("Camera" = Location, "Labels" = Species) %>% 
  distinct() %>% 
  mutate(
    Labels = str_to_sentence(Labels),
    Labels = case_when(
      startsWith(Labels, "Spotted") ~ "Hyena (Spotted)",
      startsWith(Labels, "Black") ~ "Rhinoceros (Black)",
      startsWith(Labels, "White") ~ "Rhinoceros (White)",
      TRUE ~ as.character(Labels)
    ),
    `Time till` = case_when(
      is.na(`Time till`) ~ `Time from`,
      TRUE ~ `Time till`
    ),
    From = ymd_hms(paste(Date, format(`Time from`, format = "%H:%M:%S"), sep = ' ')),
    Till = ymd_hms(paste(Date, format(`Time till`, format = "%H:%M:%S"), sep = ' '))
  ) %>% 
  arrange(Camera, Labels, From) %>% 
  group_by(Camera, Labels) %>% 
  mutate(
    tdiff = difftime(Till, lag(From), units = "secs"),
    gap = as.integer(tdiff >= TimeCutoff),
    gap = replace_na(gap, replace = 1),
    IndepEvent = paste(Labels, formatC(cumsum(gap), width = 3, flag = "0"), sep=".")
  )

Pictures.clean <- Pictures.sub %>% 
  group_by(Camera, IndepEvent, Labels) %>% 
  summarise(
    maxN = max(Total),
    FirstPhoto = min(From),
    LastPhoto = max(Till),
    Duration = difftime(LastPhoto, FirstPhoto, units = "secs"),
  )


###################################################################################################
####### CTDatabase #####

## Load data
CTDatabase <- read_csv("./Data/CTDatabase.csv")

CTDatabase.sub <- CTDatabase %>% 
  rename("Camera" = Location, "Labels" = Species) %>% 
  distinct() %>% 
  mutate(
    Labels = str_to_sentence(Labels),
    Labels = case_when(
      startsWith(Labels, "Spotted") ~ "Hyena (Spotted)",
      startsWith(Labels, "Black") ~ "Rhinoceros (Black)",
      startsWith(Labels, "White") ~ "Rhinoceros (White)",
      startsWith(Labels, "Hippo") ~ "Hippopotamus",
      startsWith(Labels, "Start") ~ "Human",
      startsWith(Labels, "End") ~ "Human",
      TRUE ~ as.character(Labels)
    ),
    `Time till` = case_when(
      is.na(`Time till`) ~ `Time from`,
      TRUE ~ `Time till`
    ),
    From = dmy_hms(paste(Date, format(`Time from`, format = "%H:%M:%S"), sep = ' ')),
    Till = dmy_hms(paste(Date, format(`Time till`, format = "%H:%M:%S"), sep = ' '))
  ) %>% 
  arrange(Camera, Labels, From) %>% 
  group_by(Camera, Labels) %>% 
  mutate(
    tdiff = difftime(Till, lag(From), units = "secs"),
    gap = as.integer(tdiff >= TimeCutoff),
    gap = replace_na(gap, replace = 1),
    IndepEvent = paste(Labels, formatC(cumsum(gap), width = 3, flag = "0"), sep=".")
  )

CTDatabase.clean <- CTDatabase.sub %>% 
  group_by(Camera, IndepEvent, Labels) %>% 
  summarise(
    maxN = max(Total),
    FirstPhoto = min(From),
    LastPhoto = max(Till),
    Duration = difftime(LastPhoto, FirstPhoto, units = "secs"),
  )

##########################
####### Silke's data #####
####### Kloof dam 

Silke <- read_csv("./Data/KloofCombined.csv")

Silke.sub <- Silke %>% 
  rename("Camera" = Location, "Labels" = SPECIES) %>%
  distinct() %>% 
  mutate(
    Labels = str_to_sentence(Labels),
    Labels = case_when(
      startsWith(Labels, "African ele") ~ "Elephant",
      startsWith(Labels, "Hyena") ~ "Hyena (Spotted)",
      startsWith(Labels, "Chacma") ~ "Baboon",
      startsWith(Labels, "Plains") ~ "Zebra",
      startsWith(Labels, "African civ") ~ "Civet",
      startsWith(Labels, "Banded") ~ "Mongoose",
      startsWith(Labels, "Crested") ~ "Porcupine",
      startsWith(Labels, "Large spotted") ~ "Genet",
      startsWith(Labels, "Small spotted") ~ "Genet",
      startsWith(Labels, "African wildcat") ~ "Wildcat",
      startsWith(Labels, "Black rhino") ~ "Rhinoceros (Black)",
      
      TRUE ~ as.character(Labels)
    ),
    Taken = dmy_hms(paste(DATE, TIME, sep = ' '))
  )
## Identify independent events (over 1 hour)
Silke.IE <- Silke.sub %>%
  arrange(Camera, Labels, Taken) %>% 
  group_by(Camera, Labels) %>% 
  mutate(
    tdiff = difftime(Taken, lag(Taken), units = "secs"),
    gap = as.integer(tdiff >= TimeCutoff),
    gap = replace_na(gap, replace = 1),
    IndepEvent = paste(Labels, formatC(cumsum(gap), width = 3, flag = "0"), sep=".")
  )

## Data cleaning
Silke.clean <- Silke.IE %>% 
  group_by(Camera, IndepEvent, Labels) %>% 
  summarise(
    maxN = max(TOTAL),
    FirstPhoto = min(Taken),
    LastPhoto = max(Taken),
    Duration = difftime(LastPhoto, FirstPhoto, units = "secs"),
  ) %>% 
  arrange(Camera, FirstPhoto)

#############################
####### Michelle's data #####
####### 2020 

Michelle2020 <- read_csv("./Data/MichelleData2020.csv")

A <- ymd_hms(Michelle2020$DateTime, format = "%Y:%M:%D %H:%M:%S")
Michelle2020$DateTime <- na.omit(A)

Michelle2020.sub <- Michelle2020 %>% 
  rename("Labels" = Species) %>% 
  distinct() %>% 
  arrange(Camera, Labels, DateTime) %>% 
  # mutate(
  #   DateTime = ymd_hms(DateTime, format = "%Y:%M:%D %H:%M:%S")
  # ) %>% 
  group_by(Camera, Labels) %>% 
  mutate(
    tdiff = difftime(DateTime, lag(DateTime), units = "secs"),
    gap = as.integer(tdiff >= TimeCutoff),
    gap = replace_na(gap, replace = 1),
    IndepEvent = paste(Labels, formatC(cumsum(gap), width = 3, flag = "0"), sep=".")
  )

Michelle2020.clean <- Michelle2020.sub %>% 
  group_by(Camera, IndepEvent, Labels) %>% 
  summarise(
    maxN = 1,
    FirstPhoto = min(DateTime),
    LastPhoto = max(DateTime),
    Duration = difftime(LastPhoto, FirstPhoto, units = "secs"),
  ) %>% 
  arrange(Camera, FirstPhoto)


#############################
####### WILDID Database #####
WildID.list <- list.files(path = "./Data/", pattern = "WildID.+.csv", full.names = TRUE)

WildID <- lapply(
  WildID.list, read_csv
)

WildID <- bind_rows(WildID)

## Clean up some of the names
WildID.sub <- WildID %>% 
  mutate(
    Labels = case_when(
      startsWith(Labels, "Kudu") ~ "Kudu",
      startsWith(Labels, "Lion") ~ "Lion",
      startsWith(Labels, "Hare") ~ "Hare",
      startsWith(Labels, "Zebra") ~ "Zebra",
      startsWith(Labels, "Bird") ~ "Bird",
      TRUE ~ as.character(Labels)
    )
  ) %>% 
  ## Remove entries with no animals 
  subset(!Labels == "No Animal" & !is.na(Labels) & !year(Taken)==2017)

## Identify independent events (over 1 hour)
WildID.IE <- WildID.sub %>%
  count(Camera, `Image Name`, Taken, Labels) %>% 
  arrange(Camera, Taken) %>% 
  group_by(Camera, Labels) %>% 
  mutate(
    tdiff = difftime(Taken, lag(Taken), units = "secs"),
    gap = as.integer(tdiff >= TimeCutoff),
    gap = replace_na(gap, replace = 1),
    IndepEvent = paste(Labels, formatC(cumsum(gap), width = 3, flag = "0"), sep=".")
  )

## Data cleaning
WildID.clean <- WildID.IE %>% 
  group_by(Camera, IndepEvent, Labels) %>% 
  summarise(
    maxN = max(n),
    FirstPhoto = min(Taken),
    LastPhoto = max(Taken),
    Duration = difftime(LastPhoto, FirstPhoto, units = "secs"),
  ) %>% 
  arrange(Camera, FirstPhoto)


### Merge all datasets
AllCTData <- bind_rows(CTDatabase.clean, Michelle2019.clean, Michelle2020.clean, Pictures.clean, Silke.clean, WildID.clean)

AllCTData.final <- AllCTData %>% 
  mutate(
    Labels = case_when(
      startsWith(Labels, "Scrub") ~ "Hare",
      startsWith(Labels, "Hippo") ~ "Hippopotamus",
      startsWith(Labels, "Hare") ~ "Hare",
      startsWith(Labels, "Guinea") ~ "Guineafowl",
      startsWith(Labels, "Bird") ~ "Bird",
      startsWith(Labels, "Wild dogs") ~ "Wild Dog",
      startsWith(Labels, "Squirrel") ~ "Squirrel",
      startsWith(Labels, "Hyena") ~ "Hyena",
      startsWith(Labels, "Honey Badger") ~ "Honey badger",
      startsWith(Labels, "Start") ~ "Human",
      startsWith(Labels, "End") ~ "Human",
      
      TRUE ~ as.character(Labels)
    ),
    Camera = case_when(
      startsWith(Camera, "Charlies") ~ "Charlies Hide",
      TRUE ~ as.character(Camera)
    ),
    Camera = gsub(Camera, pattern = " dam", replacement = "", ignore.case = TRUE),
    Year = year(FirstPhoto),
    Month = month(FirstPhoto),
    Date = as_date(FirstPhoto),
    Time = format(FirstPhoto, format = "%H:%M:%S")
  ) %>% 
  subset(year(FirstPhoto) > 2017) %>% 
  arrange(Camera, FirstPhoto)

names(AllCTData.final)[c(3,4,7)] <- c("Species", "Total", "Duration_sec")


# write_csv(AllCTData.final, file = "./Data/AllCTData2019_2021.csv")

##########################
####### Michel data #####
####### 2020
# Michel.files <- list.files("./Data/Michelle/", full.names = TRUE)
# 
# Michel.data <- lapply(
#   Michel.files, read_csv
# )
# Michel.data <- bind_rows(Michel.data)
# 
# write_csv(Michel.data, file = "./Data/MichelData2020.csv")
