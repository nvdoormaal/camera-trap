################################
## WildID Pre-Post Comparison ##
################################

rm(list = ls())

## Load packages
pacman::p_load(tidyverse, lubridate)

## Read pre and post data
Pre <- read_csv("./Data/WildID PrePost/WildID_PreClass_24Apr.csv")
Post <- read_csv("./Data/WildID PrePost/WildID_PostClass_24Apr.csv")

## Post data
## Find independent events for post data
TimeCutoff <- 3600

Post.edit <- Post %>% 
  mutate(
    Labels = case_when(
      startsWith(Labels, "Hare") ~ "Hare",
      startsWith(Labels, "Lion") ~ "Lion",
      startsWith(Labels, "Hyena") ~ "Hyena",
      startsWith(Labels, "Bird") ~ "Bird",
      TRUE ~ as.character(Labels)
    )
  ) %>% 
  subset(!Labels == "No Animal" & !is.na(Labels) & !Labels == "Unknown") %>% 
  arrange(Camera, Taken) %>% 
  count(Camera, `Image Name`, Taken, Labels) %>% 
  group_by(Camera, Labels) %>% 
  mutate(
    tdiff = difftime(Taken, lag(Taken), units = "secs"),
    gap = as.integer(tdiff >= TimeCutoff),
    gap = replace_na(gap, replace = 1),
    IndepEvent = paste(Labels, formatC(cumsum(gap), width = 3, flag = "0"), sep=".")
  )

## Data cleaning for post data
Post.final <- Post.edit %>% 
  group_by(Camera, IndepEvent) %>% 
  summarise(
    maxN = max(n),
    FirstPhoto = min(Taken),
    LastPhoto = max(Taken),
    Duration = difftime(LastPhoto, FirstPhoto, units = "secs"),
    Hour = hour(FirstPhoto),
    Minutes = minute(FirstPhoto),
    Month = month(FirstPhoto),
    Year = year(FirstPhoto)
  ) %>% 
  arrange(Camera, FirstPhoto)


## Pre data
## Find independent events for pre data
TimeCutoff <- 3600
ConfidenceCutoff <- 85

SpeciesToRemove <- c(NA, "No Animal", "Unknown", "Aardwolf", "Caracal", "Fox", "Dik Dik", "Secretary Bird", "Serval", "Bustard", "Rodent")

Pre.edit <- Pre %>% 
  mutate(
    Labels = case_when(
      startsWith(Labels, "Hare") ~ "Hare",
      startsWith(Labels, "Lion") ~ "Lion",
      startsWith(Labels, "Hyena") ~ "Hyena",
      startsWith(Labels, "Bird") ~ "Bird",
      TRUE ~ as.character(Labels)
    ),
    Percentage = as.numeric(sub(pattern = "%", replacement = "", x = Percentage))
  ) %>% 
  subset(!(Labels %in% SpeciesToRemove) & Percentage > ConfidenceCutoff) %>% 
  arrange(Camera, Taken) %>% 
  group_by(Camera) %>% 
  mutate(
    Photo_tdiff = difftime(Taken, lag(Taken), units = "secs")
  ) %>% 
  ungroup() %>% 
  group_by(Camera, `Image Name`) %>% 
  mutate(
   Labels = case_when(
    lag(Labels) == lead(Labels) & Photo_tdiff < 60 ~ lag(Labels) ,
    TRUE ~ Labels
   )
  ) %>% 
  count(Camera, `Image Name`, Taken, Labels) %>%
  group_by(Camera, `Image Name`, Taken) %>% 
  filter(n == max(n)) %>% 
  ungroup() %>% 
  group_by(Camera, Labels) %>% 
  mutate(
    Species_tdiff = difftime(Taken, lag(Taken), units = "secs"),
    gap = as.integer(Species_tdiff >= TimeCutoff),
    gap = replace_na(gap, replace = 1),
    IndepEvent = paste(Labels, formatC(cumsum(gap), width = 3, flag = "0"), sep=".")
  ) 

## Data cleaning for pre data
Pre.final <- Pre.edit %>% 
  group_by(Camera, IndepEvent, Labels) %>% 
  summarise(
    maxN = max(n),
    FirstPhoto = min(Taken),
    LastPhoto = max(Taken),
    Duration = as.integer(difftime(LastPhoto, FirstPhoto, units = "secs")),
    Hour = hour(FirstPhoto),
    Minutes = minute(FirstPhoto),
    Month = month(FirstPhoto),
    Year = year(FirstPhoto),
    ImageSet = paste(min(`Image Name`), "-", max(`Image Name`))
  ) %>% 
  arrange(Camera, FirstPhoto) %>% 
  subset(select = -IndepEvent)


## Create excel sheet with highlighted cells
HighLight <- createStyle(fontColour = "black", bgFill = "yellow")

wb <- createWorkbook()
addWorksheet(wb, "Data")

writeData(wb, "Data", Pre.final)
conditionalFormatting(wb, "Data", cols=1:ncol(Pre.final), rows=1:nrow(Pre.final), rule="$F1==0", style = HighLight)
conditionalFormatting(wb, "Data", cols=1:ncol(Pre.final), rows=1:nrow(Pre.final), type = "contains", rule = "Rhinoceros", style = HighLight)

saveWorkbook(wb, "Test.xlsx", TRUE)
