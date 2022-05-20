################################
###  SCRIPT CAMERA TRAPPING  ###
################################
rm(list=ls())

## LOAD PACKAGES
pacman::p_load(tidyverse, exiftoolr, ggplot2, activity) #exifr

path <- "./Data/Check 3"


## List photos
CT_Overview <- tibble(
  Name = list.dirs(path, recursive = FALSE)
) %>%
  mutate(
    N_Photos = map_int(Name, ~dir(.) %>% length)
  )

## Extract time from photos
CT_Data <- tibble(
  FileName = list.files(CT_Overview$Name, full.names = TRUE),
  Species = basename(rep(CT_Overview$Name, CT_Overview$N_Photos))
  ) %>%
  mutate(
    DateTime = exif_read(FileName)$DateTimeOriginal,
    DateTime = as.POSIXct(strptime(DateTime, format = "%Y:%m:%d %H:%M:%S")),
    Hour = hour(DateTime),
    Minutes = minute(DateTime),
    
    FileName = basename(FileName)
  ) %>%
  group_by(Species) %>%
  mutate(
    Time_Diff = difftime(DateTime, lag(DateTime), units = "secs"),
    Time_Indep = as.integer(Time_Diff > 3600),
    Time_Indep = replace_na(Time_Indep, 1)
  )

CT_Clean <- CT_Data %>%
  subset(!Time_Indep==0) %>%
  mutate(
    Time_Diff = NULL,
    Time24 = Hour + (Minutes / 60),
    TimeRadians = (Time24*pi)/12
    )

SpeciesCount <- CT_Clean %>%
  count(Species, sort = TRUE, name = "count") %>%
  filter(!(Species == "Start" | Species == "End" | Species == "Empty"))

ggplot(data = SpeciesCount) + 
  geom_bar(aes(x = reorder(Species, -count), y=count), stat="identity") + 
  theme_bw() + 
  xlab("Species") +
  ggtitle("All Data")

ggplot(data = SpeciesCount %>% filter(!Species == "Train")) + 
  geom_bar(aes(x = reorder(Species, -count), y=count), stat="identity") + 
  theme_bw() + 
  xlab("Species") +
  ggtitle("Train data excluded")


## Subset
Train <- CT_Clean %>%
  subset(Species == "Train") %>%
  pull(TimeRadians)

Elephant <- CT_Clean %>%
  subset(Species == "Impala") %>%
  pull(TimeRadians)

overlap::overlapPlot(Train, Elephant, main = "Train and Elephant Activity")
legend('topleft', c("Train", "Elephant"), lty=c(1,2), col=c(1,4), bty='n')

# This fits the circular density. If you want to plot the bootstapped confidence 
# intervals, change the [sample = "none"] command to [sample = "data"]
# Note that this takes much longer. Please use [?fitact] for more information
test <- fitact(Train, wt = NULL, reps = 1000, bw = NULL, adj = 1, show = TRUE, sample="none")

# This creates the activity plot with the species name as the title
plot(test, hrs = TRUE, frq = TRUE, dat = "histogram", add = FALSE, main = "Train")



###########################
## LOAD DATA FROM WILDID ##
###########################
pacman::p_load(tidyverse, lubridate)

WildID <- list.files(path = "./Data", pattern = "WildID", full.names = TRUE)

WildID <- read_csv(WildID)

## Remove the percentage sign
## Sort data in chronicological order
WildID <- WildID %>% 
  mutate(
    Percentage = as.numeric(sub(pattern = "%", replacement = "", x = Percentage))
  ) %>% 
  arrange(Camera, Taken)

## Clean up some of the names
WildID.sub <- WildID %>% 
  mutate(
    Labels = case_when(
      startsWith(Labels, "Kudu") ~ "Kudu",
      startsWith(Labels, "Lion") ~ "Lion",
      startsWith(Labels, "Hare") ~ "Hare",
      startsWith(Labels, "Zebra") ~ "Zebra",
      TRUE ~ as.character(Labels)
    )
  ) %>% 
  ## Remove entries with no animals 
  subset(!Labels == "No Animal" & !is.na(Labels))

## Identify independent events (over 1 hour)
WildID.IE <- WildID.sub %>%
  count(Camera, `Image Name`, Taken, Labels) %>% 
  group_by(Camera, Labels) %>% 
  mutate(
    tdiff = difftime(Taken, lag(Taken), units = "secs"),
    gap = as.integer(tdiff >= 3600),
    gap = replace_na(gap, replace = 1),
    IndepEvent = paste(Labels, formatC(cumsum(gap)+1, width = 3, flag = "0"), sep=".")
  )

## Data cleaning
WildID.clean <- WildID.IE %>% 
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

## Read other dataset
CTDatabase <- read_csv("./Data/CTDatabase.csv")

A <- CTDatabase %>% 
  mutate(
    FirstPhoto = make_datetime(year = Year, month = Month, day = Day, 
                          hour = hour(`Time from`), min = minute(`Time from`), sec = 0),
    LastPhoto = case_when(
      is.na(`Time till`) ~ FirstPhoto,
      TRUE ~ make_datetime(year = Year, month = Month, day = Day, 
                           hour = hour(`Time till`), min = minute(`Time till`), sec = 0)
    ),
    Duration = difftime(LastPhoto, FirstPhoto, units = "secs"),
    Hour = hour(FirstPhoto),
    Minutes = minute(FirstPhoto),
    Month = month(FirstPhoto),
    Year = year(FirstPhoto)
  ) %>% 
  dplyr::select(Location, Species, `#Total`, FirstPhoto, LastPhoto, Duration, Hour, Minutes, Month, Year)

Top5 <- WildID %>% 
  subset(Labels %in% c("Impala", "Elephant", "Buffalo", "Zebra", "Warthog"))

ggplot(data = Top5, aes(x=Labels, y=Percentage)) +
  geom_boxplot() +
  coord_cartesian(ylim=c(95, 100))








## ADD SPECIES TO LIST ##
OWSpecies <- sort(
              unique(
c("African Elephant", "African Wild Cat", "Baboon", "Black Rhinoceros", "Black-Backed Jackal", 
 "Bushbuck", "Bushpig", "Common Duiker", "Caracal", "Cheetah", "Civet",  "Gennet", "Giraffe", 
 "Hippo", "Impala", "Kudu", "Leopard", "Lion", "Mongoose", "Mouse", "Nyala", "Porcupine", 
 "Rabbit",  "Rat", "Scrub Hare", "Spotted Hyena", "Steenbok", "Velvet Monkey",  "Warthog", 
 "Waterbuck", "Wild Dog",  "Wildebeest", "White Rhinoceros", "Zebra", "00_Unknown")
  )
)

# Check if the system can find ExifTool
#Sys.which("exiftool")

## LOAD PACKAGES
pacman::p_load(camtrapR, secr)

## ADD STATION INFORMATION
StationInfo <- read.csv("Station_Information.csv")

## CREATE A FOLDER FOR EVERY CT STATION IN 'inDir'
createStationFolders ( inDir = "C:/Users/Daikoro/Documents/R/CT/Data",
                                               stations = as.character(StationInfo$Station), 
                                               createinDir = TRUE )

## COPY RAW DATA, RENAME FILES, AND MOVE TO A NEW LOCATION
renaming.table <- imageRename( inDir = "C:/Users/Daikoro/Documents/R/CT/Data",
                               outDir = "C:/Users/Daikoro/Documents/R/CT/Renamed",  
                               hasCameraFolders = FALSE,
                               copyImages = TRUE,
                               writecsv = FALSE
)

## CREATE FOLDERS FOR EVERY SPECIES IN 'OWSpecies'
SpeciesFolderCreate <- createSpeciesFolders( inDir = "C:/Users/Daikoro/Documents/R/CT/Renamed",
                                             species = OWSpecies,
                                             hasCameraFolders = FALSE,
                                             removeFolders = FALSE
)

###########################################################
## MANUALLY DRAG AND DROP IMAGES IN TO THE RIGHT FOLDERS ##
###########################################################

## REMOVE EMPTY FOLDERS
SpeciesFolderCreate <- createSpeciesFolders( inDir = "C:/Users/Daikoro/Documents/R/CT/Renamed",
                                             species = OWSpecies,
                                             hasCameraFolders = FALSE,
                                             removeFolders = TRUE
)

## SPECIES INDENTIFICATION CHECK
## CHECK FOR >=2 SPECIES WITHIN 300-SECOND (5 MIN) INTERVALS
check.folders <- checkSpeciesIdentification(inDir = "C:/Users/Daikoro/Documents/R/CT/Renamed",
                                            IDfrom = "directory",
                                            hasCameraFolders = FALSE,
                                            maxDeltaTime = 300,  # TIME INTERVAL IN SECONDS
                                            excludeSpecies = "00_Unknown"
)

## ADD SPECIES NAME TO IMAGE FILE
species_names_append <- appendSpeciesNames(inDir = "C:/Users/Daikoro/Documents/R/CT/Renamed",
                                           IDfrom = "directory",
                                           hasCameraFolders = FALSE,
                                           removeNames = FALSE
)

## COLLECT ALL IMAGES OF SPECIES
specImagecopy <- getSpeciesImages(species = c("Waterbuck", "Hippo"),
                                  IDfrom = "directory",
                                  inDir = "C:/Users/Daikoro/Documents/R/CT/Renamed",
                                  outDir = "C:/Users/Daikoro/Documents/R/CT",
                                  createStationSubfolders = FALSE
)

