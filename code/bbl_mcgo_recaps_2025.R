### This script is to reformat the recapture banding data to submit into bird banding lab

###activate packages
library(readxl)
library(data.table)
library(tidyverse)
library(lubridate)
library(dplyr)
library(openxlsx)

###Read in recapture data
mcgo_recap <- read_excel("data/2025/2025_MCGO_banding_data_recaps.xlsx")
mcgo <- read_excel("data/2025/2025_MCGO_banding_data.xlsx") #this is to remove birds that are captured again in the same year
head(mcgo_recap)

###Concatenate prefix and suffix band numbers
mcgo$band_number <- paste(mcgo$PREFIX, mcgo$SUFFIX, sep = "-")
head(mcgo_recap)
mcgo_recap$band_number <- paste(mcgo_recap$PREFIX, mcgo_recap$SUFFIX, sep = "-")
head(mcgo_recap)

###Remove birds that were banded in the same year, this gives us birds that were banded in other previous years
mcgo_recap <- mcgo_recap[!mcgo_recap$band_number %in% mcgo$band_number,]

###Create disposition column
mcgo_recap$disposition <- rep("R")
head(mcgo_recap)

###Convert the 'PL' enteries to 'BP'
mcgo_recap$HOW_SEX[mcgo_recap$HOW_SEX == "PL"] <- "BP"

###Create a how aged column. how aged is PL
mcgo_recap$how_aged <- rep("PL")

###Seperate year, month, and day by column
mcgo_recap <- mcgo_recap %>%
  mutate(date = ymd(DATE)) %>%
  mutate_at(vars(DATE), funs(year, month, day))

###Create how captured column
mcgo_recap$how_captured <- rep("Funnel trap")
head(mcgo_recap)

###Create a bird status code for normal wild bird and federal metal band '300'
mcgo_recap$bird_status <- rep("300")

###Create a present condition column for recaps
mcgo_recap$present_condition <- rep("07")

###Create a how obtained column for recaps
mcgo_recap$how_obtained <- rep("66")

###Convert drive locations to BBL accepted names
### Accepted names
# TUT2
# BENDCOL
# KASHTUT
# TUTCAMP
# HOCSLU
# TUT 3
# OLDVILGE
# WCAMP (AKA NORTH CAMP)
# MANO
unique(mcgo_recap$DRIVE)

###Rename drive locations to BBL accepted names
#mcgo_recap$DRIVE[mcgo_recap$DRIVE == "BEND COLONY"] <- "BENDCOL"
mcgo_recap$DRIVE[mcgo_recap$DRIVE == "CAMP DRIVE"] <- "TUTCAMP"
mcgo_recap$DRIVE[mcgo_recap$DRIVE == "TUT 2"] <- "TUT2"
mcgo_recap$DRIVE[mcgo_recap$DRIVE == "NORTH CAMP"] <- "WCAMP"
mcgo_recap$DRIVE[mcgo_recap$DRIVE == "OLD VILLAGE"] <- "OLDVILGE"
mcgo_recap$DRIVE[mcgo_recap$DRIVE == "HOCK SLOUGH"] <- "HOCSLU"
mcgo_recap$DRIVE[mcgo_recap$DRIVE == "KASH-TUT"] <- "KASHTUT"
mcgo_recap$DRIVE[mcgo_recap$DRIVE == "SOUTH CAMP"] <- "SCAMP" #NEED TO ADD TO BBL LOCATION
#mcgo_recap$DRIVE[mcgo_recap$DRIVE == "25HELO01"] <- "UPKASH"
#mcgo_recap$DRIVE[mcgo_recap$DRIVE == "25HELO02"] <- "UPKASH"
#mcgo_recap$DRIVE[mcgo_recap$DRIVE == "25HELO03"] <- "UPKASH"
mcgo_recap$DRIVE[mcgo_recap$DRIVE == "25HELO04"] <- "BRD18D"
#mcgo_recap$DRIVE[mcgo_recap$DRIVE == "25HELO05"] <- "BRD18D"
#mcgo_recap$DRIVE[mcgo_recap$DRIVE == "25HELO06"] <- "BRD18D"
#mcgo_recap$DRIVE[mcgo_recap$DRIVE == "25HELO07"] <- "BRD18D"
#mcgo_recap$DRIVE[mcgo_recap$DRIVE == "25HELO08"] <- "BRD18D"
#mcgo_recap$DRIVE[mcgo_recap$DRIVE == "25HELO09"] <- "BRD18D"
#mcgo_recap$DRIVE[mcgo_recap$DRIVE == "25HELO10"] <- "BRD18D"
#mcgo_recap$DRIVE[mcgo_recap$DRIVE == "25HELO11"] <- "MANO"
#mcgo_recap$DRIVE[mcgo_recap$DRIVE == "25HELO12"] <- "MANO"
#mcgo_recap$DRIVE[mcgo_recap$DRIVE == "25HELO13"] <- "APHREWN" #NEED TO ADD TO BBL LOCATION
#mcgo_recap$DRIVE[mcgo_recap$DRIVE == "25HELO14"] <- "MANO"
#mcgo_recap$DRIVE[mcgo_recap$DRIVE == "25HELO15"] <- "MANO"
#mcgo_recap$DRIVE[mcgo_recap$DRIVE == "25HELO16"] <- "AKNERKOR"
#mcgo_recap$DRIVE[mcgo_recap$DRIVE == "25HELO17"] <- "AKNERKOR"
#mcgo_recap$DRIVE[mcgo_recap$DRIVE == "25HELO18"] <- "MANO"
#mcgo_recap$DRIVE[mcgo_recap$DRIVE == "25HELO19"] <- "MANO"
#mcgo_recap$DRIVE[mcgo_recap$DRIVE == "25HELO20"] <- "MANO"
mcgo_recap$DRIVE[mcgo_recap$DRIVE == "25HELO21"] <- "MANO"
#mcgo_recap$DRIVE[mcgo_recap$DRIVE == "25HELO22"] <- "MYST"
#mcgo_recap$DRIVE[mcgo_recap$DRIVE == "25HELO23"] <- "MYST"
#mcgo_recap$DRIVE[mcgo_recap$DRIVE == "25HELO24"] <- "MYST"



###Create a new dataframe with selected columns so it'll be easier to copy and paste to bbl excel template sheet
mcgo_bbl_recaps_2025 <- mcgo_recap[c("band_number", "SPECIES", "disposition", "year", "month", "day", "AGE", "how_aged",
                        "SEX", "HOW_SEX", "bird_status", "how_obtained", "present_condition", "DRIVE", "how_captured")]

write.xlsx(mcgo_bbl_recaps_2025, "output/2025/mcgo_bbl_recaps_2025.xlsx")










