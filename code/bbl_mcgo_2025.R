### This script is to reformat the banding data to submit into bird banding lab

###activate packages
library(readxl)
library(data.table)
library(tidyverse)
library(lubridate)
library(dplyr)
library(openxlsx)


### Read in cackler data
mcgo <- read_excel("data/2025/2025_MCGO_banding_data.xlsx")

###Look at data
head(mcgo)

###Concatenate prefix and suffix band numbers
mcgo$band_number <- paste(mcgo$PREFIX, mcgo$SUFFIX, sep = "-")
head(mcgo)

###Create disposition column
mcgo$disposition <- rep("1")
head(mcgo)

###Convert the 'PL' enteries to 'BP'
mcgo$HOW_SEX[mcgo$HOW_SEX == "PL"] <- "BP"

###Create a how aged column. how aged is PL
mcgo$how_aged <- rep("PL")

###Seperate year, month, and day by column
mcgo <- mcgo %>%
  mutate(date = ymd(DATE)) %>%
  mutate_at(vars(DATE), funs(year, month, day))

###Create how captured column
mcgo$how_captured <- rep("Funnel trap")
head(mcgo)

###Create a bird status code for normal wild bird and federal metal band '300'
mcgo$bird_status <- rep("300")

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
unique(mcgo$DRIVE)

###Rename drive locations to BBL accepted names
mcgo$DRIVE[mcgo$DRIVE == "BEND COLONY"] <- "BENDCOL"
mcgo$DRIVE[mcgo$DRIVE == "CAMP DRIVE"] <- "TUTCAMP"
mcgo$DRIVE[mcgo$DRIVE == "TUT 2"] <- "TUT2"
mcgo$DRIVE[mcgo$DRIVE == "NORTH CAMP"] <- "WCAMP"
mcgo$DRIVE[mcgo$DRIVE == "OLD VILLAGE DRIVE"] <- "OLDVILGE"
mcgo$DRIVE[mcgo$DRIVE == "HOCK SLOUGH DRIVE"] <- "HOCSLU"
mcgo$DRIVE[mcgo$DRIVE == "KASH-TUT"] <- "KASHTUT"
mcgo$DRIVE[mcgo$DRIVE == "SOUTH CAMP"] <- "SCAMP" #NEED TO ADD TO BBL LOCATION
mcgo$DRIVE[mcgo$DRIVE == "25HELO01"] <- "UPKASH"
mcgo$DRIVE[mcgo$DRIVE == "25HELO02"] <- "UPKASH"
mcgo$DRIVE[mcgo$DRIVE == "25HELO03"] <- "UPKASH"
mcgo$DRIVE[mcgo$DRIVE == "25HELO04"] <- "BRD18D"
mcgo$DRIVE[mcgo$DRIVE == "25HELO05"] <- "BRD18D"
mcgo$DRIVE[mcgo$DRIVE == "25HELO06"] <- "BRD18D"
mcgo$DRIVE[mcgo$DRIVE == "25HELO07"] <- "BRD18D"
mcgo$DRIVE[mcgo$DRIVE == "25HELO08"] <- "BRD18D"
mcgo$DRIVE[mcgo$DRIVE == "25HELO09"] <- "BRD18D"
mcgo$DRIVE[mcgo$DRIVE == "25HELO10"] <- "BRD18D"
mcgo$DRIVE[mcgo$DRIVE == "25HELO11"] <- "MANO"
mcgo$DRIVE[mcgo$DRIVE == "25HELO12"] <- "MANO"
mcgo$DRIVE[mcgo$DRIVE == "25HELO13"] <- "APHREWN" #NEED TO ADD TO BBL LOCATION
mcgo$DRIVE[mcgo$DRIVE == "25HELO14"] <- "MANO"
mcgo$DRIVE[mcgo$DRIVE == "25HELO15"] <- "MANO"
mcgo$DRIVE[mcgo$DRIVE == "25HELO16"] <- "AKNERKOR"
mcgo$DRIVE[mcgo$DRIVE == "25HELO17"] <- "AKNERKOR"
mcgo$DRIVE[mcgo$DRIVE == "25HELO18"] <- "MANO"
mcgo$DRIVE[mcgo$DRIVE == "25HELO19"] <- "MANO"
mcgo$DRIVE[mcgo$DRIVE == "25HELO20"] <- "MANO"
mcgo$DRIVE[mcgo$DRIVE == "25HELO21"] <- "MANO"
mcgo$DRIVE[mcgo$DRIVE == "25HELO22"] <- "MYST"
mcgo$DRIVE[mcgo$DRIVE == "25HELO23"] <- "MYST"
mcgo$DRIVE[mcgo$DRIVE == "25HELO24"] <- "MYST"
unique(mcgo$DRIVE)

###Create a new dataframe with selected columns so it'll be easier to copy and paste to bbl excel template sheet
mcgo_bbl_2025 <- mcgo[c("band_number", "SPECIES", "disposition", "year", "month", "day", "AGE", "how_aged",
                        "SEX", "HOW_SEX", "bird_status", "DRIVE", "how_captured")]

write.xlsx(mcgo_bbl_2025, "output/2025/mcgo_bbl_2025.xlsx")











