### This script is to reformat the banding data to submit for the bird banding laboratory

###activate packages
library(readxl)
library(data.table)
library(tidyverse)
library(lubridate)
library(dplyr)
library(openxlsx)

####Read in newly banded birds and recaps of season 2024
mcgo <- read_excel("data/2024/2024_MCGO_banding_data.xlsx")
mcgo_recaps <- read_excel("data/2024/2024_MCGO_banding_data_recaps.xlsx")

###Create band number column to use as a reference to separate newly banded birds and novel recaps
###Concatenate prefix and suffix of the birds
mcgo$BAND_NUMBER <- paste(mcgo$PREFIX,mcgo$SUFFIX,sep = "-")
mcgo_recaps$BAND_NUMBER <- paste(mcgo_recaps$PREFIX,mcgo_recaps$SUFFIX,sep = "-")

###Remove birds that were banded in the same year, this gives us birds that were banded in other previous years
novel_recaps <- mcgo_recaps[!mcgo_recaps$BAND_NUMBER %in% mcgo$BAND_NUMBER,]

###export file to output folder
write.xlsx(novel_recaps, "output/2024/mcgo_novel_recaps_2024.xlsx")

###create disposition column
novel_recaps$disposition <- rep("1")

###separate year month and day by column
novel_recaps <- novel_recaps %>%
  mutate(date = ymd(DATE)) %>%
  mutate_at(vars(DATE), funs(year, month, day))

###create how captured column
novel_recaps$how_captured <- rep("Funnel trap")

###on BBl the capture location is supposed to match the exact ID. Sometime it'll be good organize the data to have the matching ID ready when submitting.

###create bird status for normal wild bird and federal numbered metal band only. This was for all birds
novel_recaps$bird_status <- rep("300")

###create a column for how obtained
novel_recaps$how_obtained <- rep("66")

###create a column for present condition
novel_recaps$present_condition <- rep("07")

###create a new data frame with selected columns to streamline copy and paste for BBL submission
mcgo_novel_recaps_2024_bbl <- novel_recaps[c("BAND_NUMBER","SPECIES","disposition","year","month","day",
                                             "AGE","SEX","bird_status","how_obtained","present_condition","DRIVE","LAT","LONG",
                                             "how_captured")]

write.xlsx(mcgo_novel_recaps_2024_bbl, "output/2024/mcgo_novel_recaps_2024_bbl.xlsx")
