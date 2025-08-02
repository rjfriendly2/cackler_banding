###This script is to start and create a summary statistics for cacklers

###Activate data minipulation packages.
library(readxl)
library(data.table)
library(tidyverse)
library(lubridate)
library(dplyr)
library(openxlsx)


###Read in the master file
mcgo <- read_excel("data/2025/2025_MCGO_banding_data.xlsx")

###Check the data
head(mcgo)
summary(mcgo)

###let the group variables factor variables. Easier for data visuals
#mcgo$BANDER <- as.factor(mcgo$BANDER)
mcgo$SPECIES <- as.factor(mcgo$SPECIES)
mcgo$SEX <- as.factor(mcgo$SEX)
mcgo$DRIVE <- as.factor(mcgo$DRIVE)
mcgo$BLOOD <- as.factor(mcgo$BLOOD)
#mcgo$COMMENTS <- as.factor(mcgo$COMMENTS)
mcgo$AGE <- as.factor(mcgo$AGE)
mcgo$CAPTURE_METHOD <- as.factor(mcgo$CAPTURE_METHOD)
mcgo$HOW_SEX <- as.factor(mcgo$HOW_SEX)
mcgo$RECAP <- as.factor(mcgo$RECAP)

head(mcgo) #they are now factor variables
summary(mcgo)

###Let's search for mistakes by checking unique entries
unique(mcgo$DATE)
#unique(mcgo$year) #there is year 2021, let's fix it
#unique(mcgo$year) #running this code to verify the correction was made. Will do this if needed
unique(mcgo$RECAP)
#unique(mcgo$BANDER) # why are some bander initials missing?
unique(mcgo$SPECIES)
unique(mcgo$AGE)
unique(mcgo$SEX)
unique(mcgo$DRIVE) #BC and Bend Colony. What were they named previously? Would be nice to be consistent.
# I'll just fix these to make them consistent for now
#mcgo$DRIVE[mcgo$DRIVE == "BC"] <- "Bend Colony"
unique(mcgo$DRIVE) #only Bend Colony
#mcgo$DRIVE[mcgo$DRIVE == "Kash Tut"] <- "Kash-Tut"
#unique(mcgo$DRIVE) #only Kash-Tut

###Add column that will represent helicopter or not
###          NO NEED        ###
#mcgo$method <- "ground" #did this and then I'll change the helicopter ones to helicopter ones
#mcgo$method[543:896] <- "helicopter"
#mcgo$method <- as.factor(mcgo$method)
#mcgo$RECAP <- as.factor(mcgo$RECAP)
#head(mcgo) #check data

### DATA IS NOW CLEAN ###

###Load in reecapture data from 2025
mcgo_recaps <- read_excel("data/2025/2025_MCGO_banding_data_recaps.xlsx")

###Check the recapture data
head(mcgo_recaps)
summary(mcgo_recaps)

###let the group variables factor variables. Easier for data visuals
#mcgo_recaps$BANDER <- as.factor(mcgo_recaps$BANDER)
mcgo_recaps$SPECIES <- as.factor(mcgo_recaps$SPECIES)
mcgo_recaps$SEX <- as.factor(mcgo_recaps$SEX)
mcgo_recaps$DRIVE <- as.factor(mcgo_recaps$DRIVE)
mcgo_recaps$BLOOD <- as.factor(mcgo_recaps$BLOOD)
#mcgo_recaps$COMMENTS <- as.factor(mcgo_recaps$COMMENTS)
mcgo_recaps$AGE <- as.factor(mcgo_recaps$AGE)
mcgo_recaps$CAPTURE_METHOD <- as.factor(mcgo_recaps$CAPTURE_METHOD)
mcgo_recaps$HOW_SEX <- as.factor(mcgo_recaps$HOW_SEX)
mcgo_recaps$RECAP <- as.factor(mcgo_recaps$RECAP)

head(mcgo_recaps) #they are now factor variables
summary(mcgo_recaps)


###Create band number column to use as a reference to separate newly banded birds and novel recaps
###Concatenate prefix and suffix of the birds
mcgo$BAND_NUMBER <- paste(mcgo$PREFIX,mcgo$SUFFIX,sep = "-")
mcgo_recaps$BAND_NUMBER <- paste(mcgo_recaps$PREFIX,mcgo_recaps$SUFFIX,sep = "-")

###Remove birds that were banded in the same year, this gives us birds that were banded in other previous years
novel_recaps <- mcgo_recaps[!mcgo_recaps$BAND_NUMBER %in% mcgo$BAND_NUMBER,]

### Look at data summary
summary(mcgo)
# 669 new bands
# 19 recaptures



##### DATA IS NOW CLEAN #####



###summary statistics
mcgo %>% count(DRIVE, AGE)
###Note these are only the birds without recaps
table_01 <- data.frame(mcgo %>% count(AGE, SEX))# Get numbers of age per sex
table_02 <- data.frame(mcgo %>% count(DRIVE)) #looking
table_03 <- data.frame(mcgo %>% count(CAPTURE_METHOD)) #at the data
table_04 <- data.frame(mcgo %>% count(DRIVE, DATE, CAPTURE_METHOD))
table_05 <- data.frame(mcgo %>% count(DRIVE, AGE, SEX))
table_06 <- data.frame(mcgo %>% count(DRIVE, DATE, AGE, SEX, CAPTURE_METHOD))
table_07 <- data.frame(mcgo %>% count(DRIVE, DATE, AGE, SEX))


### Use table_06 to make summary table of number of birds caught in each drive 
drive_sum <-mcgo %>%
  group_by(DRIVE, CAPTURE_METHOD, DATE) %>%
  summarise(
    AHY_M = sum(AGE == "AHY" & SEX == "M"),
    AHY_F = sum(AGE == "AHY" & SEX == "F"),
    L_M = sum(AGE == "L" & SEX == "M"),
    L_F = sum(AGE == "L" & SEX == "F")
  )

###Subset ground and helicopter captures from 'drive_sum'
ground_sum <- subset(drive_sum, CAPTURE_METHOD == "GROUND")
#remove second column
ground_sum <- ground_sum[, -2]

helicopter_sum <- subset(drive_sum, CAPTURE_METHOD == "HELICOPTER")
#remove second column
helicopter_sum <- helicopter_sum[, -2]


###Export these two tables as .csv
##############################################################################
###           TABLE 1 and 2             #####################################
############################################################################
write.csv(ground_sum, "output/2025/ground_based_2025.csv")
write.csv(helicopter_sum, "output/2025/helicopter_based_2025.csv")


################# summary of banded birds from 1990 - 2025 ####################
### Before reading this file into R, make sure to add year 2025 and number of banded birds.
### We do this copying the previous year file and manually input a new row with 2025 information.
############################################################################
###           TABLE 3               #######################################
##########################################################################
mcgo_sum <- read_excel("data/2025/raw_summary_1990_2025.xlsx")

sum(mcgo_sum$YDNWR) #15,220
sum(mcgo_sum$USGS) #10,191
sum(mcgo_sum$Total) #25,411
mean(mcgo_sum$YDNWR) #434
mean(mcgo_sum$USGS) #300
mean(mcgo_sum$Total) #726






### FOR RECAPS TABLE
novel_recaps_table <- novel_recaps[c("DATE", "RECAP","BAND_NUMBER","AGE","SEX","DRIVE")]
write.csv(novel_recaps_table, "output/2025/novel_recaps_table_2025.csv")
