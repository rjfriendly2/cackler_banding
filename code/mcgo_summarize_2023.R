###This script is to start and create a summary statistics for cacklers

###Activate data minipulation packages.
library(readxl)
library(data.table)
library(tidyverse)
library(lubridate)
library(dplyr)
library(openxlsx)


###Read in the master file
mcgo <- read_excel("data/2023/MCGO Master List.xlsx")

###Check the data
head(mcgo)
summary(mcgo)

###let the group variables factor variables. Easier for data visuals
mcgo$BANDER <- as.factor(mcgo$BANDER)
mcgo$SPECIES <- as.factor(mcgo$SPECIES)
mcgo$SEX <- as.factor(mcgo$SEX)
mcgo$DRIVE <- as.factor(mcgo$DRIVE)
mcgo$Blood <- as.factor(mcgo$Blood)
mcgo$COMMENTS <- as.factor(mcgo$COMMENTS)
mcgo$AGE <- as.factor(mcgo$AGE)
head(mcgo) #they are now factor variables


###Let's search for mistakes by checking unique entries
unique(mcgo$DATE)
unique(mcgo$year) #there is year 2021, let's fix it
unique(mcgo$year) #running this code to verify the correction was made. Will do this if needed
unique(mcgo$RECAP)
unique(mcgo$BANDER) # why are some bander initials missing?
unique(mcgo$SPECIES)
unique(mcgo$AGE)
unique(mcgo$DRIVE) #BC and Bend Colony. What were they named previously? Would be nice to be consistent.
# I'll just fix these to make them consistent for now
mcgo$DRIVE[mcgo$DRIVE == "BC"] <- "Bend Colony"
unique(mcgo$DRIVE) #only Bend Colony
mcgo$DRIVE[mcgo$DRIVE == "Kash Tut"] <- "Kash-Tut"
unique(mcgo$DRIVE) #only Kash-Tut

###Add column that will represent helicopter or not
mcgo$method <- "ground" #did this and then I'll change the helicopter ones to helicopter ones
mcgo$method[543:896] <- "helicopter"
mcgo$method <- as.factor(mcgo$method)
mcgo$RECAP <- as.factor(mcgo$RECAP)
head(mcgo) #check data


###Subset recaps and new birds to get numbers
mcgo_new <- mcgo[mcgo$RECAP == "N",] ###insert comma so error wont occur instead of trying all 896 rows
mcgo_recaps <- mcgo[mcgo$RECAP == "Y",]
###We deployed 885 new bands and had 11 total recaptures
head(mcgo_new)
summary(mcgo_new)
##from checking summary:
### Total: 885 new bands
### Ground: 542
### Helicopter: 343
### AHY: 255
### L: 630
### Sex (M): 458
### Sex (F): 427

###export recap table to look up what year they were banded.
write.csv(mcgo_recaps, "output/2023/mcgo_recaps.csv")

##### DATA IS NOW CLEAN #####



###summary statistics
mcgo %>% count(DRIVE, AGE)
###Note these are only the birds without recaps
table_01 <- data.frame(mcgo_new %>% count(DRIVE, AGE)) #just
table_02 <- data.frame(mcgo_new %>% count(DRIVE)) #looking
table_03 <- data.frame(mcgo_new %>% count(method)) #at the data
table_04 <- data.frame(mcgo_new %>% count(DRIVE, DATE, method))
table_05 <- data.frame(mcgo_new %>% count(DRIVE, AGE, SEX))
table_06 <- data.frame(mcgo_new %>% count(DRIVE, DATE, AGE, SEX, method))
table_07 <- data.frame(mcgo_new %>% count(DRIVE, DATE, AGE, SEX))



### 1. Create a table for ground based drives that will include the drive, date
### age and sex. We'll start with table_04 because it's easier and so we can subet
### easier
summary(table_04)
table_04g <- table_04[table_04$method == "ground",] #subset ground based
table_04h <- table_04[table_04$method == "helicopter",] #subset helicopter based

## we have tables for ground/helicopter drives associated by date.
## lets create a vector of AHY males, AHY females, Local males, Local females
################   ground   #####################
#take values table_05 and create a vector that will match table_04g
#take orders from babtuk, bend colony, Hock Slough, Kash-tut, North Camp,
# old village, onumtuk, tut2, tut3
table_05
table_04g$AHYM <- c(10,4,0,2,16,13,8,5,22)
table_04g$AHYF <- c(14,13,0,8,11,14,7,6,12)
table_04g$LM <- c(13,19,7,34,32,42,13,8,27)
table_04g$LF <- c(8,25,8,29,33,27,17,8,27)
table_04g #here is the tablec complete
table_04g$Total <- rowSums(table_04g[,c("AHYM","AHYF","LM","LF")]) #check to make sure this is equal to "n" column
table_04g <- table_04g[,!names(table_04g) %in% c("n")]
table_04g
sum(table_04g$Total) #total number of new bands here is 542, good
sum(table_04g$AHYM) #80
sum(table_04g$AHYF) #85
sum(table_04g$LM) #195
sum(table_04g$LF) #182
summary(table_04g)
# Average birds caught in ground-based drive is 60.22
 
ground_based <- table_04g
#export as csv to output file
write.csv(ground_based, "output/2023/ground_based.csv")


summary(ground_based)









################   helicopter   #####################
table_04h
# 2023 #1, 2023 #2, 2023 #3, 2023 #4, 2023 #5, 2023 #6,
# Kig 1, kig 2, KIG 3, mano 1, mano 2, mano 3,
# mano 4, mano 5, mano 6, mano 7, nas 1, nas 2, nas 3
table_05
table_04h$AHYM <- c(0,3,5,5,1,4,1,6,6,2,0,2,0,0,1,2,2,4,0)
table_04h$AHYF <- c(0,1,6,4,2,3,0,4,9,1,1,2,0,1,1,1,3,7,0)
table_04h$LM <- c(2,13,13,5,13,4,5,8,13,6,8,7,4,3,4,5,10,12,4)
table_04h$LF <- c(1,12,3,5,4,5,6,8,12,2,7,14,5,2,7,8,7,5,1)
table_04h$Total <- rowSums(table_04h[,c("AHYM","AHYF","LM","LF")]) #check to make sure this is equal to "n" column
table_04h
table_04h <- table_04h[,!names(table_04h) %in% c("n")]
table_04h
sum(table_04h$Total) #total number of new bands by helicopter is 343, good
sum(table_04h$AHYM) #44
sum(table_04h$AHYF) #46
sum(table_04h$LM) #139
sum(table_04h$LF) #114
summary(table_04h)
# Average number of burds caught in helicopter-based drive is 18.05

helicopter_based <- table_04h
write.csv(helicopter_based, "output/2023/helicopter_based.csv")



################# summary of banded birds from 1990 - 2023 ####################
mcgo_sum <- read_excel("data/2023/raw_summary_1990_2023.xlsx")

sum(mcgo_sum$YDNWR) #14,551
sum(mcgo_sum$USGS) #10,191
sum(mcgo_sum$Total) #24,742
mean(mcgo_sum$YDNWR) #428
mean(mcgo_sum$USGS) #300
mean(mcgo_sum$Total) #728


#### Create a bar graph for the ground based drive NEED TO RETURN TO THIS
ground_sum <- table_04g # just to work with this
ground_sum$AHY <- rowSums(ground_sum[ , c(4,5)],)
ground_sum$L <- rowSums(ground_sum[ , c(6,7)],)

ggplot(ground_sum, aes(x = DRIVE, y = Total, fill = L)) +
  geom_col()



###### Let's get a table for birds that we collected blood on
mcgo_blood <- mcgo[mcgo$Blood == "Y",]
summary(mcgo_blood)
# 2 recaps
# 62 new birds
mcgo_bloodt <- mcgo_blood[c("DATE","RECAP","band number","AGE","SEX","DRIVE")]
write.csv(mcgo_bloodt, "output/2023/mcgo_blood.csv")


##### RECAptures
#must read in a new recap file "recap info" because one of the bands was wrong in the original one.
recaps_2023 <- read_excel("data/2023/recap info.xlsx")
###Create a new dataframe with selected columns only
recap_table <- recaps_2023[c("DATE","RECAP", "band number", "AGE", "SEX", "DRIVE", "Original_banding_Date")]
recap_table$BANDED_BY <- "YDNWR"
###Change the name of original banding date to something simpler
names(recap_table)[names(recap_table) == "Original_banding_Date"] <- "orig_date"
###change the date to just show the year
head(recap_table)
recap_table$orig_date <- format(recap_table$orig_date, format = "%Y")
head(recap_table)
write.csv(recap_table, "output/2023/recap_table.csv")

