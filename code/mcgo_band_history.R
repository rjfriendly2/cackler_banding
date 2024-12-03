### Summary of cackling goose banding history

###Activate data minipulation packages.
library(readxl)
library(data.table)
library(tidyverse)
library(lubridate)
library(dplyr)
library(openxlsx)

###Read in the excel file
mcgo_summary <- read_excel("data/mcgo_summary.xlsx")

###Make year factor variable
mcgo_summary$Year <- as.factor(mcgo_summary$Year)

###check excel sheet
summary(mcgo_summary)

###get sum of last three columns
three_totals <- colSums(mcgo_summary[, c("YDNWR","USGS","Total")])
three_totals
