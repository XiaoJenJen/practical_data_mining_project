##################################
# Group Members: Daniel Truong   #
#                Jenny Xu        #
#                Ti-Tai Wang     #
#                Moonsung Kim    #
##################################

#Import data
library(readr)
adat <- read_csv("Accident_Information.csv")
vdat <- read_csv("Vehicle_Information.csv")

#Data Exploring
colnames(adat)
colnames(vdat)
head(adat)
head(vdat)

#Dealing with missing values
library(dplyr)
missing.values.row.a <- filter(adat, !complete.cases(adat))
missing.values.row.v <- filter(vdat, !complete.cases(vdat))

##not much NAs for each rows
max.na.adat <- max(apply(adat, 1, function(x) sum(is.na(x))))
max.na.vdat <- max(apply(vdat, 1, function(x) sum(is.na(x))))

##columns {LSOA_of_Accident_Location:137822, 2nd_Road_Class:789860} NAs
sort(apply(adat, 2, function(x) sum(is.na(x))))
##columns {model:214486, Propulsion:245843, Engine_Capacity_.CC.:265861, 
##         Age_of_Vehicle:358149, Driver_IMD_Decile:734812} NAs
sort(apply(vdat, 2, function(x) sum(is.na(x))))






