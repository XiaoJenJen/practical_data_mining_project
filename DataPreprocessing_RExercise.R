##################################
# Group Members: Daniel Truong   #
#                Jenny Xu        #
#                Ti-Tai Wang     #
#                Moonsung Kim    #
##################################

#Import data
library(readr)
dat <- read_csv("Accident_Information_2000.csv")

#Data Exploring
colnames(dat)
head(dat)

#Dealing with missing values (894 rows)
library(dplyr)
missing.values.row.a <- filter(dat, !complete.cases(dat))

##not much NAs for each rows (max:3)
max.na.dat <- max(apply(dat, 1, function(x) sum(is.na(x))))

##columns {LSOA_of_Accident_Location:141, 2nd_Road_Class:830} NAs
sort(apply(dat, 2, function(x) sum(is.na(x))))

### Remove "X2nd_Road_Class" and "LSOA_of_Accident_Location" from data
dat <- dat[, -c(4, 20)]

## Focus on the attributes with NAs
dat.na.col <- c("Did_Police_Officer_Attend_Scene_of_Accident", 
                 "Pedestrian_Crossing.Human_Control", "Pedestrian_Crossing.Physical_Facilities", 
                 "X2nd_Road_Number", "LSOA_of_Accident_Location")
### Data type of these columns
typeof.dat.na <- c()
for(i in dat.na.col) {
  typeof.dat.na <- c(typeof.dat.na, typeof(dat[[i]]))
}

### Filling NAs
Mode <- function(x, na.rm=FALSE){ 
    if(na.rm) x<-x[!is.na(x)]
    ux <- unique (x)
    return (ux[which.max(tabulate(match(x, ux)))])
  }

dat[is.na(dat$X2nd_Road_Number), "X2nd_Road_Number"] <- Mode(dat$X2nd_Road_Number)
dat[is.na(dat$Pedestrian_Crossing.Physical_Facilities), "Pedestrian_Crossing.Physical_Facilities"] <- Mode(dat$Pedestrian_Crossing.Physical_Facilities)
dat[is.na(dat$Pedestrian_Crossing.Human_Control), "Pedestrian_Crossing.Human_Control"] <- Mode(dat$Pedestrian_Crossing.Human_Control)
dat[is.na(dat$Did_Police_Officer_Attend_Scene_of_Accident), "Did_Police_Officer_Attend_Scene_of_Accident"] <- Mode(dat$Did_Police_Officer_Attend_Scene_of_Accident)


# Correlation of each attribute
## identify type of each attribute
typeof.dat.col <- c()
for(i in colnames(dat)) {
  typeof.dat.col <- c(typeof.dat.col, typeof(dat[[i]]))
}
typeof.dat.col


