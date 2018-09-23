##################################
# Group Members: Daniel Truong   #
#                Jenny Xu        #
#                Ti-Tai Wang     #
#                Moonsung Kim    #
##################################

#RExercise
#Filling unknowns using linear regression
#This method is used when two variables are highly correlated. One value of variable A can be used to predict the value for variable B using the linear regression model
#first let's see what variables in algae are highly correlated
symnum(cor(algae[, 4:18], use = "complete.obs")) # study only the numerical variables (4-18) and use only the complete observations -- obs with NAs are not used. symnum() makes the correlation matrix more readable

#we can see from the matrix, PO4 and oPO4 are correctly with confidence level of 90%.
#Next, we find the linear model between PO4 and oPO4
algae <- algae[-manyNAs(algae), ] #manyNAs is a method provided in DMwR2, it selects the observations with 20% or move values as NAs. 
m = lm(PO4 ~ oPO4, data=algae)
  
#check the model, is it good? See http://r-statistics.co/Linear-Regression.html
summary(m)

#this lm is PO4 = 1.293*oPO4 + 42.897
algae$PO4

#scaling and normalization
library(dplyr)
data(iris)
iris.norm <- cbind(scale(select(iris, -Species)), select(iris, Species)) #because scale() takes numeric matrix as input, we first remove Species column, then use cbind() to add the column back after normalization.
summary(iris)
max <- apply(select(iris, -Species), 2, max, na.rm=TRUE)
min <- apply(select(iris, -Species), 2, min, na.rm=TRUE)
iris.scaled <- cbind(scale(select(iris, -Species), center=min, scale=max-min), select(iris, Species)) 
summary(iris.scaled)

#discretizing variables
data(Boston, package="MASS")
summary(Boston$age)
Boston$newAge <- cut(Boston$age,5) #create 5 bins and add new column newAge to Boston
table(Boston$newAge)
Boston$newAge <- cut(Boston$age,5, labels=c("veryyoung", "young", "mid", "older", "old")) #add labels
table(Boston$newAge)

#use ChiMerge to discretize data with class labels
data(iris)
iris
install.packages("discretization")
library("discretization")
iris.dis = chiM(iris, alpha=0.5) #assume the last column is the class label
iris.dis$cutp #show cut points
iris.dis$Disc.data #discretized data

#variable correlations and dimensionality reduction
#chi-squared test
# H0: (Prisoner's race)(Victim's race) are independent
# data (contingency table):
  racetable = rbind(c(151,9),
                   c(63,103))
test1 = chisq.test(racetable, correct=F)
test1


#Correlations
data(iris)
cor(select(iris,-Species)) #base R method cor() take a numeric matrix and compute correlations among all variables
#use Principle Components Analysis (pca)
pca.data <-iris[, -5] #not including the 5th column Species
pca <- princomp(pca.data)
loadings(pca)
head(pca$scores)
#if we are happy with capturing 75% of the original variance of the cases, we can reduce the original data to the first three components.
#component scores are computed based on the loading, for example, comp1 = 0.361*Sepal.Length+0.857*Petal.Length+0.358*Petal.Width 
iris.reduced <- data.frame(pca$scores[,1:3], Species=iris$Species)
head(iris.reduced)

#stratified sampling
library(dplyr)
set.seed(1) #make results the same each run
summary(algae)
sample <-algae %>% group_by(season) %>% sample_frac(0.25)
summary(sample)























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
#typeof.dat.col <- c()
#for(i in colnames(dat)) {
#  typeof.dat.col <- c(typeof.dat.col, typeof(dat[[i]]))
#}
#typeof.dat.col

# Get numeric attributes and compute correlations
numeric_dat = select_if(dat, is.numeric)
attr_cor = cor(numeric_dat)

# Sort correlations, by absolute value, from highest to lowest.
attr_cor[lower.tri(attr_cor,diag=TRUE)]=NA  
sorted_cor=as.data.frame(as.table(attr_cor))
sorted_cor=na.omit(sorted_cor)
sorted_cor=sorted_cor[order(-abs(sorted_cor$Freq)),]
