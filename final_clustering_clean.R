library(dplyr)
library(ISLR)
library(cluster)
library(Rtsne)
library(ggplot2)
library(FactoMineR)

set.seed(777)

#######################
# Data Preparation
#######################
accidents = read.csv(file = "./Accident_data_rural_cleaned.csv",                     
                     header = TRUE,
                     sep = ",",
                     stringsAsFactors = FALSE)

summary(accidents)

# Explore Dataset
colnames(accidents)
lapply(accidents, class)

a = accidents

acc_numeric = a[1]
acc_factor = a[1]
acc_onehot = accidents[1]
acc_binary = accidents[1]

#transform Date into Accident severity
unique(accidents$Accident_Severity)
acc_factor$Accident_Severity = as.factor(accidents$Accident_Severity)
acc_numeric$Accident_Severity = as.numeric(factor(accidents$Accident_Severity, levels = c("Slight", "Serious", "Fatal")))
for(unique_value in unique(accidents$Accident_Severity)){
  acc_onehot[paste("Accident_Severity", unique_value, sep = ".")] <- ifelse(accidents$Accident_Severity == unique_value, 1, 0)
}
unique(acc_onehot$Accident_Severity.Fatal)
acc_onehot[2,]

acc_binary$Accident_Severity <- matrix(
  as.integer(intToBits(as.integer(as.factor(accidents$Accident_Severity)))),
  ncol = 32,
  nrow = length(accidents$Accident_Severity),
  byrow = TRUE
)[, 1:ceiling(log(length(unique(accidents$Accident_Severity)) + 1)/log(2))]
acc_binary[1,]


#transform Date into 4 seasons, we're using just single year data, sesonal value is not important..?
unique(accidents$Date)
a$Date = sapply(strsplit(accidents$Date,"-"),
                function(x) {
                  x <- as.numeric(x)
                  x[2]
                }
)
unique(a$Date)
acc_factor$Month = as.factor(factor(a$Date, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels = c(1,2,3,4,5,6,7,8,9,10,11,12)))
acc_numeric$Month = as.numeric(factor(a$Date, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)))

#season
a$Season = as.factor(factor(a$Date, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                            labels = c("Winter", "Winter", "Spring", "Spring", "Spring", "Summer", "Summer", "Summer", "Autumn", "Autumn", "Autumn", "Winter")))
unique(a$Season)
acc_factor$Season = as.factor(factor(a$Season, levels = c("Winter", "Spring", "Summer", "Autumn")))
acc_numeric$Season = as.numeric(factor(a$Season, levels = c("Winter", "Spring", "Summer", "Autumn")))
for(unique_value in unique(a$Season)){
  acc_onehot[paste("Season", unique_value, sep = ".")] <- ifelse(a$Season == unique_value, 1, 0)
}
acc_binary$Season <- matrix(
  as.integer(intToBits(as.integer(as.factor(a$Season)))),
  ncol = 32,
  nrow = length(a$Season),
  byrow = TRUE
)[, 1:ceiling(log(length(unique(a$Season)) + 1)/log(2))]


#day
unique(accidents$Day_of_Week)
acc_factor$Day_of_Week = as.factor(accidents$Day_of_Week)
acc_numeric$Day_of_Week = as.numeric(factor(accidents$Day_of_Week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))
for(unique_value in unique(accidents$Day_of_Week)){
  acc_onehot[paste("Day_of_Week", unique_value, sep = ".")] <- ifelse(accidents$Day_of_Week == unique_value, 1, 0)
}
acc_binary$Day_of_Week <- matrix(
  as.integer(intToBits(as.integer(as.factor(accidents$Day_of_Week)))),
  ncol = 32,
  nrow = length(accidents$Day_of_Week),
  byrow = TRUE
)[, 1:ceiling(log(length(unique(accidents$Day_of_Week)) + 1)/log(2))]


#Light_Condition
acc_factor$Light_Conditions = as.factor(accidents$Light_Conditions)
for(unique_value in unique(accidents$Light_Conditions)){
  acc_onehot[paste("Light_Conditions", unique_value, sep = ".")] <- ifelse(accidents$Light_Conditions == unique_value, 1, 0)
}
acc_binary$Light_Conditions <- matrix(
  as.integer(intToBits(as.integer(as.factor(accidents$Light_Conditions)))),
  ncol = 32,
  nrow = length(accidents$Light_Conditions),
  byrow = TRUE
)[, 1:ceiling(log(length(unique(accidents$Light_Conditions)) + 1)/log(2))]


#Road_Surface_Conditions
acc_factor$Road_Surface_Conditions = as.factor(accidents$Road_Surface_Conditions)
for(unique_value in unique(accidents$Road_Surface_Conditions)){
  acc_onehot[paste("Road_Surface_Conditions", unique_value, sep = ".")] <- ifelse(accidents$Road_Surface_Conditions == unique_value, 1, 0)
}
acc_binary$Road_Surface_Conditions <- matrix(
  as.integer(intToBits(as.integer(as.factor(accidents$Road_Surface_Conditions)))),
  ncol = 32,
  nrow = length(accidents$Road_Surface_Conditions),
  byrow = TRUE
)[, 1:ceiling(log(length(unique(accidents$Road_Surface_Conditions)) + 1)/log(2))]


#Road_Type
acc_factor$Road_Type = as.factor(accidents$Road_Type)
for(unique_value in unique(accidents$Road_Type)){
  acc_onehot[paste("Road_Type", unique_value, sep = ".")] <- ifelse(accidents$Road_Type == unique_value, 1, 0)
}
acc_binary$Road_Type <- matrix(
  as.integer(intToBits(as.integer(as.factor(accidents$Road_Type)))),
  ncol = 32,
  nrow = length(accidents$Road_Type),
  byrow = TRUE
)[, 1:ceiling(log(length(unique(accidents$Road_Type)) + 1)/log(2))]


#Special_Conditions_at_Site
acc_factor$Special_Conditions_at_Site = as.factor(accidents$Special_Conditions_at_Site)
for(unique_value in unique(accidents$Special_Conditions_at_Site)){
  acc_onehot[paste("Special_Conditions_at_Site", unique_value, sep = ".")] <- ifelse(accidents$Special_Conditions_at_Site == unique_value, 1, 0)
}
acc_binary$Special_Conditions_at_Site <- matrix(
  as.integer(intToBits(as.integer(as.factor(accidents$Special_Conditions_at_Site)))),
  ncol = 32,
  nrow = length(accidents$Special_Conditions_at_Site),
  byrow = TRUE
)[, 1:ceiling(log(length(unique(accidents$Special_Conditions_at_Site)) + 1)/log(2))]


#Time
unique(accidents$Time)
a$Time = sapply(strsplit(accidents$Time,":"),
                function(x) {
                  x <- as.numeric(x)
                  x[1]+x[2]/60
                }
)
# Midnight - 6am = Night; 6am - Noon = Morning; Noon - 6pm = afternoon; 6pm - Midnight = evening
a$Time = cut(a$Time, breaks=4, labels=c("Night", "Morning", "Afternoon", "Evening"))
acc_factor$Time = as.factor(a$Time)
acc_numeric$Time = as.numeric(factor(a$Time, levels = c("Night", "Morning", "Afternoon", "Evening")))
for(unique_value in unique(a$Time)){
  acc_onehot[paste("Time", unique_value, sep = ".")] <- ifelse(a$Time == unique_value, 1, 0)
}
acc_binary$Time <- matrix(
  as.integer(intToBits(as.integer(as.factor(a$Time)))),
  ncol = 32,
  nrow = length(a$Time),
  byrow = TRUE
)[, 1:ceiling(log(length(unique(a$Time)) + 1)/log(2))]


#Urban_or_Rural_Area
acc_factor$Urban_or_Rural_Area = as.factor(accidents$Urban_or_Rural_Area)
for(unique_value in unique(accidents$Urban_or_Rural_Area)){
  acc_onehot[paste("Urban_or_Rural_Area", unique_value, sep = ".")] <- ifelse(accidents$Urban_or_Rural_Area == unique_value, 1, 0)
}
acc_binary$Urban_or_Rural_Area <- matrix(
  as.integer(intToBits(as.integer(as.factor(accidents$Urban_or_Rural_Area)))),
  ncol = 32,
  nrow = length(accidents$Urban_or_Rural_Area),
  byrow = TRUE
)[, 1:ceiling(log(length(unique(accidents$Urban_or_Rural_Area)) + 1)/log(2))]


#Weather_Conditions
acc_factor$Weather_Conditions = as.factor(accidents$Weather_Conditions)
for(unique_value in unique(accidents$Weather_Conditions)){
  acc_onehot[paste("Weather_Conditions", unique_value, sep = ".")] <- ifelse(accidents$Weather_Conditions == unique_value, 1, 0)
}
acc_binary$Weather_Conditions <- matrix(
  as.integer(intToBits(as.integer(as.factor(accidents$Weather_Conditions)))),
  ncol = 32,
  nrow = length(accidents$Weather_Conditions),
  byrow = TRUE
)[, 1:ceiling(log(length(unique(accidents$Weather_Conditions)) + 1)/log(2))]


#Age_Band_of_Driver
acc_factor$Age_Band_of_Driver = as.factor(accidents$Age_Band_of_Driver)
acc_numeric$Age_Band_of_Driver = as.numeric(factor(accidents$Age_Band_of_Driver, levels = c("Data missing or out of range", "6 - 10", "11 - 15", "16 - 20", "21 - 25", "26 - 35", "36 - 45", "56 - 65", "46 - 55", "66 - 75", "Over 75")))
for(unique_value in unique(accidents$Age_Band_of_Driver)){
  acc_onehot[paste("Age_Band_of_Driver", unique_value, sep = ".")] <- ifelse(accidents$Age_Band_of_Driver == unique_value, 1, 0)
}
acc_binary$Age_Band_of_Driver <- matrix(
  as.integer(intToBits(as.integer(as.factor(accidents$Age_Band_of_Driver)))),
  ncol = 32,
  nrow = length(accidents$Age_Band_of_Driver),
  byrow = TRUE
)[, 1:ceiling(log(length(unique(accidents$Age_Band_of_Driver)) + 1)/log(2))]


#Sex_of_Driver
acc_factor$Sex_of_Driver = as.factor(accidents$Sex_of_Driver)
for(unique_value in unique(accidents$Sex_of_Driver)){
  acc_onehot[paste("Sex_of_Driver", unique_value, sep = ".")] <- ifelse(accidents$Sex_of_Driver == unique_value, 1, 0)
}
acc_binary$Sex_of_Driver <- matrix(
  as.integer(intToBits(as.integer(as.factor(accidents$Sex_of_Driver)))),
  ncol = 32,
  nrow = length(accidents$Sex_of_Driver),
  byrow = TRUE
)[, 1:ceiling(log(length(unique(accidents$Sex_of_Driver)) + 1)/log(2))]


#Driver_age_sex
acc_factor$Driver_age_sex = as.factor(accidents$Driver_age_sex)
for(unique_value in unique(accidents$Driver_age_sex)){
  acc_onehot[paste("Driver_age_sex", unique_value, sep = ".")] <- ifelse(accidents$Driver_age_sex == unique_value, 1, 0)
}
acc_binary$Driver_age_sex <- matrix(
  as.integer(intToBits(as.integer(as.factor(accidents$Driver_age_sex)))),
  ncol = 32,
  nrow = length(accidents$Driver_age_sex),
  byrow = TRUE
)[, 1:ceiling(log(length(unique(accidents$Driver_age_sex)) + 1)/log(2))]


#qualit L severity, month, time, age_driver
b = acc_factor
b$nb_Accident_Severity = as.numeric(factor(accidents$Accident_Severity, levels = c("Slight", "Serious", "Fatal")))
b$nb_month = as.numeric(factor(a$Date, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)))
b$nb_age_of_driver = as.numeric(factor(accidents$Age_Band_of_Driver, levels = c("Data missing or out of range", "6 - 10", "11 - 15", "16 - 20", "21 - 25", "26 - 35", "36 - 45", "56 - 65", "46 - 55", "66 - 75", "Over 75")))
b$nb_time = sapply(strsplit(accidents$Time,":"),
                   function(x) {
                     x <- as.numeric(x)
                     x[1]+x[2]/60
                   }
)
b$nb_day = as.numeric(factor(accidents$Day_of_Week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))
summary(b)

####################
# MCA              #
####################
c <- acc_factor
d <- acc_factor[, c(1, 2, 3, 4, 5, 10, 13, 6, 7, 8, 9, 12, 14)] #change order
glimpse(d)

res.mca <- MCA(d[-1], graph=TRUE)

summary(res.mca)

barplot(res.mca$eig[,2],main="Eigenvalues", names.arg=1:nrow(res.mca$eig))
plot(res.mca,invisible=c("var","quali.sup"),cex=.5,label="none",
     title="Graph of the individuals")
plot(res.mca,invisible=c("var","quali.sup"),cex=.5,label="none",
     title="Graph of the individuals", habillage="Accident_Severity")
plot(res.mca,invisible=c("var","quali.sup"),cex=.5,label="none",
     title="Graph of the individuals", habillage="Month")
plot(res.mca,invisible=c("var","quali.sup"),cex=.5,label="none",
     title="Graph of the individuals", habillage="Season")
plot(res.mca,invisible=c("var","quali.sup"),cex=.5,label="none",
     title="Graph of the individuals", habillage="Day_of_Week")
plot(res.mca,invisible=c("var","quali.sup"),cex=.5,label="none",
     title="Graph of the individuals", habillage="Time")
plot(res.mca,invisible=c("var","quali.sup"),cex=.5,label="none",
     title="Graph of the individuals", habillage="Age_Band_of_Driver")
plot(res.mca,invisible=c("var","quali.sup"),cex=.5,label="none",
     title="Graph of the individuals", habillage="Light_Conditions")
plot(res.mca,invisible=c("var","quali.sup"),cex=.5,label="none",
     title="Graph of the individuals", habillage="Road_Surface_Conditions")
plot(res.mca,invisible=c("var","quali.sup"),cex=.5,label="none",
     title="Graph of the individuals", habillage="Special_Conditions_at_Site")
plot(res.mca,invisible=c("var","quali.sup"),cex=.5,label="none",
     title="Graph of the individuals", habillage="Weather_Conditions")
plot(res.mca,invisible=c("var","quali.sup"),cex=.5,label="none",
     title="Graph of the individuals", habillage="Sex_of_Driver")
plot(res.mca,invisible=c("var","quali.sup"),cex=.5,label="none",
     title="Graph of the individuals", habillage="Driver_age_sex")

plot(res.mca,invis=c("ind","quali.sup"),col.var=c(rep(c("black","red", "green"),17),"black",rep("red",4)),
     title="Graph of the active categories")
plot(res.mca,invisible=c("ind","var"),hab="quali",
     palette=palette(c("blue","maroon","darkgreen","black","red")),
     title="Graph of the supplementary categories")
plot(res.mca,choix="var",title="Graph of the variables")
plot(res.mca,choix="quanti.sup",title="Graph of the continuous variables")
dimdesc(res.mca)

plotellipses(res.mca, cex=0.2, magnify=12, keepvar=2:12)

##############################
# Clustering
##############################

#library(PCAmix)
glimpse(d)
b <- subset(d, select = c(1, 2, 6, 7, 9, 10, 12, 13))  #select features

glimpse(b)
# factor to number
b$Accident_Severity <- acc_numeric$Accident_Severity
b$Time <- acc_numeric$Time
b$Age_Band_of_Driver <- acc_numeric$Age_Band_of_Driver
glimpse(b)


##########
# PAM    #
##########
gower_dist <- daisy(b[, -1], metric = "gower", type = list(logratio = 3))
summary(gower_dist)
gower_mat <- as.matrix(gower_dist)

#Silhouette: number of cluster PAM
sil <- c(NA)
for(i in 2:10){
  pam_fit <- pam(gower_mat, diss=TRUE, k=i)
  sil[i] <-pam_fit$silinfo$avg.width
}

plot(1:10, sil,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil)


#Cluster Interpretation
pam_fit <- pam(gower_dist, diss = TRUE, k = 3)
c <- data.frame(b, pam_fit$cluster)

# evaluate PAM clusters
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data_3 <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         id = a$Accident_Index)

ggplot(aes(x = X, y = Y), data = tsne_data_3) +  geom_point(aes(color = cluster))

# graph
str(si_3 <- silhouette(pam_fit))
(ssi <- summary(si))
plot(si_3, col = c("red", "green", "blue"))


# examine results in each class
pam_results <- d %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary


#########################
# Cluster Visualization #
#########################
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         id = a$Accident_Index)

ggplot(aes(x = X, y = Y), data = tsne_data) +  geom_point(aes(color = cluster))


########################
# OPTICS               
########################
library("dbscan")
(res_col <- optics(gower_dist, eps=0.5, minPts = 50))
plot(res_col)
abline(h=0.10, col ="red", lty=2)

(res_col_d <- extractDBSCAN(res_col, eps_cl =.14))
plot(res_col_d)


# examine results in each class
optic_results <- d %>%
  mutate(cluster = res_col_d$cluster) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
optic_results$the_summary

