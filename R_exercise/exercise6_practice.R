##################################
# Group Members: Daniel Truong   #
#                Jenny Xu        #
#                Ti-Tai Wang     #
#                Moonsung Kim    #
##################################

library(dplyr)
library(ISLR)
library(cluster)
library(Rtsne)
library(ggplot2) 

####################
# Data preparation #
####################
accidents = read.csv(file = "../Accident_Information_London_C.csv",
                     header = TRUE,
                     sep = ",",
                     stringsAsFactors = FALSE)

# Explore Dataset
colnames(accidents)
lapply(accidents, class)

a = accidents

unique(accidents$Accident_Severity)
a$Accident_Severity = factor(accidents$Accident_Severity, levels = c("Slight", "Serious", "Fatal"),
                             labels = c("Slight", "Serious", "Fatal"))
unique(a$Accident_Severity)

#transform Date into 4 seasons
#unique(accidents$Date)
a$Date = sapply(strsplit(accidents$Date,"-"),
                function(x) {
                  x <- as.numeric(x)
                  x[2]
                }
)
a$Date = factor(a$Date, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                labels = c("Winter", "Winter", "Spring", "Spring", "Spring", "Summer", "Summer", "Summer", "Autumn", "Autumn", "Autumn", "Winter"))
unique(a$Date)

#transform Day_of_Week into weekday and weekend
unique(accidents$Day_of_Week)
a$Day_of_Week = factor(accidents$Day_of_Week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                       labels = c("Weekday", "Weekday", "Weekday", "Weekday", "Weekday", "Weekend", "Weekend")) #labels = c(1, 2, 3, 4, 5, 6, 7))
unique(a$Day_of_Week)

#transform Light_Conditions into daylight and darkness
unique(accidents$Light_Conditions)
a$Light_Conditions = factor(accidents$Light_Conditions, levels = c("Daylight", "Darkness - lights lit", "Darkness - lighting unknown", "Darkness - lights unlit"),
                            #labels = c("Daylight", "Darkness", "Darkness", "Darkness")) #labels = c(1, 2, 3, 4))
                            labels = c("Daylight", "Darkness - lights lit", "Darkness - lighting unknown", "Darkness - lights unlit")) #labels = c(1, 2, 3, 4))
unique(a$Light_Conditions)

unique(accidents$Road_Surface_Conditions)
a$Road_Surface_Conditions = factor(accidents$Road_Surface_Conditions, levels = c("Data missing or out of range", "Dry", "Wet or damp", "Frost or ice", "Snow"),
                                   labels = c("Data missing or out of range", "Dry", "Wet or damp", "Frost or ice", "Snow"))
unique(a$Road_Surface_Conditions)

unique(accidents$Road_Type)
a$Road_Type = factor(accidents$Road_Type, levels = c("Unknown", "Single carriageway", "Dual carriageway", "One way street", "Roundabout", "Slip road"),
                     labels = c("Unknown", "Single carriageway", "Dual carriageway", "One way street", "Roundabout", "Slip road")) 
unique(a$Road_Type)

#transform Special_Conditions_at_Site into none and special
unique(accidents$Special_Conditions_at_Site)
a$Special_Conditions_at_Site = factor(accidents$Special_Conditions_at_Site, levels = c("Data missing or out of range", "None", "Road surface defective", "Roadworks", "Auto traffic signal - out", "Oil or diesel", "Auto signal part defective", "Road sign or marking defective or obscured"),
                                      labels = c("None", "Special", "Special", "Special", "Special", "Special", "Special", "Special")) #labels = c(0, 1, 2, 3, 4, 5, 6, 7))
unique(a$Special_Conditions_at_Site)

unique(accidents$Time)
a$Time = sapply(strsplit(accidents$Time,":"),
                function(x) {
                  x <- as.numeric(x)
                  x[1]+x[2]/60
                }
)
# Midnight - 6am = Night; 6am - Noon = Morning; Noon - 6pm = afternoon; 6pm - Midnight = evening
a$Time = cut(a$Time, breaks=4, labels=c("Night", "Morning", "Afternoon", "Evening"))
unique(a$Time)

unique(accidents$Weather_Conditions)
a$Weather_Conditions = factor(accidents$Weather_Conditions, levels = c("Unknown", "Other", "Fine no high winds", "Fine + high winds", "Raining no high winds", "Raining + high winds", "Snowing no high winds", "Fog or mist"),
                              labels = c("Unknown", "Unknown", "Fine", "Fine", "Bad", "Bad", "Bad", "Bad")) #labels = c(0, 1, 2, 3, 4, 5, 6, 7))
unique(a$Weather_Conditions)

#select subset
b = subset(a, select = c(3, 4, 5, 7, 11, 12, 15, 17))
glimpse(b)


#######
# PAM #
#######

#compute the distance matrix using ‘gower’
gower_dist <- daisy(b[, -1], metric = "gower", type = list(logratio = 3))
summary(gower_dist)
gower_mat <- as.matrix(gower_dist)


# number of cluster PAM
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
pam_fit <- pam(gower_dist, diss = TRUE, k = 6)
result1 <- c %>% mutate(id = a$Accident_Index) %>% dplyr::select(id, pam_fit.cluster)
View(result1)
c <- data.frame(b, pam_fit$cluster)

pam_results <- b %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

#Output medoids
b[pam_fit$medoids, ]


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


#######################
# Hierarchical method # 
#######################
h <- hclust(gower_dist, method="average")
plot(h, hang=-0.1)
clus6 <- cutree(h, 6)

# compare DIANA results to PAM
d <- diana(gower_dist)
d6 <- cutree(d, 6)
table(result1$pam_fit.cluster, d6)

# examine results in each class
t_c1 <- which(d6 %in% c(1))
b[t_c1,]
t_c2 <- which(d6 %in% c(2))
b[t_c2,]
t_c3 <- which(d6 %in% c(3))
b[t_c3,]
t_c4 <- which(d6 %in% c(4))
b[t_c4,]
t_c5 <- which(d6 %in% c(5))
b[t_c5,]
t_c6 <- which(d6 %in% c(6))
b[t_c6,]

########################
# Density-base methods # 
########################
library("dbscan")
(res_col <- optics(gower_dist, eps=50, minPts = 100))
plot(res_col)
(res_col_d <- extractDBSCAN(res_col, eps_cl=0.1)) #6 Cluster, 961 noise points
(res_col_h <- extractXi(res_col, xi=0.1)) #14 Cluster, 0 noise points
res_col_h$clusters_xi

# examine results in each class
db_1 <-  which(res_col_d$cluster %in% c(0))
db_2 <-  which(res_col_d$cluster %in% c(1))
db_3 <-  which(res_col_d$cluster %in% c(2))
db_4 <-  which(res_col_d$cluster %in% c(3))
db_5 <-  which(res_col_d$cluster %in% c(4))
db_6 <-  which(res_col_d$cluster %in% c(5))
b[db_1,]
b[db_2,]
b[db_3,]
b[db_4,]
b[db_5,]
b[db_6,]

# When we run PAM on our dataset multiple times without setting seed, silhouette width has no change but overlaps of observations in different clustering is changed every time.
# This is because the algorithm set initial points and groups randomly.

# Some of clusters are meaningful but some are not.
# C2: acciednts occured in winter morning.
# C4: accidents by alone (e.g., associated with only 1 vehicle) occured in weekend night.
# C6: accidents under bad road contitions occured in the weekday morning.

# hierarchical method has more explainable than PAM results
# d1: accidents under special conditions
# d2: accidents at daylight time
# d3: accidents in weekend and wet_damp road conditions
# d4: accidents in weekday and wet_damp road conditions
# d5: accidents when wether condition is unknown
# d6: accidents at night time

# The cluster results of Density-based methods seperates into cluster pretty well.
# db1: overlapped observations
# db2: accidents on [weekday, darkness, dry road, single way, evening, fine wether]
# db3: accidents on [weekday, daylight, dry road, single way, morning, fine wether]
# db4: accidents on [weekday, daylight, dry road, single way, afternoon, fine wether]
# db5: accidents on [weekday, darkness light, dry road, dual way, afternoon, fine wether]
# db6: accidents on [weekday, darkness light, dry road, dual way, morning, fine wether]