library(arules)
library(dplyr)
library(arulesViz)


accidents = read.csv(file = "Accident_Information_London_C.csv",
                        header = TRUE,
                        sep = ",",
                        stringsAsFactors = FALSE)

lapply(accidents, class)
unique(accidents$Pedestrian_Crossing_Human_Control)
unique(accidents$Speed_limit)
unique(accidents$Accident_Severity)
unique(accidents$Light_Conditions)
unique(accidents$Pedestrian_Crossing_Physical_Facilities)
unique(accidents$Number_of_Casualties)
unique(accidents$Number_of_Vehicles)
unique(accidents$Date)

a = accidents
a$Pedestrian_Crossing_Human_Control = factor(accidents$Pedestrian_Crossing_Human_Control, labels=c("None", "School", "Other"))
a$Pedestrian_Crossing_Physical_Facilities = factor(accidents$Pedestrian_Crossing_Physical_Facilities, labels=c("None", "Zebra", 
                                                                                                               "Pelican", "Signal",
                                                                                                               "Footbridge/Subway",
                                                                                                               "CentralRefuge"))

a$Speed_limit = factor(accidents$Speed_limit, labels=c("Low", "MedLow", "HighLow", "High"))


a$Time = sapply(strsplit(accidents$Time,":"),
       function(x) {
         x <- as.numeric(x)
         x[1]+x[2]/60
       }
)

# Midnight - 6am = Night; 6am - Noon = Morning; Noon - 6pm = afternoon; 6pm - Midnight = evening
a$Time = cut(a$Time, breaks=4, labels=c("Night", "Morning", "Afternoon", "Evening"))

a$Date = sapply(strsplit(accidents$Date,"-"),
                function(x) {
                  x <- as.numeric(x)
                  x[2]
                }
)

a$Date = factor(a$Date, labels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

a$Number_of_Casualties = cut(a$Number_of_Casualties, breaks=4, labels=c("Low", "MedLow", "MedHigh", "High"))

a$Number_of_Vehicles[a$Number_of_Vehicles > 2] = "Multiple"
a$Number_of_Vehicles[a$Number_of_Vehicles == 1] = "Single"
a$Number_of_Vehicles[a$Number_of_Vehicles == 2] = "Double"

# Road_Surface_Conditions has the same value for ~85% of data
a = select(a, -c("Accident_Index", "Police_Force", "Road_Surface_Conditions"))
lapply(a, class)

cols = c(1, 3, 4, 6, 9, 10, 11, 12, 13, 14, 15)
for(i in cols) {
  a[,i] = as.factor(a[,i])
}

a = as(a, "transactions")

itemFrequencyPlot(a, support=0.3, cex.names=0.8)

ars <- apriori(a, parameter = list(support=0.025, confidence=0.75))
summary(ars)
inspect(head(subset(ars, subset=rhs %in% "Number_of_Casualties=Low"), 5, by="confidence"))
inspect(head(subset(ars, subset=rhs %in% "Number_of_Casualties=Low"), 5, by="support"))
inspect(head(subset(ars, subset=rhs %in% "Number_of_Casualties=Low"), 5, by="lift"))
inspect(head(subset(ars, subset=lhs %in% "Number_of_Vehicles=Single"), 5, by="confidence"))

somerules <- subset(ars, confidence>0.90 & support>0.5)
plot(somerules, method="grouped")
plot(somerules, method="matrix")
plot(somerules, method="graph", engine="htmlwidget")

freq.itemsets <- apriori(b, parameter=list(target="frequent itemsets", support=0.025))
inspect(head(subset(ars, subset=is.closed(freq.itemsets), 5, by="confidence")))

inspect(head(subset(ars, subset= size(lhs)<5 & size(lhs) >1), 5, by="support"))
inspect(head(subset(ars, subset= size(lhs)<5 & size(lhs) >1 & lift > 2), 20, by="confidence"))
inspect(head(subset(ars, subset=lift > 2), 20, by="confidence"))

closed = freq.itemsets[is.closed(freq.itemsets)]
summary(closed) #inspect(closed) list all closed itemsets

# Interesting Rules:
# {Number_of_Vehicles=Single, Road_Type=One way street} => {Number_of_Casualties=Low} 
# Support = 0.02693438; Confidence = 1; lift = 1.005169; Count = 110
#
# This rule says that if the accident occurs on a one way street and there is only one vehicle involved,
# then the number of casualties is low. This could be suggesting that pedestrians are more likely to be
# hit by cars on one way streets.

# {Number_of_Casualties=Low, Urban_or_Rural_Area=Urban} => {Pedestrian_Crossing_Human_Control=None} 
# Support = 0.9843291; Confidence = 0.9987578; Lift = 0.9999820; Count = 4020