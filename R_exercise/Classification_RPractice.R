accidents = read.csv(file = "Accident_Information_London_C.csv",
                     header = TRUE,
                     sep = ",",
                     stringsAsFactors = FALSE)

#Explore Dataset
colnames(accidents)
lapply(accidents, class)

#Drop ID and Police Force(only contains one variable)
a = subset(accidents, select = -c(1, 10, 18))

#transform character into integer
unique(accidents$Accident_Severity)
a$Accident_Severity = factor(accidents$Accident_Severity, levels = c("Slight", "Serious", "Fatal"),
                                                          labels = c(1, 2, 3))
unique(a$Accident_Severity)

#transform Date into 4 seasons
unique(accidents$Date)
a$Date = sapply(strsplit(accidents$Date,"-"),
                function(x) {
                  x <- as.numeric(x)
                  x[2]
                }
)
a$Date = factor(a$Date, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                        labels = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4))
unique(a$Date)

unique(accidents$Day_of_Week)
a$Day_of_Week = factor(accidents$Day_of_Week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                             labels = c(1, 2, 3, 4, 5, 6, 7))
unique(a$Day_of_Week)

unique(accidents$Light_Conditions)
a$Light_Conditions = factor(accidents$Light_Conditions, levels = c("Daylight", "Darkness - lights lit", "Darkness - lighting unknown", "Darkness - lights unlit"),
                             labels = c(1, 2, 3, 4))
unique(a$Light_Conditions)

unique(accidents$Road_Surface_Conditions)
a$Road_Surface_Conditions = factor(accidents$Road_Surface_Conditions, levels = c("Data missing or out of range", "Dry", "Wet or damp", "Frost or ice", "Snow"),
                            labels = c(0, 1, 2, 3, 4))
unique(a$Road_Surface_Conditions)

unique(accidents$Road_Type)
a$Road_Type = factor(accidents$Road_Type, levels = c("Unknown", "Single carriageway", "Dual carriageway", "One way street", "Roundabout", "Slip road"),
                                   labels = c(0, 1, 2, 3, 4, 5))
unique(a$Road_Type)

unique(accidents$Special_Conditions_at_Site)
a$Special_Conditions_at_Site = factor(accidents$Special_Conditions_at_Site, levels = c("Data missing or out of range", "None", "Road surface defective", "Roadworks", "Auto traffic signal - out", "Oil or diesel", "Auto signal part defective", "Road sign or marking defective or obscured"),
                     labels = c(0, 1, 2, 3, 4, 5, 6, 7))
unique(a$Special_Conditions_at_Site)

unique(accidents$Time)
a$Time = sapply(strsplit(accidents$Time,":"),
                function(x) {
                  x <- as.numeric(x)
                  x[1]+x[2]/60
                }
)
# Midnight - 6am = Night; 6am - Noon = Morning; Noon - 6pm = afternoon; 6pm - Midnight = evening
a$Time = cut(a$Time, breaks=4, labels=c(4, 1, 2, 3))

unique(accidents$Urban_or_Rural_Area)
a$Urban_or_Rural_Area = factor(accidents$Urban_or_Rural_Area, levels = c("Rural", "Urban"),
                                      labels = c(1, 2))
unique(a$Urban_or_Rural_Area)

unique(accidents$Weather_Conditions)
a$Weather_Conditions = factor(accidents$Weather_Conditions, levels = c("Unknown", "Other", "Fine no high winds", "Fine + high winds", "Raining no high winds", "Raining + high winds", "Snowing no high winds", "Fog or mist"),
                                      labels = c(0, 1, 2, 3, 4, 5, 6, 7))
unique(a$Weather_Conditions)

a = subset(a, select = c(1, 4, 5, 10, 13, 15))

library(rpart)
#Build a Tree
##Split Data (train:3684/test:400)
set.seed(123)
nrow(a)
rndSample <- sample(1:nrow(a),3684)                                                                                                
tr <- a[rndSample, ]
#rest is the test examples
ts <- a[-rndSample, ]
#build a tree
ct <- rpart(Accident_Severity ~ ., data = tr, method = "class", control=rpart.control(minsplit=1, minbucket=1, cp=0.001))
#use ct to predict class labels for the test examples
ps1 <- predict(ct, ts) #this gives the probably of an instance belong to a class
head(ps1)
ps2 <- predict(ct, ts, type="class")
head(ps2)
table(a$Accident_Severity)

(cm <- table(ps2, ts$Accident_Severity))
(1-sum(diag(cm))/sum(cm))

library(rpart.plot)
prp(ct, type=0, extra=101, roundint = FALSE)

# Ensemble method
library(adabag)
# 500 trees will be too mouch
ctbag <- bagging(Accident_Severity ~ ., tr, mfinal = 100, control=rpart.control(minsplit=1, minbucket=1, cp=0.001))
ps2bag <- predict(ctbag, ts)
ps2bag$confusion
ps2bag$error   #reduced from 0.1375 to 0.1375

#boosting:#iteratively add new models to the ensemble, each model tries to overcome the errors made by the previous model
ctboo <- boosting(Accident_Severity ~ ., tr, mfinal = 100, control=rpart.control(minsplit=1, minbucket=1, cp=0.001))
ps2boo <- predict(ctboo, ts) 
ps2boo$confusion
ps2boo$error

# In this part, we want to know "if this accident is serious or not?",
# so we tried to predict Accident_Severity with three levels: "Slight:1", "Serious:2, Fatal:3". 
# However, the prediction almost everytime shows "slight" as the result, since "Slight" accidents are 85% of all.
# Therefore, we tried to balance our dataset and drop unrelated features (with too many features, 
# running time would icrease dramatically), but it doesn't work well.
# It seems that our features are not very relevant to Accident_Severity, because I only got one root at first,
# and then I tried to use the most loose control, the tree was finally built successfully.

# We ran it several times, and it is different for every time (large variance). Based on our tree plots, 
# the features: "Number of Casualties", "Weather_Condition", "Time", "Road_Tupe" are most relevant.
# According to the tree, this result suggests that when casualties greater than 1.5, 
# it is a serious accidents (obviously), and when the weather is rainy or snowy (other than fine), 
# it is more likely a serious accident. Another interesting thing is that we assume that serious car
# accidents happened during the midenight at first. However, the result showed that  the slight accidents happened 
# in midnight and afternoon.

