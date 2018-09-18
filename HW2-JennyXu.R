# 3.6
data <- as.matrix(c(200, 300, 400, 600, 1000))
# b) z-score normalization
data.norm1 <- scale(data) 
min <- min(data)
max <- max(data)
# a) min-max normalization by setting min = 0 and max = 1
data.norm2 <- scale(data, center = min, scale = max - min) 
# c) z-score normalization using the mean absolute deviation instead of standard deviation
data.norm3 <- scale(data, center = mean(data), scale = mean(abs(data-mean(data))))
# d) normalization by decimal scaling
data.norm4 <- scale(data, center = 0, scale = 10000) 

# 3.11
set.seed(1)
age = c(13, 15, 16, 16, 19, 20, 20, 21, 22, 22, 25, 25, 25, 25, 30, 33, 33, 35, 35, 35, 35, 36, 40, 45, 46, 52, 70)
ageframe = as.data.frame(age)
# SRSWOR
sample(age, 5)
# SRSWR
sample(age, 5, replace = TRUE)
# Cluster sampling
library(sampling)
cluster(ageframe, clustername = "age", size = 5)
# Stratified sampling
library(dplyr)
s <- kmeans(age, 2)
s
ageframe$condition <- factor(c(rep("youth", 15), rep("middle-aged", 12)))
ageframe %>% group_by(condition) %>% sample_n(5)