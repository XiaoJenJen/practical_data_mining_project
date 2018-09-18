# 3.3
age = c(13, 15, 16, 16, 19, 20, 20, 21, 22, 22, 25, 25, 25, 25, 30, 33, 33, 35, 35, 35, 35, 36, 40, 45, 46, 52, 70)
# Separate data into bins of depth 3
bins = matrix(age, nrow = length(age) / 3, byrow = TRUE)
# Find the average of each bin
bin_means = apply(bins, 1, FUN = mean)
# Replace values with their bin mean
for (i in seq(1:nrow(bins))) {
  bins[i,] = bin_means[i]
}
age_bin_mean_smoothed = round(as.vector(t(bins)), 2)
age_bin_mean_smoothed

mean(age)
mean(age_bin_mean_smoothed)
summary(age)
summary(age_bin_mean_smoothed)


#3.6
data <- c(200, 300, 400, 600, 1000)

##min-max
new_data_mm <- c()
new_max <- 1.0
new_min <- 0.0
min <- min(data)
max <- max(data)

for(n in data) {
  n <- (n - min) / (max - min) * (new_max - new_min) + new_min
  new_data_mm <- c(new_data_mm, n)
}
new_data_mm

##z-score
new_data_z <- c()
mean <- mean(data)
sd <- sd(data)

for(n in data) {
  n <- (n - mean) / (sd) 
  new_data_z <- c(new_data_z, n)
}
new_data_z

##z-score_mad
meanAbosluteDeviation = sum(abs(data - mean(data))) / length(data)
zscoreMAD = (data - mean(data)) / meanAbosluteDeviation
zscoreMAD

##decimal
nDigits = nchar(max(abs(data)))
decimalScale = data / (10^nDigits)
decimalScale

#3.8
age <- c(23, 23, 27, 27, 39, 41, 47, 49, 50, 52, 54, 54, 56, 57, 58, 58, 60, 61)
fat <- c(9.5, 26.5, 7.8, 17.8, 31.4, 25.9, 27.4, 27.2, 31.2, 34.6, 42.5, 28.8, 33.4, 30.2, 34.1, 32.9, 41.2, 35.7)

##(A)
z_score <- function(data) {
  
  new_data_z <- c()
  mean <- mean(data)
  sd <- sd(data)
  
  for(n in data) {
    n <- (n - mean) / (sd) 
    new_data_z <- c(new_data_z, n)
  }
  
  return(new_data_z)
}

new_age <- z_score(age)
new_age
new_fat <- z_score(fat)
new_fat

##(B)
cor_value <- cor(age, fat, method = "pearson")
cor_value
cov_value <- cov(age, fat, method = "pearson")
cov_value

#3.11
##A
set.seed(1)
age <- c(13, 15, 16, 16, 19, 20, 20, 21, 22, 22, 25, 25, 25, 25, 30, 33, 33, 35, 35, 35, 35, 36, 40, 45, 46, 52, 70) 
hist(age, xlab = "Age", main = "Equal-Width Histogram")
##B
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
s$cluster
ageframe$condition <- factor(c(rep("youth", 15), rep("middle-aged", 12)))
ageframe %>% group_by(condition) %>% sample_n(5)


