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

##z-score
new_data_z <- c()
mean <- mean(data)
sd <- sd(data)

for(n in data) {
  n <- (n - mean) / (sd) 
  new_data_z <- c(new_data_z, n)
}

##z-score_mad
new_data_zmad <- c()
mad <- mad(s,center= mean(s))
for(n in data) {
  n <- (n - mean) / (mad) 
  new_data_zmad <- c(new_data_zmad, n)
}


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
new_fat <- z_score(fat)

##(B)
cor_value <- cor(age, fat, method = "pearson")
cov_value <- cov(age, fat, method = "pearson")

#3.11
##A
age_2 <- c(13, 15, 16, 16, 19, 20, 20, 21, 22, 22, 25, 25, 25, 25, 30, 33, 33, 35, 35, 35, 35, 36, 40, 45, 46, 52, 70) 
hist(age_2, xlab = "Age", main = "Equal-Width Histogram")
##B
library(sampling)


