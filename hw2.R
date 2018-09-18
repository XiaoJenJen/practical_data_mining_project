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

# 3.6
normData = c(200, 300, 400, 600, 1000)

minMax = (normData - min(normData)) / (max(normData) - min(normData))
minMax

zscore = (normData - mean(normData)) / sd(normData)
zscore

meanAbosluteDeviation = sum(abs(normData - mean(normData))) / length(normData)
zscoreMAD = (normData - mean(normData)) / meanAbosluteDeviation
zscoreMAD

nDigits = nchar(max(abs(normData)))
decimalScale = normData / (10^nDigits)
decimalScale

# 3.8
age_body <- data.frame(age = c(23,23,27,27,39,41,47,49,50,52,54,54,56,57,58,58,60,61), body = c(9.5,26.5,7.8,17.8,31.4,25.9,27.4,27.2,31.2,34.6,42.5,28.8,33.4,30.2,34.1,32.9,41.2,35.7))
age_body_norm = scale(age_body) 
pCoefficient = cor(age_body)
covariance = cov(age_body)
#cov = sum((age_body$age - mean(age_body$age)) * (age_body$body -  mean(age_body$body))) / nrow(age_body)

# 3.11
ageframe = as.data.frame(age)
ggplot(ageframe, aes(x = ageframe$age)) +
  scale_x_discrete(name = "age", limits = c(10,20,30,40,50,60,70)) +
  geom_histogram(binwidth = 10, boundary=10, color = "black", fill = "cyan")


