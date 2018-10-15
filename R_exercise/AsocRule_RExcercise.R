library(arules)
library(dplyr)
library(arulesViz)

data(Boston, package = "MASS")
lapply(Boston, class)

hist(Boston$chas)

unique(Boston$chas)
unique(Boston$rad)

b <- Boston
b$chas <- factor(Boston$chas, labels=c("river", "noriver"))
b$rad <- factor(Boston$rad)

b$black <- cut(Boston$black, breaks=4, labels=c(">31.5%", "18.5-31.5%", "8-18.5%", "<8%"))
discr <-function(x) cut(x, breaks=4, labels=c("low", "medLow", "medHigh", "High"))
b <- select(b, -c("chas", "rad", "black")) %>% mutate_all(funs(discr)) %>% bind_cols(select(b, c("chas", "rad", "black")))

dim(b)
summary(b)

b <- as(b, "transactions")
colnames(b)
summary(b)

itemFrequencyPlot(b, support=0.3, cex.names=0.8)

ars <- apriori(b, parameter = list(support=0.025, confidence=0.75))
summary(ars)
inspect(head(subset(ars, subset=rhs %in% "medv=High"), 5, by="confidence"))
inspect(head(subset(ars, subset=rhs %in% "medv=low"), 5, by="confidence"))
inspect(head(subset(ars, subset=rhs %in% "nox=High" | lhs %in% "nox=High"), 5, by="confidence"))
inspect(head(subset(ars, subset=rhs %in% "medv=High"), 5, by="support"))
inspect(head(subset(ars, subset=is.maximal(ars), 5, by="confidence")))


freq.itemsets <- apriori(b, parameter=list(target="frequent itemsets", support=0.025))
inspect(head(subset(ars, subset=is.closed(freq.itemsets), 5, by="confidence")))

closed = freq.itemsets[is.closed(freq.itemsets)]
summary(closed)

maximal = freq.itemsets[is.maximal(freq.itemsets)]
summary(maximal)

inspect(head(subset(ars, subset= size(lhs)<5 & size(lhs) >1), 5, by="support"))
inspect(head(subset(ars, subset= size(lhs)<5 & size(lhs) >1 & lift > 2), 5, by="support"))

#plot(ars, engine="interactive")
plot(ars, engine="htmlwidget")

somerules <- subset(ars, subset=size(lhs)> 1 & confidence>0.90 & support>0.5)
plot(somerules, method="grouped")
plot(somerules, method="matrix")
plot(somerules, method="graph", engine="htmlwidget")