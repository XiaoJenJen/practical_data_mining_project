##################################
# Group Members: Daniel Truong   #
#                Jenny Xu        #
#                Ti-Tai Wang     #
#                Moonsung Kim    #
##################################

library(DMwR2)
set.seed(1234) 
data(iris)

#build a tree
ct1 <- rpartXse(Species ~ ., iris, se=1, cp=0, minsplit = 6, maxdepth = 10)
ct2 <- rpartXse(Species ~ ., iris, se=0, cp=0, minsplit = 6, maxdepth = 10)

#plot the trees
install.packages("rpart.plot")
library(rpart.plot)

#the tree can be displayed with a variety of features controlled by the parameters supplied to prp()
#e.g: type=0, only displays labels at terminal nodes, type=1 also for internal nodes
#extra=1 Display the number of observations that fall in the node (per class for class objects;)
#extra=3 Class models: misclassification rate at the node, expressed as the number of incorrect classifications and the number of observations in the node.
#extra=+100 Add 100 to any of the above to also display the percentage of observations in the node.
prp(ct1, type=0, extra=101, roundint = FALSE)
prp(ct2, type=1, extra=103, roundint = FALSE)

set.seed(1234)
nrow(iris)
rndSample <- sample(1:nrow(iris),100)
tr <- iris[rndSample, ]
#rest is the test examples
ts <- iris[-rndSample, ]
#build a tree
ct <- rpartXse(Species ~ ., tr, se=0.5)
#use ct to predict class labels for the test examples
ps1 <- predict(ct, ts) #this gives the probably of an instance belong to a class
head(ps1)
ps2 <- predict(ct, ts, type="class")
head(ps2)

#now lets create a contingency table and see how well the classifier works on test examples
#compare ps2 results (machine predication) with correct labels in test
(cm <- table(ps2, ts$Species))
#find the error rate in percentage
(1-sum(diag(cm))/sum(cm))

#Ensemble methods for classification
#prediction performance of a single tree is often not robust. Building many trees from the data set can improve the performance.  
set.seed(1234)
rndSample <- sample(1:nrow(iris),100)
tr <- iris[rndSample, ]
ts <- iris[-rndSample, ]

#AdaBoost bagging and boosting are for classification (not regression)
#bagging: learn trees on boostrapped samples using all variables
install.packages("adabag")
library(adabag)
ct <- bagging(Species ~ ., tr, mfinal=500)
ps2 <- predict(ct, ts)
ps2$confusion
ps2$error   #reduced from 0.06 to 0.04

#boosting:#iteratively add new models to the ensemble, each model tries to overcome the errors made by the previous model
ct <- boosting(Species ~ ., tr, mfinal=500)
ps2 <- predict(ct, ts) 
ps2$confusion
ps2$error

#############################################
#regression tree: based on http://uc-r.github.io/regression_trees
install.packages("rsample")
install.packages("ipred")
install.packages("AmesHousing") #dataset

library(rsample)     # data splitting 
library(dplyr)       # data wrangling
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ipred)       # bagging

# Create training (70%) and test (30%) sets for the AmesHousing::make_ames() data.
# Use set.seed for reproducibility
set.seed(123)
ames_split <- initial_split(AmesHousing::make_ames(), prop = .7)
ames_train <- training(ames_split)
ames_test  <- testing(ames_split)
#examine the structure of ames_train
str(ames_train)

m1 <- rpart(
     formula = Sale_Price ~ .,
     data    = ames_train,
     method  = "anova"
  )
m1 # format of a decision node: “node), split, n, deviance, yval”
#visualize the tree
prp(m1, type=1, extra=101, roundint = FALSE)
plotcp(m1)

m2 <- rpart(
     formula = Sale_Price ~ .,
     data    = ames_train,
     method  = "anova", 
     control = list(cp = 0, xval = 10)
   )
plotcp(m2)
abline(v = 12, lty = "dashed") #vertical line at x-value = 12.

#continue examining the model
m1$cptable

#tune the model: try out different combinations of minsplit and maxdepth values
#and compare the models
hyper_grid <- expand.grid(
       minsplit = seq(5, 20, 1),
       maxdepth = seq(8, 15, 1)
     )
head(hyper_grid)

#use a for-loop to run the combinations one by one and save the trees/models.
models <- list()
for (i in 1:nrow(hyper_grid)) {
       # get minsplit, maxdepth values at row i
       minsplit <- hyper_grid$minsplit[i]
       maxdepth <- hyper_grid$maxdepth[i]
        # train a model and store in the list
       models[[i]] <- rpart(
             formula = Sale_Price ~ .,
             data    = ames_train,
             method  = "anova",
             control = list(minsplit = minsplit, maxdepth = maxdepth)
           )
}

# function to get optimal cp
get_cp <- function(x) {
       min    <- which.min(x$cptable[, "xerror"])
       cp <- x$cptable[min, "CP"] 
}

# function to get minimum error
get_min_error <- function(x) {
       min    <- which.min(x$cptable[, "xerror"])
       xerror <- x$cptable[min, "xerror"] 
}

hyper_grid %>%
     mutate(
         cp    = purrr::map_dbl(models, get_cp),
         error = purrr::map_dbl(models, get_min_error)
       ) %>%
     arrange(error) %>%
     top_n(-5, wt = error)


#use the parameter combination that results in the least error to build the final tree
#because there are additional random factors in the tree building process, your results may not be exactly the same as mine. cp=0.01 was the default values used for all trees
optimal_tree <- rpart(
       formula = Sale_Price ~ .,
       data    = ames_train,
       method  = "anova",
       control = list(minsplit = 7, maxdepth = 8, cp = 0.01)
     )
pred <- predict(optimal_tree, newdata = ames_test)
pred

#model error: Root Mean Squared Error. on average, our predicted sales prices are about $39,145 off from the actual sales price.
RMSE(pred = pred, obs = ames_test$Sale_Price)

#Single tree models suffer from high variance: different random samples will likely produce different trees
#We see the prediction performance is not very good even after tuning. 
#Model ensembles are a way to reduce performance variations, for decision trees, random forest is a robust approach to improve tree prediction performace
  
#Bagging
#750 trees built to bootstrapped samples using all variables
rfm <- ipred::bagging(Sale_Price ~ ., ames_train, nbag=750)
rfpred <- predict(rfm, ames_test)
#error reduced from 39145 to 34536
RMSE(rfpred, ames_test$Sale_Price)

install.packages("randomForest")
library(randomForest)
#build model based on a forest of 750 trees
rfm <- randomForest(Sale_Price ~ ., ames_train, ntree=750)

#predict test data 
rfpred <- predict(rfm, ames_test)

#error reduced from 39145.39 to 24226.48.
RMSE(rfpred, ames_test$Sale_Price)

# Gradient boosting: like AdaBoost, but use gradient descent to address the current errors. Can overfit, so using cross-validation is very important (cv.folds)
install.packages("gbm")
library(gbm)
rfm <- gbm(Sale_Price ~ ., data=ames_train, n.trees=750, cv.folds=5)
rfpred <- predict(rfm, ames_test, n.trees=750)
RMSE(rfpred, ames_test$Sale_Price)










