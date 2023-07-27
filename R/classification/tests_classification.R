library(caret)

library(caTools)
library(dplyr)

library(randomForest)








############################
## rf data
###########################

#Getting data
dataset <- output

dataset


dataset <- dataset[ , -which(names(dataset) %in% c("inst","minbestcost"))]

dataset <- dataset[ , -which(names(dataset) %in% c("inst","minbestcost","typeD","typeF","NW"))]

dataset <- dataset[ , -which(names(dataset) %in% c("inst","minbestcost","typeD","typeF","NR","NW","balanced"))]

#dataset <- dataset[ , -which(names(dataset) %in% c("inst","minaverageconf"))]

dataset <- subset(dataset,dataset$minbestconf != "c_2_2")


dataset$minbestconf <- as.factor(dataset$minbestconf)
dataset$balanced <- as.factor(dataset$balanced)
dataset$typeF <- as.factor(dataset$typeF)
dataset$typeD <- as.factor(dataset$typeD)


dataset$NT <- as.factor(dataset$NT)
dataset$NW <- as.factor(dataset$NW)
dataset$NR <- as.factor(dataset$NR)


# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$minbestconf, p=0.85, list=FALSE)
# select 20% of the data for validation
validation <- dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]


dataset


names(dataset)

# dimensions of dataset
dim(dataset)

# list types for each attribute
sapply(dataset, class)

# take a peek at the first 5 rows of the data
head(dataset)


# list the levels for the class
levels(dataset$minbestconf)


# summarize the class distribution
percentage <- prop.table(table(dataset$minbestconf)) * 100
cbind(freq=table(dataset$minbestconf), percentage=percentage)


# summarize attribute distributions
summary(dataset)




# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"


# a) linear algorithms
set.seed(7)
fit.lda <- caret::train(minbestconf~., data=dataset, method="lda", metric=metric, trControl=control)

# 
set.seed(7)
fit.sda <- caret::train(minbestconf~., data=dataset, method="sda", metric=metric, trControl=control)

# a) linear algorithms
set.seed(7)
fit.slda <- caret::train(minbestconf~., data=dataset, method="slda", metric=metric, trControl=control)


#2 (pda) Penalized Discriminant Analysis
set.seed(7)
fit.pda <- caret::train(minbestconf~., data=dataset, method="pda", metric=metric, trControl=control)

# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- caret::train(minbestconf~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- caret::train(minbestconf~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- caret::train(minbestconf~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- caret::train(minbestconf~., data=dataset, method="rf", metric=metric, trControl=control)
# AdaBoost 
set.seed(7)
fit.gbm <- caret::train(minbestconf~., data=dataset, method="gbm", metric=metric, trControl=control)

# AdaBoost 
set.seed(7)
fit.glm <- caret::train(minbestconf~., data=dataset, method="glm", metric=metric, trControl=control)

# AdaBoost 
set.seed(7)
fit.glmboost <- caret::train(minbestconf~., data=dataset, method="glmboost", metric=metric, trControl=control)


# summarize accuracy of models
#results <- resamples(list(lda=fit.lda,sda=fit.sda,slda=fit.slda,pda=fit.pda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf,gbm=fit.gbm,glm=fit.glm,glmboost=fit.glmboost))
results <- resamples(list(lda=fit.lda,sda=fit.sda,slda=fit.slda,pda=fit.pda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)




# compare accuracy of models
dotplot(results)


# summarize Best Model
print(fit.lda)
print(fit.gbm)
print(fit.pda)

print(fit.rf)

# estimate skill of LDA on the validation dataset
predictions <- predict(fit.rf, validation)

length(predictions)

length(validation$minbestconf)

confusionMatrix(predictions, validation$minbestconf)











############################
## dpheur data
###########################

#Getting data
dataset <- output

dataset


dataset <- dataset[ , -which(names(dataset) %in% c("inst","minaverageconf","balanced","NT"))]

#dataset <- dataset[ , -which(names(dataset) %in% c("inst","minaverageconf"))]

dataset <- subset(dataset,dataset$minbestconf != "c_0.2_0.85_0.7_0.7")


dataset$minbestconf <- as.factor(dataset$minbestconf)
dataset$balanced <- as.factor(dataset$balanced)
dataset$NT <- as.factor(dataset$NT)
dataset$NW <- as.factor(dataset$NW)
dataset$NR <- as.factor(dataset$NR)


# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$minbestconf, p=0.70, list=FALSE)
# select 20% of the data for validation
validation <- dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]


dataset


names(dataset)

# dimensions of dataset
dim(dataset)

# list types for each attribute
sapply(dataset, class)

# take a peek at the first 5 rows of the data
head(dataset)


# list the levels for the class
levels(dataset$minbestconf)


# summarize the class distribution
percentage <- prop.table(table(dataset$minbestconf)) * 100
cbind(freq=table(dataset$minbestconf), percentage=percentage)


# summarize attribute distributions
summary(dataset)




# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"


# a) linear algorithms
set.seed(7)
fit.lda <- caret::train(minbestconf~., data=dataset, method="lda", metric=metric, trControl=control)

# 
set.seed(7)
fit.sda <- caret::train(minbestconf~., data=dataset, method="sda", metric=metric, trControl=control)

# a) linear algorithms
set.seed(7)
fit.slda <- caret::train(minbestconf~., data=dataset, method="slda", metric=metric, trControl=control)


#2 (pda) Penalized Discriminant Analysis
set.seed(7)
fit.pda <- caret::train(minbestconf~., data=dataset, method="pda", metric=metric, trControl=control)

# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- caret::train(minbestconf~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- caret::train(minbestconf~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- caret::train(minbestconf~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- caret::train(minbestconf~., data=dataset, method="rf", metric=metric, trControl=control)
# AdaBoost 
set.seed(7)
fit.gbm <- caret::train(minbestconf~., data=dataset, method="gbm", metric=metric, trControl=control)

# AdaBoost 
set.seed(7)
fit.glm <- caret::train(minbestconf~., data=dataset, method="glm", metric=metric, trControl=control)

# AdaBoost 
set.seed(7)
fit.glmboost <- caret::train(minbestconf~., data=dataset, method="glmboost", metric=metric, trControl=control)


# summarize accuracy of models
#results <- resamples(list(lda=fit.lda,sda=fit.sda,slda=fit.slda,pda=fit.pda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf,gbm=fit.gbm,glm=fit.glm,glmboost=fit.glmboost))
results <- resamples(list(lda=fit.lda,sda=fit.sda,slda=fit.slda,pda=fit.pda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)




# compare accuracy of models
dotplot(results)


# summarize Best Model
print(fit.lda)
print(fit.gbm)
print(fit.pda)

print(fit.rf)

# estimate skill of LDA on the validation dataset
predictions <- predict(fit.knn, validation)

length(predictions)

length(validation$minbestconf)

confusionMatrix(predictions, validation$minbestconf)
































#######################
## iris data
########################
# attach the iris dataset to the environment
data(iris)
# rename the dataset
dataset <- iris

# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]


# dimensions of dataset
dim(dataset)

# list types for each attribute
sapply(dataset, class)

# take a peek at the first 5 rows of the data
head(dataset)


# list the levels for the class
levels(dataset$Species)


# summarize the class distribution
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentage=percentage)


# summarize attribute distributions
summary(dataset)


# split input and output
x <- dataset[,1:4]
y <- dataset[,5]


# boxplot for each attribute on one image
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}

# barplot for class breakdown
plot(y)

# scatterplot matrix
featurePlot(x=x, y=y, plot="ellipse")


# box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")

# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)



# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"


# a) linear algorithms
set.seed(7)
fit.lda <- caret::train(Species~., data=dataset, method="lda", metric=metric, trControl=control)

# 
set.seed(7)
fit.sda <- caret::train(Species~., data=dataset, method="sda", metric=metric, trControl=control)

# a) linear algorithms
set.seed(7)
fit.slda <- caret::train(Species~., data=dataset, method="slda", metric=metric, trControl=control)


#2 (pda) Penalized Discriminant Analysis
set.seed(7)
fit.pda <- caret::train(Species~., data=dataset, method="pda", metric=metric, trControl=control)

# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- caret::train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- caret::train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- caret::train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- caret::train(Species~., data=dataset, method="rf", metric=metric, trControl=control)
# AdaBoost 
set.seed(7)
fit.gbm <- caret::train(Species~., data=dataset, method="gbm", metric=metric, trControl=control)



# summarize accuracy of models
results <- resamples(list(lda=fit.lda,sda=fit.sda,slda=fit.slda,pda=fit.pda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf,gbm=fit.gbm))
summary(results)




# compare accuracy of models
dotplot(results)


# summarize Best Model
print(fit.lda)


dim(validation)

# estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)










############################
## titanic data
###########################

#Getting data
titanic_data <- "https://goo.gl/At238b" %>%  #DataFlair
  read.csv %>% # read in the data
  select(survived, embarked, sex, 
         sibsp, parch, fare) %>%
  mutate(embarked = factor(embarked),
         sex = factor(sex))
dataset <- titanic_data

dataset <- dataset[complete.cases(dataset), ]

dataset$survived <- as.factor(dataset$survived)

# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$survived, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]







# dimensions of dataset
dim(dataset)

# list types for each attribute
sapply(dataset, class)

# take a peek at the first 5 rows of the data
head(dataset)


# list the levels for the class
levels(dataset$survived)


# summarize the class distribution
percentage <- prop.table(table(dataset$survived)) * 100
cbind(freq=table(dataset$survived), percentage=percentage)


# summarize attribute distributions
summary(dataset)




# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"


# a) linear algorithms
set.seed(7)
fit.lda <- caret::train(survived~., data=dataset, method="lda", metric=metric, trControl=control)

# 
set.seed(7)
fit.sda <- caret::train(survived~., data=dataset, method="sda", metric=metric, trControl=control)

# a) linear algorithms
set.seed(7)
fit.slda <- caret::train(survived~., data=dataset, method="slda", metric=metric, trControl=control)


#2 (pda) Penalized Discriminant Analysis
set.seed(7)
fit.pda <- caret::train(survived~., data=dataset, method="pda", metric=metric, trControl=control)

# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- caret::train(survived~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- caret::train(survived~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- caret::train(survived~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- caret::train(survived~., data=dataset, method="rf", metric=metric, trControl=control)
# AdaBoost 
set.seed(7)
fit.gbm <- caret::train(survived~., data=dataset, method="gbm", metric=metric, trControl=control)

# AdaBoost 
set.seed(7)
fit.glm <- caret::train(survived~., data=dataset, method="glm", metric=metric, trControl=control)

# AdaBoost 
set.seed(7)
fit.glmboost <- caret::train(survived~., data=dataset, method="glmboost", metric=metric, trControl=control)


# summarize accuracy of models
results <- resamples(list(lda=fit.lda,sda=fit.sda,slda=fit.slda,pda=fit.pda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf,gbm=fit.gbm,glm=fit.glm,glmboost=fit.glmboost))
summary(results)




# compare accuracy of models
dotplot(results)


# summarize Best Model
print(fit.lda)
print(fit.gbm)


# estimate skill of LDA on the validation dataset
predictions <- predict(fit.rf, validation)

length(predictions)

length(validation$survived)

confusionMatrix(predictions, validation$survived)





library(DiagrammeR)

mermaid("
gantt
dateFormat  YYYY-MM-DD
title A Very Nice Gantt Diagram

section Basic Tasks
This is completed             :done,          first_1,    2014-01-06, 2014-01-08
This is active                :active,        first_2,    2014-01-09, 3d
Do this later                 :               first_3,    after first_2, 5d
Do this after that            :               first_4,    after first_3, 5d

section Important Things
Completed, critical task      :crit, done,    import_1,   2014-01-06,24h
Also done, also critical      :crit, done,    import_2,   after import_1, 2d
Doing this important task now :crit, active,  import_3,   after import_2, 3d
Next critical task            :crit,          import_4,   after import_3, 5d

section The Extras
First extras                  :active,        extras_1,   after import_4,  3d
Second helping                :               extras_2,   after extras_1, 20h
More of the extras            :               extras_3,   after extras_1, 48h
")




library(timevis)

data <- data.frame(
  id      = 1:4,
  content = c("Item one"  , "Item two"  ,"Ranged item", "Item four"),
  start   = c("2016-01-10", "2016-01-11", "2016-01-20", "2016-02-14 15:00:00"),
  end     = c(NA          ,           NA, "2016-02-04", NA)
)

timevis(data)
















