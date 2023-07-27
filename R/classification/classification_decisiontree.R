
#Author DataFlair
library(rpart)
library(readr)
library(caTools)
library(dplyr)
library(party)
library(partykit)
library(rpart.plot)

library(MASS)
library(rpart)
library(cvTools) 
library(caret)
require(mlr)
require(randomForest)
require(rpart.plot)
require(dplyr)
require(corrplot)



#Getting data
titanic_data <- "https://goo.gl/At238b" %>%  #DataFlair
  read.csv %>% # read in the data
  select(survived, embarked, sex, 
         sibsp, parch, fare) %>%
  mutate(embarked = factor(embarked),
         sex = factor(sex))


#Splitting the data into training and testing sets
set.seed(123)
sample_data = sample.split(titanic_data, SplitRatio = 0.75)
train_data <- subset(titanic_data, sample_data == TRUE)
test_data <- subset(titanic_data, sample_data == FALSE)



rtree <- rpart(survived ~ ., train_data)
rpart.plot(rtree)




















rtreemodel <- train(rtree,testtask)

ctree_ <- ctree(survived ~ ., train_data)
plot(ctree_)



rpart <- makeLearner("classif.rpart", predict.type = "response", fix.factors.prediction = TRUE)

traintask <- makeClassifTask(data = train_data, target = "survived") 
testtask <- makeClassifTask(data = test_data, target = "survived")

rpartmodel <- train(rpart.tree,traintask)

rpartmodel.predict <- predict(rtree, testtask)
performance(rpartmodel.predict, measures = list(rmse, mae, rsq))



