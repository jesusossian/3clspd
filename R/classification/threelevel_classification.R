
#import the package
library(randomForest)
library(caret)

library(caTools)



















#Getting data
dataset <- output

dataset


dataset <- dataset[ , -which(names(dataset) %in% c("inst","minbestcost"))]


dataset <- dataset[ , -which(names(dataset) %in% c("inst","minbestcost","typeD","typeF","NW","balanced"))]

dataset <- dataset[ , -which(names(dataset) %in% c("inst","minbestcost","typeD","typeF","NR","NW","balanced"))]

#dataset <- dataset[ , -which(names(dataset) %in% c("inst","minaverageconf"))]

dataset <- subset(dataset,dataset$minbestconf != "c_2_2")


dataset$minbestconf <- as.factor(dataset$minbestconf)
dataset$balanced <- as.factor(dataset$balanced)
dataset$typeF <- as.factor(dataset$typeF)
dataset$typeD <- as.factor(dataset$typeD)




# Set random seed to make results reproducible:
set.seed(17)


# create a list of 80% of the rows in the original dataset we can use for training
indexes <- createDataPartition(dataset$minbestconf, p=0.85, list=FALSE)
# select 20% of the data for validation
training <- dataset[indexes,]
validation1 <- dataset[-indexes,]




#training$minbestconf <- as.factor(training$minbestconf)
#validation1$Species <- as.factor(iris[-indexes,])



# Perform training:
rf_classifier = randomForest(minbestconf ~ ., data=training, ntree=100, mtry=2, importance=TRUE)

rf_classifier




confusion.training <- rf_classifier$confusion

confusion.training


rf.cv <- rfcv(training, training$Species, cv.fold=10)
with(rf.cv, plot(n.var, error.cv, type="b", col="red"))


varImpPlot(rf_classifier)

# Validation set assessment #1: looking at confusion matrix
prediction_for_table <- predict(rf_classifier,validation1[,-4])
table(observed=validation1[,4],predicted=prediction_for_table)
























################
#
# dpheur
#
###########333




dataset <- output

dataset


dataset <- dataset[ , -which(names(dataset) %in% c("inst","minaverageconf","balanced","NT"))]

dataset <- subset(dataset,dataset$minbestconf != "c_0.2_0.85_0.7_0.7")


dataset$minbestconf <- as.factor(dataset$minbestconf)
dataset$balanced <- as.factor(dataset$balanced)




# Set random seed to make results reproducible:
set.seed(17)


# create a list of 80% of the rows in the original dataset we can use for training
indexes <- createDataPartition(dataset$minbestconf, p=0.70, list=FALSE)
# select 20% of the data for validation
training <- dataset[indexes,]
validation1 <- dataset[-indexes,]




#training$minbestconf <- as.factor(training$minbestconf)
#validation1$Species <- as.factor(iris[-indexes,])



# Perform training:
rf_classifier = randomForest(minbestconf ~ ., data=training, ntree=100, mtry=2, importance=TRUE)

rf_classifier




confusion.training <- rf_classifier$confusion

confusion.training


rf.cv <- rfcv(training, training$Species, cv.fold=10)
with(rf.cv, plot(n.var, error.cv, type="b", col="red"))


varImpPlot(rf_classifier)

# Validation set assessment #1: looking at confusion matrix
prediction_for_table <- predict(rf_classifier,validation1[,-4])
table(observed=validation1[,4],predicted=prediction_for_table)































# Set random seed to make results reproducible:
set.seed(17)
# Calculate the size of each of the data sets:
data_set_size <- floor(nrow(iris)/2)
# Generate a random sample of "data_set_size" indexes
indexes <- sample(1:nrow(iris), size = data_set_size)
# Assign the data to the correct sets
training <- iris[indexes,]
validation1 <- iris[-indexes,]





training$Species <- as.factor(training$Species)
validation1$Species <- as.factor(iris[-indexes,])



# Perform training:
rf_classifier = randomForest(Species ~ ., data=training, ntree=100, mtry=2, importance=TRUE)

rf_classifier




confusion.training <- rf_classifier$confusion

confusion.training


rf.cv <- rfcv(training, training$Species, cv.fold=10)
with(rf.cv, plot(n.var, error.cv, type="b", col="red"))


varImpPlot(rf_classifier)

# Validation set assessment #1: looking at confusion matrix
prediction_for_table <- predict(rf_classifier,validation1[,-5])
table(observed=validation1[,5],predicted=prediction_for_table)





# Validation set assessment #2: ROC curves and AUC
# Needs to import ROCR package for ROC curve plotting:
library(ROCR)
# Calculate the probability of new observations belonging to each class
# prediction_for_roc_curve will be a matrix with dimensions data_set_size x number_of_classes
prediction_for_roc_curve <- predict(rf_classifier,validation1[,-5],type="prob")
# Use pretty colours:
pretty_colours <- c("#F8766D","#00BA38","#619CFF")
# Specify the different classes 
classes <- levels(validation1$Species)
# For each class
for (i in 1:3)
{
  # Define which observations belong to class[i]
  true_values <- ifelse(validation1[,5]==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(prediction_for_roc_curve[,i],true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i==1)
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i]) 
  }
  else
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i],add=TRUE) 
  }
  # Calculate the AUC and print it to screen
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}
