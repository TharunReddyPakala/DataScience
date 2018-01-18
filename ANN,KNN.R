#*******************************************************KNN dataset1******************************************************************#
rm(list = ls())
heart.df <- read.csv('C:/AML-BUAN 6341/CDSdata1.csv')

library(DMwR)
library(caret)
#Split dataset into training and test(70/30)
set.seed(123)
split <- sample(1:nrow(heart.df),as.integer(0.7*nrow(heart.df)))
traindata <- heart.df[split,]
testdata <- heart.df[-split,]

## A 3-nearest neighbours model with no normalization
nn3 <- kNN(NSP ~ .,traindata,testdata,norm=FALSE,k=3)

## The resulting confusion matrix
table(testdata[,'NSP'],nn3)

## Now a 5-nearest neighbours model with normalization
nn5 <- kNN(NSP ~ .,traindata,testdata,norm=TRUE,k=5)

## The resulting confusion matrix
table(testdata[,'NSP'],nn5)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(123)
knn_fit <- train(NSP ~., data = traindata, method = "knn",
                 trControl=trctrl,
                 
                 preProcess = c("center", "scale"),
                 tuneLength = 10)

plot(knn_fit)
test_pred <- predict(knn_fit, newdata = testdata)

# Create a confusion matrix
cm_Knn <- confusionMatrix(test_pred, testdata$NSP )
cm_Knn$table
cm_Knn$overall

rm(list = ls())
#********************************************KNN dataset2**************************************************************
wine.df <- read.csv('C:/AML-BUAN 6341/winequality-red1.csv')

#Split dataset into training and test(70/30)
set.seed(123)
split <- sample(1:nrow(wine.df),as.integer(0.7*nrow(wine.df)))
traindata <- wine.df[split,]
testdata <- wine.df[-split,]

#traindata[["NSP"]] = factor(traindata[["NSP"]])

## A 3-nearest neighbours model with no normalization
nn3 <- kNN(quality ~ .,traindata,testdata,norm=FALSE,k=3)

## The resulting confusion matrix
table(testdata[,'quality'],nn3)

## Now a 5-nearest neighbours model with normalization
nn5 <- kNN(quality ~ .,traindata,testdata,norm=TRUE,k=5)

## The resulting confusion matrix
table(testdata[,'quality'],nn5)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(123)
knn_fit <- train(quality ~., data = traindata, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)

plot(knn_fit)
test_pred <- predict(knn_fit, newdata = testdata)

# Create a confusion matrix
cm_Knn <- confusionMatrix(test_pred, testdata$quality )
cm_Knn$table
cm_Knn$overall

#********************************************Artificial Neural Networks dataset2******************************************************#
rm(list=ls())
library(dplyr)
library(neuralnet)
library(functional)

setwd("C:/AML-BUAN 6341/")
wine <- read.csv("winequality-red.csv",header = TRUE, sep = ",")

#create test and training data sets
set.seed(123)
train_rows <- sample(nrow(wine),0.7*nrow(wine),replace=FALSE)
wine_train <- as.data.frame(wine[train_rows,])
wine_test <- as.data.frame(wine[-train_rows,])
rm(train_rows)

#=========================== Artificial Neural Networks for wine quality data set(dataset2)===========================#
formula <- "quality ~ fixed_acidity + volatile_acidity + citric_acid + residual_sugar + chlorides + free_sulfur_dioxide + total_sulfur_dioxide + density + pH + sulphates + alcohol"
wine_ann_function <- function(form,dataset,algo,acc_func,nodes){
  model <- neuralnet(form,
                     data = dataset,
                     algorithm = algo,
                     act.fct = acc_func,
                     hidden = nodes,
                     learningrate.limit = NULL, #for rprop
                     learningrate.factor = list(minus = 0.5, plus = 1.2), #for rprop
                     learningrate=NULL, #for backprop
                     threshold = 0.05,
                     stepmax = 1e+06, rep = 1, startweights = NULL,
                     lifesign = "minimal",
                     lifesign.step = 1000, 
                     err.fct = "sse", 
                     linear.output = FALSE, exclude = NULL,
                     constant.weights = NULL,
                     likelihood = TRUE)
  return(model)
}


#=========================Trial 1=============================#
#one layer(3),logistic
trial1 <- wine_ann_function(form=formula,dataset=wine_train,algo="rprop+",acc_func="logistic",nodes=c(3))

#print the neural network
plot(trial1)

#Model Error
error <- trial1$result.matrix[1]
print(paste("ANN-Logistic-Nodes 3-No Cross Validation-Model Error = ",error))

#AIC
aic <- trial1$result.matrix[4]
print(paste("ANN-Logistic-Nodes 3-No Cross Validation-Model AIC = ",aic))

#BIC
bic <- trial1$result.matrix[5]
print(paste("ANN-Logistic-Nodes 3-No Cross Validation-BIC = ",bic))

#ROC Train data
library(pROC)
plot(roc(wine_train$quality, unlist(trial1$net.result)))

#AUC Train Data
auc <- auc(wine_train$quality, unlist(trial1$net.result))
print(paste("ANN-Logistic-Nodes 3-No Cross Validation-AUC on Train data = ",auc))

# Compute predictions on test data
test1 <- compute(trial1,wine_test[1:11])

#Confusion matrix
library(ModelMetrics)
confusionMatrix(wine_test$quality,test1$net.result)

#ROC Test data
plot(roc(wine_test$quality, as.numeric(test1$net.result)))

#AUC Test Data
auc <- auc(wine_test$quality, as.numeric(test1$net.result))
print(paste("ANN-Logistic- Nodes 3-No Cross Validation-AUC on Test data = ",auc))

# Accuracy
original_values <- max.col(wine_test[, 12])
test1_2 <- max.col(unlist(test1$net.result))
acc <- mean(test1_2 == original_values)
print(paste("ANN-Logistic- Nodes 3-No Cross Validation-Accuracy on Test data = ",acc))

#=========================Trial 2=============================#
#one layer(5),logistic
trial1 <- wine_ann_function(form=formula,dataset=wine_train,algo="rprop+",acc_func="logistic",nodes=c(5))

#print the neural network
plot(trial1)

#Model Error
error <- trial1$result.matrix[1]
print(paste("ANN-Logistic-Nodes 5-No Cross Validation-Model Error = ",error))

#AIC
aic <- trial1$result.matrix[4]
print(paste("ANN-Logistic-Nodes 5-No Cross Validation-Model AIC = ",aic))

#BIC
bic <- trial1$result.matrix[5]
print(paste("ANN-Logistic-Nodes 5-No Cross Validation-BIC = ",bic))

#ROC Train data
library(pROC)
plot(roc(wine_train$quality, unlist(trial1$net.result)))

#AUC Train Data
auc <- auc(wine_train$quality, unlist(trial1$net.result))
print(paste("ANN-Logistic-Nodes 5-No Cross Validation-AUC on Train data = ",auc))

# Compute predictions on test data
test1 <- compute(trial1,wine_test[1:11])

#Confusion matrix
library(ModelMetrics)
confusionMatrix(wine_test$quality,test1$net.result)

#ROC Test data
plot(roc(wine_test$quality, as.numeric(test1$net.result)))

#AUC Test Data
auc <- auc(wine_test$quality, as.numeric(test1$net.result))
print(paste("ANN-Logistic- Nodes 5-No Cross Validation-AUC on Test data = ",auc))

# Accuracy
original_values <- max.col(wine_test[, 12])
test1_2 <- max.col(unlist(test1$net.result))
acc <- mean(test1_2 == original_values)
print(paste("ANN-Logistic- Nodes 5-No Cross Validation-Accuracy on Test data = ",acc))

#=========================Trial 3=============================#
#one layer(7),logistic
trial1 <- wine_ann_function(form=formula,dataset=wine_train,algo="rprop+",acc_func="logistic",nodes=c(7))

#print the neural network
plot(trial1)

#Model Error
error <- trial1$result.matrix[1]
print(paste("ANN-Logistic-Nodes 7-No Cross Validation-Model Error = ",error))

#AIC
aic <- trial1$result.matrix[4]
print(paste("ANN-Logistic-Nodes 7-No Cross Validation-Model AIC = ",aic))

#BIC
bic <- trial1$result.matrix[5]
print(paste("ANN-Logistic-Nodes 7-No Cross Validation-BIC = ",bic))

#ROC Train data
library(pROC)
plot(roc(wine_train$quality, unlist(trial1$net.result)))

#AUC Train Data
auc <- auc(wine_train$quality, unlist(trial1$net.result))
print(paste("ANN-Logistic-Nodes 7-No Cross Validation-AUC on Train data = ",auc))

# Compute predictions on test data
test1 <- compute(trial1,wine_test[1:11])

#Confusion matrix
library(ModelMetrics)
confusionMatrix(wine_test$quality,test1$net.result)

#ROC Test data
plot(roc(wine_test$quality, as.numeric(test1$net.result)))

#AUC Test Data
auc <- auc(wine_test$quality, as.numeric(test1$net.result))
print(paste("ANN-Logistic- Nodes 7-No Cross Validation-AUC on Test data = ",auc))

# Accuracy
original_values <- max.col(wine_test[, 12])
test1_2 <- max.col(unlist(test1$net.result))
acc <- mean(test1_2 == original_values)
print(paste("ANN-Logistic- Nodes 7-No Cross Validation-Accuracy on Test data = ",acc))

#=========================Trial 4=============================#
#two layer(5,3),logistic
trial1 <- wine_ann_function(form=formula,dataset=wine_train,algo="rprop+",acc_func="logistic",nodes=c(5,3))

#print the neural network
plot(trial1)

#Model Error
error <- trial1$result.matrix[1]
print(paste("ANN-Logistic-Nodes (5,3)-No Cross Validation-Model Error = ",error))

#AIC
aic <- trial1$result.matrix[4]
print(paste("ANN-Logistic-Nodes (5,3)-No Cross Validation-Model AIC = ",aic))

#BIC
bic <- trial1$result.matrix[5]
print(paste("ANN-Logistic-Nodes (5,3)-No Cross Validation-BIC = ",bic))

#ROC Train data
library(pROC)
plot(roc(wine_train$quality, unlist(trial1$net.result)))

#AUC Train Data
auc <- auc(wine_train$quality, unlist(trial1$net.result))
print(paste("ANN-Logistic-Nodes (5,3)-No Cross Validation-AUC on Train data = ",auc))

# Compute predictions on test data
test1 <- compute(trial1,wine_test[1:11])

#Confusion matrix
library(ModelMetrics)
confusionMatrix(wine_test$quality,test1$net.result)

#ROC Test data
plot(roc(wine_test$quality, as.numeric(test1$net.result)))

#AUC Test Data
auc <- auc(wine_test$quality, as.numeric(test1$net.result))
print(paste("ANN-Logistic- Nodes (5,3)-No Cross Validation-AUC on Test data = ",auc))

# Accuracy
original_values <- max.col(wine_test[, 12])
test1_2 <- max.col(unlist(test1$net.result))
acc <- mean(test1_2 == original_values)
print(paste("ANN-Logistic- Nodes (5,3)-No Cross Validation-Accuracy on Test data = ",acc))

#=========================Trial 5=============================#
#two layer(7,5),logistic
trial1 <- wine_ann_function(form=formula,dataset=wine_train,algo="rprop+",acc_func="logistic",nodes=c(7,5))

#print the neural network
plot(trial1)

#Model Error
error <- trial1$result.matrix[1]
print(paste("ANN-Logistic-Nodes (7,5)-No Cross Validation-Model Error = ",error))

#AIC
aic <- trial1$result.matrix[4]
print(paste("ANN-Logistic-Nodes (7,5)-No Cross Validation-Model AIC = ",aic))

#BIC
bic <- trial1$result.matrix[5]
print(paste("ANN-Logistic-Nodes (7,5)-No Cross Validation-BIC = ",bic))

#ROC Train data
library(pROC)
plot(roc(wine_train$quality, unlist(trial1$net.result)))

#AUC Train Data
auc <- auc(wine_train$quality, unlist(trial1$net.result))
print(paste("ANN-Logistic-Nodes (7,5)-No Cross Validation-AUC on Train data = ",auc))

# Compute predictions on test data
test1 <- compute(trial1,wine_test[1:11])

#Confusion matrix
library(ModelMetrics)
confusionMatrix(wine_test$quality,test1$net.result)

#ROC Test data
plot(roc(wine_test$quality, as.numeric(test1$net.result)))

#AUC Test Data
auc <- auc(wine_test$quality, as.numeric(test1$net.result))
print(paste("ANN-Logistic- Nodes (7,5)-No Cross Validation-AUC on Test data = ",auc))

# Accuracy
original_values <- max.col(wine_test[, 12])
test1_2 <- max.col(unlist(test1$net.result))
acc <- mean(test1_2 == original_values)
print(paste("ANN-Logistic- Nodes (7,5)-No Cross Validation-Accuracy on Test data = ",acc))

#=========================Trial 6=============================#
#three layer(9,7,5),logistic
trial1 <- wine_ann_function(form=formula,dataset=wine_train,algo="rprop+",acc_func="logistic",nodes=c(9,7,5))

#print the neural network
plot(trial1)

#Model Error
error <- trial1$result.matrix[1]
print(paste("ANN-Logistic-Nodes (9,7,5)-No Cross Validation-Model Error = ",error))

#AIC
aic <- trial1$result.matrix[4]
print(paste("ANN-Logistic-Nodes (9,7,5)-No Cross Validation-Model AIC = ",aic))

#BIC
bic <- trial1$result.matrix[5]
print(paste("ANN-Logistic-Nodes (9,7,5)-No Cross Validation-BIC = ",bic))

#ROC Train data
library(pROC)
plot(roc(wine_train$quality, unlist(trial1$net.result)))

#AUC Train Data
auc <- auc(wine_train$quality, unlist(trial1$net.result))
print(paste("ANN-Logistic-Nodes (9,7,5)-No Cross Validation-AUC on Train data = ",auc))

# Compute predictions on test data
test1 <- compute(trial1,wine_test[1:11])

#Confusion matrix
library(ModelMetrics)
confusionMatrix(wine_test$quality,test1$net.result)

#ROC Test data
plot(roc(wine_test$quality, as.numeric(test1$net.result)))

#AUC Test Data
auc <- auc(wine_test$quality, as.numeric(test1$net.result))
print(paste("ANN-Logistic- Nodes (9,7,5)-No Cross Validation-AUC on Test data = ",auc))

# Accuracy
original_values <- max.col(wine_test[, 12])
test1_2 <- max.col(unlist(test1$net.result))
acc <- mean(test1_2 == original_values)
print(paste("ANN-Logistic- Nodes (9,7,5)-No Cross Validation-Accuracy on Test data = ",acc))


#=========================Trial 7=============================#
#one layer(3),tanh
trial1 <- wine_ann_function(form=formula,dataset=wine_train,algo="rprop+",acc_func="tanh",nodes=c(3))

#print the neural network
plot(trial1)

#Model Error
error <- trial1$result.matrix[1]
print(paste("ANN-tanh-Nodes 3-No Cross Validation-Model Error = ",error))

#AIC
aic <- trial1$result.matrix[4]
print(paste("ANN-tanh-Nodes 3-No Cross Validation-Model AIC = ",aic))

#BIC
bic <- trial1$result.matrix[5]
print(paste("ANN-tanh-Nodes 3-No Cross Validation-BIC = ",bic))

#ROC Train data
library(pROC)
plot(roc(wine_train$quality, unlist(trial1$net.result)))

#AUC Train Data
auc <- auc(wine_train$quality, unlist(trial1$net.result))
print(paste("ANN-tanh-Nodes 3-No Cross Validation-AUC on Train data = ",auc))

# Compute predictions on test data
test1 <- compute(trial1,wine_test[1:11])

#Confusion matrix
library(ModelMetrics)
confusionMatrix(wine_test$quality,test1$net.result)

#ROC Test data
plot(roc(wine_test$quality, as.numeric(test1$net.result)))

#AUC Test Data
auc <- auc(wine_test$quality, as.numeric(test1$net.result))
print(paste("ANN-tanh- Nodes 3-No Cross Validation-AUC on Test data = ",auc))

# Accuracy
original_values <- max.col(wine_test[, 12])
test1_2 <- max.col(unlist(test1$net.result))
acc <- mean(test1_2 == original_values)
print(paste("ANN-tanh- Nodes 3-No Cross Validation-Accuracy on Test data = ",acc))

#=========================Trial 8=============================#
#one layer(5),tanh
trial1 <- wine_ann_function(form=formula,dataset=wine_train,algo="rprop+",acc_func="tanh",nodes=c(5))

#print the neural network
plot(trial1)

#Model Error
error <- trial1$result.matrix[1]
print(paste("ANN-tanh-Nodes 5-No Cross Validation-Model Error = ",error))

#AIC
aic <- trial1$result.matrix[4]
print(paste("ANN-tanh-Nodes 5-No Cross Validation-Model AIC = ",aic))

#BIC
bic <- trial1$result.matrix[5]
print(paste("ANN-tanh-Nodes 5-No Cross Validation-BIC = ",bic))

#ROC Train data
library(pROC)
plot(roc(wine_train$quality, unlist(trial1$net.result)))

#AUC Train Data
auc <- auc(wine_train$quality, unlist(trial1$net.result))
print(paste("ANN-tanh-Nodes 5-No Cross Validation-AUC on Train data = ",auc))

# Compute predictions on test data
test1 <- compute(trial1,wine_test[1:11])

#Confusion matrix
library(ModelMetrics)
confusionMatrix(wine_test$quality,test1$net.result)

#ROC Test data
plot(roc(wine_test$quality, as.numeric(test1$net.result)))

#AUC Test Data
auc <- auc(wine_test$quality, as.numeric(test1$net.result))
print(paste("ANN-tanh- Nodes 5-No Cross Validation-AUC on Test data = ",auc))

# Accuracy
original_values <- max.col(wine_test[, 12])
test1_2 <- max.col(unlist(test1$net.result))
acc <- mean(test1_2 == original_values)
print(paste("ANN-tanh- Nodes 5-No Cross Validation-Accuracy on Test data = ",acc))

#=========================Trial 9=============================#
#one layer(7),tanh
trial1 <- wine_ann_function(form=formula,dataset=wine_train,algo="rprop+",acc_func="tanh",nodes=c(7))

#print the neural network
plot(trial1)

#Model Error
error <- trial1$result.matrix[1]
print(paste("ANN-tanh-Nodes 7-No Cross Validation-Model Error = ",error))

#AIC
aic <- trial1$result.matrix[4]
print(paste("ANN-tanh-Nodes 7-No Cross Validation-Model AIC = ",aic))

#BIC
bic <- trial1$result.matrix[5]
print(paste("ANN-tanh-Nodes 7-No Cross Validation-BIC = ",bic))

#ROC Train data
library(pROC)
plot(roc(wine_train$quality, unlist(trial1$net.result)))

#AUC Train Data
auc <- auc(wine_train$quality, unlist(trial1$net.result))
print(paste("ANN-tanh-Nodes 7-No Cross Validation-AUC on Train data = ",auc))

# Compute predictions on test data
test1 <- compute(trial1,wine_test[1:11])

#Confusion matrix
library(ModelMetrics)
confusionMatrix(wine_test$quality,test1$net.result)

#ROC Test data
plot(roc(wine_test$quality, as.numeric(test1$net.result)))

#AUC Test Data
auc <- auc(wine_test$quality, as.numeric(test1$net.result))
print(paste("ANN-tanh- Nodes 7-No Cross Validation-AUC on Test data = ",auc))

# Accuracy
original_values <- max.col(wine_test[, 12])
test1_2 <- max.col(unlist(test1$net.result))
acc <- mean(test1_2 == original_values)
print(paste("ANN-tanh- Nodes 7-No Cross Validation-Accuracy on Test data = ",acc))

#=========================Trial 10=============================#
#two layer(5,3),tanh
trial1 <- wine_ann_function(form=formula,dataset=wine_train,algo="rprop+",acc_func="tanh",nodes=c(5,3))

#print the neural network
plot(trial1)

#Model Error
error <- trial1$result.matrix[1]
print(paste("ANN-tanh-Nodes (5,3)-No Cross Validation-Model Error = ",error))

#AIC
aic <- trial1$result.matrix[4]
print(paste("ANN-tanh-Nodes (5,3)-No Cross Validation-Model AIC = ",aic))

#BIC
bic <- trial1$result.matrix[5]
print(paste("ANN-tanh-Nodes (5,3)-No Cross Validation-BIC = ",bic))

#ROC Train data
library(pROC)
plot(roc(wine_train$quality, unlist(trial1$net.result)))

#AUC Train Data
auc <- auc(wine_train$quality, unlist(trial1$net.result))
print(paste("ANN-tanh-Nodes (5,3)-No Cross Validation-AUC on Train data = ",auc))

# Compute predictions on test data
test1 <- compute(trial1,wine_test[1:11])

#Confusion matrix
library(ModelMetrics)
confusionMatrix(wine_test$quality,test1$net.result)

#ROC Test data
plot(roc(wine_test$quality, as.numeric(test1$net.result)))

#AUC Test Data
auc <- auc(wine_test$quality, as.numeric(test1$net.result))
print(paste("ANN-tanh- Nodes (5,3)-No Cross Validation-AUC on Test data = ",auc))

# Accuracy
original_values <- max.col(wine_test[, 12])
test1_2 <- max.col(unlist(test1$net.result))
acc <- mean(test1_2 == original_values)
print(paste("ANN-tanh- Nodes (5,3)-No Cross Validation-Accuracy on Test data = ",acc))

#=========================Trial 11=============================#
#two layer(7,5),tanh
trial1 <- wine_ann_function(form=formula,dataset=wine_train,algo="rprop+",acc_func="tanh",nodes=c(7,5))

#print the neural network
plot(trial1)

#Model Error
error <- trial1$result.matrix[1]
print(paste("ANN-tanh-Nodes (7,5)-No Cross Validation-Model Error = ",error))

#AIC
aic <- trial1$result.matrix[4]
print(paste("ANN-tanh-Nodes (7,5)-No Cross Validation-Model AIC = ",aic))

#BIC
bic <- trial1$result.matrix[5]
print(paste("ANN-tanh-Nodes (7,5)-No Cross Validation-BIC = ",bic))

#ROC Train data
library(pROC)
plot(roc(wine_train$quality, unlist(trial1$net.result)))

#AUC Train Data
auc <- auc(wine_train$quality, unlist(trial1$net.result))
print(paste("ANN-tanh-Nodes (7,5)-No Cross Validation-AUC on Train data = ",auc))

# Compute predictions on test data
test1 <- compute(trial1,wine_test[1:11])

#Confusion matrix
library(ModelMetrics)
confusionMatrix(wine_test$quality,test1$net.result)

#ROC Test data
plot(roc(wine_test$quality, as.numeric(test1$net.result)))

#AUC Test Data
auc <- auc(wine_test$quality, as.numeric(test1$net.result))
print(paste("ANN-tanh- Nodes (7,5)-No Cross Validation-AUC on Test data = ",auc))

# Accuracy
original_values <- max.col(wine_test[, 12])
test1_2 <- max.col(unlist(test1$net.result))
acc <- mean(test1_2 == original_values)
print(paste("ANN-tanh- Nodes (7,5)-No Cross Validation-Accuracy on Test data = ",acc))

#=========================Trial 12=============================#
#three layer(9,7,5),tanh
trial1 <- wine_ann_function(form=formula,dataset=wine_train,algo="rprop+",acc_func="tanh",nodes=c(9,7,5))

#print the neural network
plot(trial1)

#Model Error
error <- trial1$result.matrix[1]
print(paste("ANN-tanh-Nodes (9,7,5)-No Cross Validation-Model Error = ",error))

#AIC
aic <- trial1$result.matrix[4]
print(paste("ANN-tanh-Nodes (9,7,5)-No Cross Validation-Model AIC = ",aic))

#BIC
bic <- trial1$result.matrix[5]
print(paste("ANN-tanh-Nodes (9,7,5)-No Cross Validation-BIC = ",bic))

#ROC Train data
library(pROC)
plot(roc(wine_train$quality, unlist(trial1$net.result)))

#AUC Train Data
auc <- auc(wine_train$quality, unlist(trial1$net.result))
print(paste("ANN-tanh-Nodes (9,7,5)-No Cross Validation-AUC on Train data = ",auc))

# Compute predictions on test data
test1 <- compute(trial1,wine_test[1:11])

#Confusion matrix
library(ModelMetrics)
confusionMatrix(wine_test$quality,test1$net.result)

#ROC Test data
plot(roc(wine_test$quality, as.numeric(test1$net.result)))

#AUC Test Data
auc <- auc(wine_test$quality, as.numeric(test1$net.result))
print(paste("ANN-tanh- Nodes (9,7,5)-No Cross Validation-AUC on Test data = ",auc))

# Accuracy
original_values <- max.col(wine_test[, 12])
test1_2 <- max.col(unlist(test1$net.result))
acc <- mean(test1_2 == original_values)
print(paste("ANN-tanh- Nodes (9,7,5)-No Cross Validation-Accuracy on Test data = ",acc))

#==============================================================================#
#=========================== Cardiotocogram Dataset(Dataset1) ===========================#
#==============================================================================#
cds <- read.csv("CDSdata1.csv",header = TRUE, sep = ",")

#create test and training data sets
set.seed(123)
train_rows <- sample(nrow(cds),0.7*nrow(cds),replace=FALSE)
cds_train <- as.data.frame(cds[train_rows,])
cds_test <- as.data.frame(cds[-train_rows,])
rm(train_rows)

formula <- "NSP~LB+AC+FM+UC+DL+DS+DP+ASTV+MSTV+ALTV+MLTV+Width+Min+Max+Nmax+Nzeros+Mode+Mean+Median+Variance+Tendency"
cds_ann_function <- function(form,dataset,algo,acc_func,nodes){
  model <- neuralnet(form,
                     data = dataset,
                     algorithm = algo,
                     act.fct = acc_func,
                     hidden = nodes,
                     learningrate.limit = NULL, #for rprop
                     learningrate.factor = list(minus = 0.5, plus = 1.2), #for rprop
                     learningrate=NULL, #for backprop
                     threshold = 0.05,
                     stepmax = 1e+06, rep = 1, startweights = NULL,
                     lifesign = "minimal",
                     lifesign.step = 1000, 
                     err.fct = "sse", 
                     linear.output = FALSE, exclude = NULL,
                     constant.weights = NULL,
                     likelihood = TRUE)
  return(model)
}


#=========================Trial 1=============================#
#one layer(3),logistic
trial1 <- cds_ann_function(form=formula,dataset=cds_train,algo="rprop+",acc_func="logistic",nodes=c(3))

#print the neural network
plot(trial1)

#Model Error
error <- trial1$result.matrix[1]
print(paste("ANN-Logistic-Nodes 3-No Cross Validation-Model Error = ",error))

#AIC
aic <- trial1$result.matrix[4]
print(paste("ANN-Logistic-Nodes 3-No Cross Validation-Model AIC = ",aic))

#BIC
bic <- trial1$result.matrix[5]
print(paste("ANN-Logistic-Nodes 3-No Cross Validation-BIC = ",bic))

#ROC Train data
library(pROC)
plot(roc(cds_train$NSP, unlist(trial1$net.result)))

#AUC Train Data
auc <- auc(cds_train$NSP, unlist(trial1$net.result))
print(paste("ANN-Logistic-Nodes 3-No Cross Validation-AUC on Train data = ",auc))

# Compute predictions on test data
test1 <- compute(trial1,cds_test[1:21])

#Confusion matrix
library(ModelMetrics)
confusionMatrix(cds_test$NSP,test1$net.result)

#ROC Test data
plot(roc(cds_test$NSP, as.numeric(test1$net.result)))

#AUC Test Data
auc <- auc(cds_test$NSP, as.numeric(test1$net.result))
print(paste("ANN-Logistic- Nodes 3-No Cross Validation-AUC on Test data = ",auc))

# Accuracy
original_values <- max.col(cds_test[, 12])
test1_2 <- max.col(unlist(test1$net.result))
acc <- mean(test1_2 == original_values)
print(paste("ANN-Logistic- Nodes 3-No Cross Validation-Accuracy on Test data = ",acc))


#=========================Trial 2=============================#
#one layer(3),tanh
trial1 <- cds_ann_function(form=formula,dataset=cds_train,algo="rprop+",acc_func="tanh",nodes=c(3))

#print the neural network
plot(trial1)

#Model Error
error <- trial1$result.matrix[1]
print(paste("ANN-tanh-Nodes 3-No Cross Validation-Model Error = ",error))

#AIC
aic <- trial1$result.matrix[4]
print(paste("ANN-tanh-Nodes 3-No Cross Validation-Model AIC = ",aic))

#BIC
bic <- trial1$result.matrix[5]
print(paste("ANN-tanh-Nodes 3-No Cross Validation-BIC = ",bic))

#ROC Train data
library(pROC)
plot(roc(cds_train$NSP, unlist(trial1$net.result)))

#AUC Train Data
auc <- auc(cds_train$NSP, unlist(trial1$net.result))
print(paste("ANN-tanh-Nodes 3-No Cross Validation-AUC on Train data = ",auc))

# Compute predictions on test data
test1 <- compute(trial1,cds_test[1:21])

#Confusion matrix
library(ModelMetrics)
confusionMatrix(cds_test$NSP,test1$net.result)

#ROC Test data
plot(roc(cds_test$NSP, as.numeric(test1$net.result)))

#AUC Test Data
auc <- auc(cds_test$NSP, as.numeric(test1$net.result))
print(paste("ANN-tanh- Nodes 3-No Cross Validation-AUC on Test data = ",auc))

# Accuracy
original_values <- max.col(cds_test[, 12])
test1_2 <- max.col(unlist(test1$net.result))
acc <- mean(test1_2 == original_values)
print(paste("ANN-tanh- Nodes 3-No Cross Validation-Accuracy on Test data = ",acc))