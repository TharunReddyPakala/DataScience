# Create a simple decision tree model for predicting car ratings

# We will need these libraries ... install.packages("    "), before running
library(rpart)
library(caret)

# Read in the dataset
data<-read.csv(file="car.csv")

# Look at the column names
names(data)

# Plot the distribution of ratings
plot(data$Rating)

# Print the number of observations in the dataset
print(nrow(data))

# Split the dataset into a training and test dataset using a split of 75% training
# and 25% test
set.seed(323)
trainingObs<-sample(nrow(data),0.75*nrow(data),replace=FALSE)

# Print a few of the instances that made it into the training dataset
print(trainingObs[1:10])

# Create the training dataset
trainingDS<-data[trainingObs,]

# Print the number of instances in the training datset
print(nrow(trainingDS))

# Create the test dataset
testDS<-data[-trainingObs,]

# Print the number instances that made it into the test dataset
print(nrow(testDS))

# Create a simple decision tree model
treeModel<-rpart(Rating ~ BuyingPrice + Maintenance + Doors + PersonCapacity + LuggageArea + Safety, method="class", data=trainingDS)

# Do the predictions on the test dataset (output CLASS not PROBABILITIES)
predictions1<-predict(treeModel,testDS,type=c("class"))

# Create a confusion matrix
confMat1<-confusionMatrix(testDS$Rating,predictions1)
print(confMat1$table)

# Prune the tree and re-access the model after pruning
# prune the tree 
ptreeModel<- prune(treeModel, cp=treeModel$cptable[which.min(treeModel$cptable[,"xerror"]),"CP"])

# Make revised decisions based on the pruned tree
predictions2<-predict(ptreeModel,testDS,type=c("class"))

# Create a confusion matrix
confMat2<-confusionMatrix(testDS$Rating,predictions2)
print(confMat2$table)