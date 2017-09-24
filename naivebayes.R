# Create a naivebayes model for predicting car ratings

# We will need these libraries ... install.packages("    "), before running
library(caret)
library(e1071)

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

# Create a NaiveBayes
nbModel<-naiveBayes(Rating ~ .,trainingDS)

# Do the predictions on the test dataset (output CLASS not PROBABILITIES)
predictions3<-predict(nbModel,testDS,type=c("class"))

# Create a confusion matrix
confMat3<-confusionMatrix(testDS$Rating,predictions3)
print(confMat3$table)

