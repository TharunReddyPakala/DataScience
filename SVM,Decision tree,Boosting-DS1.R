rm(list = ls())
bank.df <- read.csv('C:/AML-BUAN 6341/bankmarketing.csv')
data = bank.df

head(data)
#Split dataset into training and test(70/30)
set.seed(123)
split <- sample(2,nrow(data),replace = TRUE,prob = c(0.70,0.30))
trainingdata <- data[split==1,]
testdata <- data[split==2,]

library(rpart)
library(rpart.plot)				
library(caret)
library(pROC)

#**********************************************Decision Tree implementation******************************************************** 
decision_fit_gini <- rpart(y ~ age + job + marital + education + default + balance + housing + loan + contact + day + month + duration + campaign + pdays + previous + poutcome,data= trainingdata,
                           method="class", parms = list(split = "gini"),
                           control = list(cp = 0.01))
printcp(decision_fit_gini) # display the results
summary(decision_fit_gini) # detailed summary of splits
plotcp(decision_fit_gini) # visualize cross-validation results 

# plot tree 
plot(decision_fit_gini,main="decision Tree for Bank Marketing ")
text(decision_fit_gini)


#nice looking tree
prp(decision_fit_gini,varlen=3)
rpart.plot(decision_fit_gini)

pred_tree_gini <- predict(decision_fit_gini,testdata[-17], type = 'prob')
pred_tree_gini


library(pROC)
plot(roc(testdata$y, pred_tree_gini[,2]))
auc_gini<- auc(testdata$y, pred_tree_gini[,2])
auc_gini

# Do the predictions on the test dataset (output CLASS not PROBABILITIES)
test_preds_gini <- predict(decision_fit_gini, testdata,type = 'class' )
plot(test_preds_gini)

# Create a confusion matrix and find accuracy
cm_treegini <- confusionMatrix(testdata$y, test_preds_gini)
cm_treegini$table 
cm_treegini$overall

# prune the tree 
pfit_gini<- prune(decision_fit_gini, cp=0.01) # from cptable   

# plot the pruned tree 
plot(pfit_gini, uniform=TRUE, 
     main="Pruned decision Tree for Bank Marketing")
text(pfit_gini, use.n=TRUE, all=TRUE, cex=.8)
prp(pfit_gini)
rpart.plot(decision_fit_gini)

#***************************************Using INFORMATION GAIN***************************************************
decision_fit_info <- rpart(y ~ age + job + marital + education + default + balance + housing + loan + contact + day + month + duration + campaign + pdays + previous + poutcome,data= trainingdata, method = 'class', 
                           parms = list(split = "information"),
                           control = list(cp = 0.01))

printcp(decision_fit_info) # display the results
summary(decision_fit_info) # detailed summary of splits
plotcp(decision_fit_info) # visualize cross-validation results 

# plot tree 
plot(decision_fit_info,main="decision Tree for Bank Marketing ")
text(decision_fit_info)

#nice looking tree
prp(decision_fit_info,varlen=3) #takes time to run since it is a huge tree
rpart.plot(decision_fit_info) #takes time to run since it is a huge tree


pred_tree_info <- predict(decision_fit_info,testdata[-17], type = 'prob')
pred_tree_info

plot(roc(testdata$y, pred_tree_info[,2]))
auc_info<- auc(testdata$y, pred_tree_info[,2])
auc_info

# Do the predictions on the test dataset (output CLASS not PROBABILITIES)
test_preds_info <- predict(decision_fit_info, testdata,type = 'class')
test_preds_info

# Create a confusion matrix and find accuracy
cm_treeinfo <- confusionMatrix(testdata$y, test_preds_info)
cm_treeinfo$table 
cm_treeinfo$overall

# prune the tree 
pfit_info<- prune(decision_fit_info, cp=0.02) # from cptable   

# plot the pruned tree 
plot(pfit_info, uniform=TRUE, 
     main="Pruned decision Tree for Bank Marketing")
text(pfit_info, use.n=TRUE, all=TRUE, cex=.15)

#nice looking tree
prp(pfit_info,varlen=3)
rpart.plot(pfit_info)


#************************************************SVM model**************************************************
#Note:All svm models execution takes time
library(e1071)

#*******************************************using linear kernel*********************************************
svm_model_linear <- svm(y ~ age + job + marital + education + default + balance + housing + loan + contact + day + month + duration + campaign + pdays + previous + poutcome,data= trainingdata,scale = TRUE,kernel = "linear")
#model execution takes time

# Do the predictions on the test dataset (output CLASS not PROBABILITIES)
predictions_svm_linear<-predict(svm_model_linear,testdata)
plot(predictions_svm_linear)

# Create a confusion matrix and find accuracy
cm_linear <- confusionMatrix(testdata$y, predictions_svm_linear)
cm_linear$table 
cm_linear$overall

#*****************************************using radial kernel*******************************************************
svm_model_radial <- svm(y ~ age + job + marital + education + default + balance + housing + loan + contact + day + month + duration + campaign + pdays + previous + poutcome,data= trainingdata,scale = TRUE,kernel = "radial")

# Do the predictions on the test dataset (output CLASS not PROBABILITIES)
predictions_svm_radial<-predict(svm_model_radial,testdata)
plot(predictions_svm_radial)

# Create a confusion matrix and find accuracy
cm_radial <- confusionMatrix(testdata$y, predictions_svm_radial)
cm_radial$table 
cm_radial$overall

#*********************************************using polyomial kernel********************************************************
svm_model_polynomial <- svm(y ~ age + job + marital + education + default + balance + housing + loan + contact + day + month + duration + campaign + pdays + previous + poutcome,data= trainingdata,scale = TRUE,kernel = "polynomial")

# Do the predictions on the test dataset (output CLASS not PROBABILITIES)
predictions_svm_polynomial<-predict(svm_model_polynomial,testdata)
plot(predictions_svm_polynomial)

# Create a confusion matrix and find accuracy
cm_polynomial <- confusionMatrix(testdata$y, predictions_svm_polynomial)
cm_polynomial$table 
cm_polynomial$overall
#**************************************using sigmoid kernel******************************************************************
svm_model_sigmoid <- svm(y ~ age + job + marital + education + default + balance + housing + loan + contact + day + month + duration + campaign + pdays + previous + poutcome,data= trainingdata,scale = TRUE,kernel = "sigmoid")

# Do the predictions on the test dataset (output CLASS not PROBABILITIES)
predictions_svm_sigmoid<-predict(svm_model_sigmoid,testdata)
plot(predictions_svm_sigmoid)

# Create a confusion matrix and find accuracy
cm_sigmoid <- confusionMatrix(testdata$y, predictions_svm_sigmoid)
cm_sigmoid$table 
cm_sigmoid$overall

#*********************************************************BOOSTING with adaboost**********************************************
library(ada)
# Create a forest
adaModel <- ada(y ~ .,data=trainingdata,control=rpart.control(maxdepth=30,cp=0.008000,minsplit=20,xval=10),iter=25)

predictions<-predict(adaModel,testdata[-17], type = 'prob')
predictions

plot(roc(testdata$y, predictions[,2]))
auc_boost<- auc(testdata$y, predictions[,2])
auc_boost

# Do the predictions on the test dataset (output CLASS not PROBABILITIES)
predictions_boost<-predict(adaModel,testdata)
predictions_boost

# Create a confusion matrix
cm_boost <- confusionMatrix(testdata$y, predictions_boost)
cm_boost$table 
cm_boost$overall
