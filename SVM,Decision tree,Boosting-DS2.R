rm(list = ls())
heart.df <- read.csv('C:/AML-BUAN 6341/CDSdata.csv')
data = heart.df

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
decision_fit_gini <- rpart(NSP ~ LB + AC + FM + UC + DL + DS + DP + ASTV + MSTV + ALTV + MLTV + Width + Min + Max + Nmax + Nzeros + Mode + Mean + Median + Variance + Tendency,
                           data= trainingdata,method="class", parms = list(split = "gini"),
                           control = list(cp = 0.01))

printcp(decision_fit_gini) # display the results
summary(decision_fit_gini) # detailed summary of splits
plotcp(decision_fit_gini) # visualize cross-validation results 

# plot tree 
plot(decision_fit_gini,main="decision Tree for Heart condition ")
text(decision_fit_gini)


#nice looking tree
prp(decision_fit_gini,varlen=3)
rpart.plot(decision_fit_gini)

pred_tree_gini <- predict(decision_fit_gini,testdata[-22])
pred_tree_gini

library(pROC)
plot(roc(testdata$NSP, pred_tree_gini[ ,2]))
auc_gini<- auc(testdata$NSP, pred_tree_gini[ ,2])
auc_gini

# Do the predictions on the test dataset (output CLASS not PROBABILITIES)
test_preds_gini <- predict(decision_fit_gini, testdata,type = 'class' )
test_preds_gini

# Create a confusion matrix
cm_treegini <- confusionMatrix(testdata$NSP, test_preds_gini)
cm_treegini$table 
cm_treegini$overall

# prune the tree 
pfit_gini<- prune(decision_fit_gini, cp=0.005) # from cptable   

# plot the pruned tree 
plot(pfit_gini, uniform=TRUE, 
     main="Pruned decision Tree for Heart condition")
text(pfit_gini, use.n=TRUE, all=TRUE, cex=.8)
prp(pfit_gini)



#***************************************Using INFORMATION GAIN***************************************************
 decision_fit_info <- rpart(NSP ~ LB + AC + FM + UC + DL + DS + DP + ASTV + MSTV + ALTV + MLTV + Width + Min + Max + Nmax + Nzeros + Mode + Mean + Median + Variance + Tendency, data= trainingdata, method = 'class', 
                           parms = list(split = "information"),
                           control = list(cp = 0.01))

printcp(decision_fit_info) # display the results
summary(decision_fit_info) # detailed summary of splits
plotcp(decision_fit_info) # visualize cross-validation results 

# plot tree 
plot(decision_fit_info,main="decision Tree for Heart condition ")
text(decision_fit_info)

#nice looking tree
prp(decision_fit_info,varlen=3) #takes time to run since it is a huge tree
rpart.plot(decision_fit_info) #takes time to run since it is a huge tree


pred_tree_info <- predict(decision_fit_info,testdata[-22], type = 'prob')
pred_tree_info

plot(roc(testdata$NSP, pred_tree_info[,2]))
auc_info<- auc(testdata$NSP, pred_tree_info[,2])
auc_info

# Do the predictions on the test dataset (output CLASS not PROBABILITIES)
test_preds_info <- predict(decision_fit_info, testdata,type = 'class')
test_preds_info

# Create a confusion matrix
cm_treeinfo <- confusionMatrix(testdata$NSP, test_preds_info)
cm_treeinfo$table 
cm_treeinfo$overall

# prune the tree 
pfit_info<- prune(decision_fit_info, cp=0.005) # from cptable   

# plot the pruned tree 
plot(pfit_info, uniform=TRUE, 
     main="Pruned decision Tree for Heart Condition")
text(pfit_info, use.n=TRUE, all=TRUE, cex=.15)

#nice looking tree
prp(pfit_info,varlen=3)
rpart.plot(pfit_info)


#************************************************SVM model**************************************************
#Note:All svm models execution takes time
library(e1071)

#*******************************************using linear kernel*********************************************
svm_model_linear <- svm(NSP ~ LB + AC + FM + UC + DL + DS + DP + ASTV + MSTV + ALTV + MLTV + Width + Min + Max + Nmax + Nzeros + Mode + Mean + Median + Variance + Tendency,data= trainingdata,scale = TRUE,kernel = "linear")

# Do the predictions on the test dataset (output CLASS not PROBABILITIES)
predictions_svm_linear <- predict(svm_model_linear,testdata,type = 'class')
plot(predictions_svm_linear)

cm_linear <- confusionMatrix(testdata$NSP, predictions_svm_linear)
cm_linear$table 
cm_linear$overall

#**********************************************tuning with linear kernel************************************
library(kernlab)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
set.seed(123)
svm_Linear_Grid <- train(NSP ~., data = trainingdata, method = "svmLinear",
                         trControl=trctrl,
                         tuneGrid = grid,
                         tuneLength = 10)
plot(svm_Linear_Grid)

#*****************************************using radial kernel*******************************************************
svm_model_radial <- svm(NSP ~ LB + AC + FM + UC + DL + DS + DP + ASTV + MSTV + ALTV + MLTV + Width + Min + Max + Nmax + Nzeros + Mode + Mean + Median + Variance + Tendency,data= trainingdata,scale = TRUE,kernel = "radial")

# Do the predictions on the test dataset (output CLASS not PROBABILITIES)
predictions_svm_radial<-predict(svm_model_radial,testdata)
plot(predictions_svm_radial)

# Create a confusion matrix
cm_radial <- confusionMatrix(testdata$NSP, predictions_svm_radial)
cm_radial$table 
cm_radial$overall
#*********************************************using polyomial kernel********************************************************
svm_model_polynomial <- svm(NSP ~ LB + AC + FM + UC + DL + DS + DP + ASTV + MSTV + ALTV + MLTV + Width + Min + Max + Nmax + Nzeros + Mode + Mean + Median + Variance + Tendency,data= trainingdata,scale = TRUE,kernel = "polynomial")

# Do the predictions on the test dataset (output CLASS not PROBABILITIES)
predictions_svm_polynomial<-predict(svm_model_polynomial,testdata)
plot(predictions_svm_polynomial)

# Create a confusion matrix
cm_polynomial <- confusionMatrix(testdata$NSP, predictions_svm_polynomial)
cm_polynomial$table 
cm_polynomial$overall

#**************************************using sigmoid kernel******************************************************************
svm_model_sigmoid <- svm(NSP ~ LB + AC + FM + UC + DL + DS + DP + ASTV + MSTV + ALTV + MLTV + Width + Min + Max + Nmax + Nzeros + Mode + Mean + Median + Variance + Tendency,data= trainingdata,scale = TRUE,kernel = "sigmoid")

# Do the predictions on the test dataset (output CLASS not PROBABILITIES)
predictions_svm_sigmoid<-predict(svm_model_sigmoid,testdata)
plot(predictions_svm_sigmoid)

# Create a confusion matrix
cm_sigmoid <- confusionMatrix(testdata$NSP, predictions_svm_sigmoid)
cm_sigmoid$table 
cm_sigmoid$overall

#*************************************************tuning with radial kernel************************************
set.seed(123)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
svm_Radial_Grid <- train(NSP ~.
                         ,data = trainingdata
                         ,method = "svmRadial"
                         ,trControl=trctrl
                         ,tuneLength = 10)
plot(svm_Radial_Grid)

#*********************************************************BOOSTING with adaboost**********************************************
library(ada)
# Create a forest
adaModel <- ada(NSP ~ LB + AC + FM + UC + DL + DS + DP + ASTV + MSTV + ALTV + MLTV + Width + Min + Max + Nmax + Nzeros + Mode + Mean + Median + Variance + Tendency,
                data=trainingdata,
                control=rpart.control(maxdepth=30,cp=0.100000,minsplit=20,xval=10),iter=50)

predictions<-predict(adaModel,testdata[-23], type = 'prob')
predictions

plot(roc(testdata$NSP, predictions[,2]))
auc_boost<- auc(testdata$NSP, predictions[,2])
auc_boost

# Do the predictions on the test dataset (output CLASS not PROBABILITIES)
predictions_boost<-predict(adaModel,testdata)
predictions_boost

# Create a confusion matrix
cm_boost <- confusionMatrix(testdata$NSP, predictions_boost)
cm_boost$table 
cm_boost$overall
