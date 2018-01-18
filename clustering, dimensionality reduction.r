library(dplyr)
library(broom)
library(tidyr)
library(mlbench)
library(caret)
library(ggplot2)
#*******************************************************K-Means dataset1******************************************************************#
rm(list = ls())
set.seed(123)
heart <- read.csv('C:/AML-BUAN 6341/CDSdata1.csv')
cor(heart)
heart$NSP <- as.factor(heart$NSP)
summary(heart)

#*****************************Trial 1-k = 2 because our data has two classes 0,1 and with 10 random initial sets**********************
kmean_heart <- kmeans(heart[1:21],centers = 2, nstart = 10)
kmean_heart
table(kmean_heart$cluster,heart$NSP)

#plot the clusters
kmean_heart$cluster <- as.factor(kmean_heart$cluster)
kmean_heart$centers #decide on which columns to plot the graph on
ggplot(heart, aes(ASTV,MSTV, color = kmean_heart$cluster)) + geom_point()
ggplot(heart, aes(ALTV,MLTV, color = kmean_heart$cluster)) + geom_point()
ggplot(heart, aes(ALTV,ASTV, color = kmean_heart$cluster)) + geom_point()

#************************************************Trial 2-K-Means with 5 random initial sets********************************************
kmean_heart <- kmeans(heart[1:21],centers = 2, nstart = 5)
table(kmean_heart$cluster,heart$NSP)

#plot the clusters
kmean_heart$cluster <- as.factor(kmean_heart$cluster)
kmean_heart$centers #decide on which columns to plot the graph on
ggplot(heart, aes(ASTV,MSTV, color = kmean_heart$cluster)) + geom_point()
ggplot(heart, aes(ALTV,MLTV, color = kmean_heart$cluster)) + geom_point()
ggplot(heart, aes(ALTV,ASTV, color = kmean_heart$cluster)) + geom_point()
ggplot(heart, aes(MLTV,ASTV, color = kmean_heart$cluster)) + geom_point()

#*****************************Trial 3-k = 4 and with 10 random initial sets **********************
kmean_heart <- kmeans(heart[1:21],centers = 4, nstart = 10)
table(kmean_heart$cluster,heart$NSP)

#plot the clusters
kmean_heart$cluster <- as.factor(kmean_heart$cluster)
kmean_heart$centers #decide on which columns to plot the graph on
ggplot(heart, aes(ASTV,MSTV, color = kmean_heart$cluster)) + geom_point()
ggplot(heart, aes(ALTV,MLTV, color = kmean_heart$cluster)) + geom_point()
ggplot(heart, aes(ALTV,ASTV, color = kmean_heart$cluster)) + geom_point()

#*****************************Trial 4-k = 4 and with 5 random initial sets**********************
kmean_heart <- kmeans(heart[1:21],centers = 4, nstart = 5)
table(kmean_heart$cluster,heart$NSP)

#plot the clusters
kmean_heart$cluster <- as.factor(kmean_heart$cluster)
kmean_heart$centers #decide on which columns to plot the graph on
ggplot(heart, aes(ASTV,MSTV, color = kmean_heart$cluster)) + geom_point()
ggplot(heart, aes(ALTV,MLTV, color = kmean_heart$cluster)) + geom_point()
ggplot(heart, aes(ALTV,ASTV, color = kmean_heart$cluster)) + geom_point()

#*****************************Trial 5-k = 6 and with 10 random initial sets**********************
kmean_heart <- kmeans(heart[1:21],centers = 6, nstart = 10)
table(kmean_heart$cluster,heart$NSP)

#plot the clusters
kmean_heart$cluster <- as.factor(kmean_heart$cluster)
kmean_heart$centers #decide on which columns to plot the graph on
ggplot(heart, aes(ASTV,MSTV, color = kmean_heart$cluster)) + geom_point()
ggplot(heart, aes(ALTV,MLTV, color = kmean_heart$cluster)) + geom_point()
ggplot(heart, aes(ALTV,ASTV, color = kmean_heart$cluster)) + geom_point()

#*****************************Trial 6-k = 6 and with 5 random initial sets**********************
kmean_heart <- kmeans(heart[1:21],centers = 6, nstart = 5)
table(kmean_heart$cluster,heart$NSP)

#plot the clusters
kmean_heart$cluster <- as.factor(kmean_heart$cluster)
kmean_heart$centers #decide on which columns to plot the graph on
ggplot(heart, aes(ASTV,MSTV, color = kmean_heart$cluster)) + geom_point()
ggplot(heart, aes(ALTV,MLTV, color = kmean_heart$cluster)) + geom_point()
ggplot(heart, aes(ALTV,ASTV, color = kmean_heart$cluster)) + geom_point()

#*****************************Trial 7-k = 8 and with 10 random initial sets**********************
kmean_heart <- kmeans(heart[1:21],centers = 8, nstart = 10)
table(kmean_heart$cluster,heart$NSP)

#plot the clusters
kmean_heart$cluster <- as.factor(kmean_heart$cluster)
kmean_heart$centers #decide on which columns to plot the graph on
ggplot(heart, aes(ASTV,MSTV, color = kmean_heart$cluster)) + geom_point()
ggplot(heart, aes(ALTV,MLTV, color = kmean_heart$cluster)) + geom_point()
ggplot(heart, aes(ALTV,ASTV, color = kmean_heart$cluster)) + geom_point()
ggplot(heart, aes(MLTV,ASTV, color = kmean_heart$cluster)) + geom_point()

#*****************************Trial 8-k = 8 and with 5 random initial sets**********************
kmean_heart <- kmeans(heart[1:21],centers = 8, nstart = 5)
table(kmean_heart$cluster,heart$NSP)

#plot the clusters
kmean_heart$cluster <- as.factor(kmean_heart$cluster)
kmean_heart$centers #decide on which columns to plot the graph on
ggplot(heart, aes(ASTV,MSTV, color = kmean_heart$cluster)) + geom_point()
ggplot(heart, aes(ALTV,MLTV, color = kmean_heart$cluster)) + geom_point()
ggplot(heart, aes(ALTV,ASTV, color = kmean_heart$cluster)) + geom_point()

#*****************************Trial 9-k = 10 and with 10 random initial sets**********************
kmean_heart <- kmeans(heart[1:21],centers = 10, nstart = 10)
kmean_heart$cluster
table(kmean_heart$cluster,heart$NSP)

#plot the clusters
kmean_heart$cluster <- as.factor(kmean_heart$cluster)
kmean_heart$centers #decide on which columns to plot the graph on
ggplot(heart, aes(ASTV,MSTV, color = kmean_heart$cluster)) + geom_point()
ggplot(heart, aes(ALTV,MLTV, color = kmean_heart$cluster)) + geom_point()
ggplot(heart, aes(ALTV,ASTV, color = kmean_heart$cluster)) + geom_point()

#*****************************Trial 10-k = 10 and with 5 random initial sets plus clustering comparison**********************
kclust <- kmeans(heart[1:21],centers = 10, nstart = 5)
table(kclust$cluster,heart$NSP)

#plot the clusters
kclust$cluster <- as.factor(kclust$cluster)
kclust$centers #decide on which columns to plot the graph on
ggplot(heart, aes(ASTV,MSTV, color = kclust$cluster)) + geom_point()
ggplot(heart, aes(ALTV,MLTV, color = kclust$cluster)) + geom_point()
ggplot(heart, aes(ALTV,ASTV, color = kclust$cluster)) + geom_point()

kclusts <- data.frame(k=1:10) %>% group_by(k) %>% do(kclust=kmeans(heart[1:21], .$k))
clusters <- kclusts %>% group_by(k) %>% do(tidy(.$kclust[[1]]))
assignments <- kclusts %>% group_by(k) %>% do(augment(.$kclust[[1]], heart[1:21]))
clusterings <- kclusts %>% group_by(k) %>% do(glance(.$kclust[[1]]))

#Now we can plot the original points, with each point colored according to the original cluster:
p1 <- ggplot(heart, aes(ASTV,MSTV)) + geom_point(aes(color=kclust$cluster))
p1

ggplot(clusterings, aes(k, tot.withinss)) + geom_line()

#************************************************************K-Means Dataset-2*****************************************************************
rm(list=ls())

#setwd()
wine <- read.csv("C:/AML-BUAN 6341/winequality-red1.csv")
cor(wine)
wine$quality <- as.factor(wine$quality)
summary(wine)

#*****************************Trial 1-k = 2 because our data has two classes 0,1 and with 10 random initial sets**********************
kmean_wine <- kmeans(wine[1:11],centers = 2, nstart = 10)
table(kmean_wine$cluster,wine$quality)

#plot the clusters
kmean_wine$cluster <- as.factor(kmean_wine$cluster)
kmean_wine$centers #decide on which columns to plot the graph on
ggplot(wine, aes(free_sulfur_dioxide,total_sulfur_dioxide, color = kmean_wine$cluster)) + geom_point()

#************************************************Trial 2-K-Means with 5 random initial sets********************************************
kmean_wine2 <- kmeans(wine[1:11],centers = 2, nstart = 5)
table(kmean_wine2$cluster,wine$quality)

#plot the clusters
kmean_wine2$cluster <- as.factor(kmean_wine2$cluster)
ggplot(wine, aes(free_sulfur_dioxide,total_sulfur_dioxide, color = kmean_wine2$cluster)) + geom_point()

#*****************************Trial 3-k = 4 and with 10 random initial sets **********************
kmean_wine <- kmeans(wine[1:11],centers = 4, nstart = 10)
table(kmean_wine$cluster,wine$quality)

#plot the clusters
kmean_wine$cluster <- as.factor(kmean_wine$cluster)
kmean_wine$centers #decide on which columns to plot the graph on
ggplot(wine, aes(free_sulfur_dioxide,total_sulfur_dioxide, color = kmean_wine$cluster)) + geom_point()

#*****************************Trial 4-k = 4 and with 5 random initial sets**********************
kmean_wine <- kmeans(wine[1:11],centers = 4, nstart = 5)
table(kmean_wine$cluster,wine$quality)

#plot the clusters
kmean_wine$cluster <- as.factor(kmean_wine$cluster)
kmean_wine$centers #decide on which columns to plot the graph on
ggplot(wine, aes(free_sulfur_dioxide,total_sulfur_dioxide, color = kmean_wine$cluster)) + geom_point()

#*****************************Trial 5-k = 6 and with 10 random initial sets**********************
kmean_wine <- kmeans(wine[1:11],centers = 6, nstart = 10)
table(kmean_wine$cluster,wine$quality)

#plot the clusters
kmean_wine$cluster <- as.factor(kmean_wine$cluster)
kmean_wine$centers #decide on which columns to plot the graph on
ggplot(wine, aes(free_sulfur_dioxide,total_sulfur_dioxide, color = kmean_wine$cluster)) + geom_point()

#*****************************Trial 6-k = 6 and with 5 random initial sets**********************
kmean_wine <- kmeans(wine[1:11],centers = 6, nstart = 5)
table(kmean_wine$cluster,wine$quality)

#plot the clusters
kmean_wine$cluster <- as.factor(kmean_wine$cluster)
kmean_wine$centers #decide on which columns to plot the graph on
ggplot(wine, aes(free_sulfur_dioxide,total_sulfur_dioxide, color = kmean_wine$cluster)) + geom_point()

#*****************************Trial 7-k = 8 and with 10 random initial sets**********************
kmean_wine <- kmeans(wine[1:11],centers = 8, nstart = 10)
table(kmean_wine$cluster,wine$quality)

#plot the clusters
kmean_wine$cluster <- as.factor(kmean_wine$cluster)
kmean_wine$centers #decide on which columns to plot the graph on
ggplot(wine, aes(free_sulfur_dioxide,total_sulfur_dioxide, color = kmean_wine$cluster)) + geom_point()

#*****************************Trial 8-k = 8 and with 5 random initial sets**********************
kmean_wine <- kmeans(wine[1:11],centers = 8, nstart = 5)
table(kmean_wine$cluster,wine$quality)

#plot the clusters
kmean_wine$cluster <- as.factor(kmean_wine$cluster)
kmean_wine$centers #decide on which columns to plot the graph on
ggplot(wine, aes(free_sulfur_dioxide,total_sulfur_dioxide, color = kmean_wine$cluster)) + geom_point()

#*****************************Trial 9-k = 10 and with 10 random initial sets**********************
kmean_wine <- kmeans(wine[1:11],centers = 10, nstart = 10)
table(kmean_wine$cluster,wine$quality)

#plot the clusters
kmean_wine$cluster <- as.factor(kmean_wine$cluster)
kmean_wine$centers #decide on which columns to plot the graph on
ggplot(wine, aes(free_sulfur_dioxide,total_sulfur_dioxide, color = kmean_wine$cluster)) + geom_point()

#*****************************Trial 10-k = 10 and with 5 random initial sets plus clustering comparison**********************
library(dplyr)
library(broom)
library(tidyr)
kclust <- kmeans(wine[1:11],centers = 10, nstart = 5)
table(kclust$cluster,wine$quality)

#plot the clusters
kclust$cluster <- as.factor(kclust$cluster)
kclust$centers #decide on which columns to plot the graph on
ggplot(wine, aes(free_sulfur_dioxide,total_sulfur_dioxide, color = kclust$cluster)) + geom_point()

kclusts <- data.frame(k=1:10) %>% group_by(k) %>% do(kclust=kmeans(wine[1:11], .$k))
clusters <- kclusts %>% group_by(k) %>% do(tidy(.$kclust[[1]]))
assignments <- kclusts %>% group_by(k) %>% do(augment(.$kclust[[1]], wine[1:11]))
clusterings <- kclusts %>% group_by(k) %>% do(glance(.$kclust[[1]]))

#Now we can plot the original points, with each point colored according to the original cluster:
p1 <- ggplot(wine, aes(free_sulfur_dioxide,total_sulfur_dioxide)) + geom_point(aes(color=kclust$cluster))
p1

ggplot(clusterings, aes(k, tot.withinss)) + geom_line()


#****************************************************************EM clustering dataset1*******************************************************
rm(list = ls())
heart <- read.csv('C:/AML-BUAN 6341/CDSdata1.csv')
heart$NSP <- as.factor(heart$NSP)
library(mclust)
em_heart <- Mclust(heart[1:21]) #Execution takes time
em_heart
summary(em_heart)
plot(em_heart,what="BIC")
plot(em_heart,what="classification") #too many graphs. each attribute vs other attributes. 11*11 graphs
plot(em_heart,what="uncertainty") #too many graphs. each attribute vs other attributes. 11*11 graphs
plot(em_heart,what="density")

#************************************************************EM clusterings Dataset-2*****************************************************************
rm(list=ls())
wine <- read.csv("C:/AML-BUAN 6341/winequality-red1.csv")
wine$quality <- as.factor(wine$quality)
em_wine <- Mclust(wine[1:11]) #Execution takes time
em_wine
summary(em_wine)
X = wine[,-12]
class.d = wine$quality
clPairs(X,class.d)

plot(em_wine,what="BIC")
plot(em_wine,what="classification") #too many graphs. each attribute vs other attributes. 11*11 graphs
plot(em_wine,what="uncertainty") #too many graphs. each attribute vs other attributes. 11*11 graphs
plot(em_wine,what="density")

#***************************************************PCA dataset1**********************************************
rm(list = ls())
heart <- read.csv('C:/AML-BUAN 6341/CDSdata1.csv')
pca <- princomp(heart[1:21],cor = TRUE, scores = TRUE)
summary(pca)

#Loadings of principal components
pca$loadings

#Scree plot of eigenvalues
plot(pca)
screeplot(pca,type = "line", main = "Scree plot")

#Biplot of score variables
biplot(pca)

#Scores of the components
pca$scores[1:21,]


#***************************************************PCA dataset2**********************************************
rm(list=ls())
wine <- read.csv("C:/AML-BUAN 6341/winequality-red1.csv")

pca <- princomp(wine[1:11],cor = TRUE, scores = TRUE)
summary(pca)

#Loadings of principal components
pca$loadings

#Scree plot of eigenvalues
plot(pca)
screeplot(pca,type = "line", main = "Scree plot")

#Biplot of score variables
biplot(pca)

#Scores of the components
pca$scores[1:11,]

#***************************************************Forward Selection dataset1**********************************************
rm(list = ls())
heart <- read.csv('C:/AML-BUAN 6341/CDSdata1.csv')
FitAll <- lm(NSP ~ .,data = heart)
FitStart <- lm(NSP ~ 1,data = heart)
step(FitStart,direction = "forward",scope = formula(FitAll))


#***************************************************Feature Selection dataset2**********************************************
rm(list=ls())
wine <- read.csv("C:/AML-BUAN 6341/winequality-red1.csv")
FitAll <- lm(quality ~ .,data = wine)
FitStart <- lm(quality ~ 1,data = wine)
step(FitStart,direction = "forward",scope = formula(FitAll))


#***************************************************ICA-Trial 1-Extracting 8 components*********************************************************
library(fastICA)
#=============================ICA dataset 1===============#
rm(list=ls())
# load libraries
library(mlbench)
library(caret)
# load the dataset
heart <- read.csv("C:/AML-BUAN 6341/CDSdata1.csv")
str(heart)
heart$NSP  <- as.factor(heart$NSP)
str(heart)
# calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(heart[,1:21], method=c("center", "scale", "ica"), n.comp=8)
# summarize transform parameters
print(preprocessParams)

# transform the dataset using the parameters
transformed <- predict(preprocessParams, heart[,1:21])
# summarize the transformed dataset
summary(transformed)

#============================Apply k means on this ICA output======================#
#**********k =  and with 10 random initial sets plus clustering comparison*******
set.seed(123)
kclust <- kmeans(transformed,centers = 10, nstart = 5)
table(kclust$cluster,heart$NSP)

#plot the clusters
kclust$cluster <- as.factor(kclust$cluster)
kclust$centers #decide on which columns to plot the graph on
ggplot(transformed, aes(ICA4,ICA5, color = kclust$cluster)) + geom_point()

kclusts <- data.frame(k=1:10) %>% group_by(k) %>% do(kclust=kmeans(transformed, .$k))
clusters <- kclusts %>% group_by(k) %>% do(tidy(.$kclust[[1]]))
assignments <- kclusts %>% group_by(k) %>% do(augment(.$kclust[[1]], transformed))
clusterings <- kclusts %>% group_by(k) %>% do(glance(.$kclust[[1]]))

ggplot(clusterings, aes(k, tot.withinss)) + geom_line()



#====================Apply EM on this ICA output==========================#
ica_em_heart <- Mclust(transformed[1:8]) #Execution takes time

summary(ica_em_heart)

X = transformed[1:8]
class.d = heart$NSP
clPairs(X,class.d)

plot(ica_em_heart,what="BIC")


#=============================ICA dataset 2===============#
rm(list=ls())
# load libraries
library(mlbench)
library(caret)
# load the dataset
wine <- read.csv("C:/AML-BUAN 6341/winequality-red1.csv")
str(wine)
wine$quality  <- as.factor(wine$quality)
str(wine)
# calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(wine[,1:11], method=c("center", "scale", "ica"), n.comp=8)
# summarize transform parameters
print(preprocessParams)

# transform the dataset using the parameters
transformed <- predict(preprocessParams, wine[,1:11])
# summarize the transformed dataset
summary(transformed)
#**********Trial 10-k = 10 and with 5 random initial sets plus clustering comparison*******
set.seed(123)
kclust <- kmeans(transformed,centers = 2, nstart = 5)
table(kclust$cluster,wine$quality)

#plot the clusters
kclust$cluster <- as.factor(kclust$cluster)
kclust$centers #decide on which columns to plot the graph on
ggplot(transformed, aes(ICA1,ICA2, color = kclust$cluster)) + geom_point()

kclusts <- data.frame(k=1:10) %>% group_by(k) %>% do(kclust=kmeans(transformed, .$k))
clusters <- kclusts %>% group_by(k) %>% do(tidy(.$kclust[[1]]))
assignments <- kclusts %>% group_by(k) %>% do(augment(.$kclust[[1]], transformed))
clusterings <- kclusts %>% group_by(k) %>% do(glance(.$kclust[[1]]))

#Now we can plot the original points, with each point colored according to the original cluster:
p1 <- ggplot(transformed, aes(ICA1,ICA2)) + geom_point(aes(color=kclust$cluster))
p1

ggplot(clusterings, aes(k, tot.withinss)) + geom_line()




#====================Apply EM on this ICA output==========================#
ica_em_wine <- Mclust(transformed[1:8]) #Execution takes time

summary(ica_em_wine)

X = transformed[1:8]
class.d = wine$quality
clPairs(X,class.d)

plot(ica_em_wine,what="BIC")




#***************************************************Randomized Projections**********************************************
library(RandPro)
library(mclust)

#******************Randomized Projections***************
#****************Wine data set************#
rm(list=ls())
wine <- read.csv('C:/AML-BUAN 6341/winequality-red.csv')
str(wine)
wine$quality <- as.factor(wine$quality)
str(wine)
est <- meVVV(wine[,-12], unmap(wine[,12]))
randProj(wine[,-12], seeds=1:4, parameters = est$parameters, z = est$z, what = "classification", main = TRUE) 
randProj(wine[1:11], seeds=1:4, parameters = est$parameters, z = est$z, truth = wine[,12], what = "errors", main = TRUE) 
randProj(wine[1:11], seeds=1:4, parameters = est$parameters, z = est$z, what = "uncertainty", main = TRUE)
                 
#****************CDS data set************#
rm(list=ls())
cds <- read.csv('C:/AML-BUAN 6341/CDSdata1.csv')
str(cds)
cds$NSP <- as.factor(cds$NSP)
str(cds)
est <- meVVV(cds[1:21], unmap(cds[,22]))
par(pty = "s", mfrow = c(1,1))
randProj(cds[1:21], seeds=1:3, parameters = est$parameters, z = est$z, what = "classification", main = TRUE) 
randProj(cds[1:21], seeds=2, parameters = est$parameters, z = est$z,
truth = cds[,22], what = "errors", main = TRUE) 
randProj(cds[1:21], seeds=1:3, parameters = est$parameters, z = est$z,what = "uncertainty", main = TRUE)
                 


#Run the clustering algorithms again, this time after applying dimensionality reduction. Describe the difference compared to previous experimentation
#*******************************************************K-Means dataset1 with selected features******************************************************************#
rm(list = ls())
heart <- read.csv('C:/AML-BUAN 6341/selectedCDSdata.csv')
heart$NSP <- as.factor(heart$NSP)

kclust <- kmeans(heart[1:15],centers = 2, nstart = 10)
table(kclust$cluster,heart$NSP)

#plot the clusters
kclust$cluster <- as.factor(kclust$cluster)
kclust$centers #decide on which columns to plot the graph on
ggplot(heart, aes(ASTV,ALTV, color = kclust$cluster)) + geom_point()

kclusts <- data.frame(k=1:10) %>% group_by(k) %>% do(kclust=kmeans(heart[1:15], .$k))
clusters <- kclusts %>% group_by(k) %>% do(tidy(.$kclust[[1]]))
assignments <- kclusts %>% group_by(k) %>% do(augment(.$kclust[[1]], heart[1:15]))
clusterings <- kclusts %>% group_by(k) %>% do(glance(.$kclust[[1]]))

#Now we can plot the original points, with each point colored according to the original cluster:
p1 <- ggplot(heart, aes(ASTV,ALTV)) + geom_point(aes(color=kclust$cluster))
p1

ggplot(clusterings, aes(k, tot.withinss)) + geom_line()

#************************************************************K-Means Dataset-2 with selected features*****************************************************************
rm(list=ls())

wine <- read.csv("C:/AML-BUAN 6341/selectedwinequality-red.csv")
kmean_wine <- kmeans(wine[1:7],centers = 2, nstart = 10)
table(kmean_wine$cluster,wine$quality)

#plot the clusters
kmean_wine$cluster <- as.factor(kmean_wine$cluster)
kmean_wine$centers #decide on which columns to plot the graph on
ggplot(wine, aes(free_sulfur_dioxide,total_sulfur_dioxide, color = kmean_wine$cluster)) + geom_point()

kclust <- kmeans(wine[1:7],centers = 2, nstart = 10)
table(kclust$cluster,wine$quality)

#plot the clusters
kclust$cluster <- as.factor(kclust$cluster)
kclust$centers #decide on which columns to plot the graph on
ggplot(wine, aes(free_sulfur_dioxide,total_sulfur_dioxide, color = kclust$cluster)) + geom_point()

kclusts <- data.frame(k=1:10) %>% group_by(k) %>% do(kclust=kmeans(wine[1:7], .$k))
clusters <- kclusts %>% group_by(k) %>% do(tidy(.$kclust[[1]]))
assignments <- kclusts %>% group_by(k) %>% do(augment(.$kclust[[1]], wine[1:7]))
clusterings <- kclusts %>% group_by(k) %>% do(glance(.$kclust[[1]]))

#Now we can plot the original points, with each point colored according to the original cluster:
p1 <- ggplot(wine, aes(free_sulfur_dioxide,total_sulfur_dioxide)) + geom_point(aes(color=kclust$cluster))
p1

ggplot(clusterings, aes(k, tot.withinss)) + geom_line()



#****************************************************************EM clustering dataset1*******************************************************
rm(list = ls())
heart <- read.csv('C:/AML-BUAN 6341/selectedCDSdata.csv')
heart$NSP <- as.factor(heart$NSP)
library(mclust)
em_heart <- Mclust(heart[1:15]) #Execution takes time
em_heart
summary(em_heart)
plot(em_heart,what="BIC")
plot(em_heart,what="classification") #too many graphs. each attribute vs other attributes. 11*11 graphs
plot(em_heart,what="uncertainty") #too many graphs. each attribute vs other attributes. 11*11 graphs
plot(em_heart,what="density")

#************************************************************EM clusterings Dataset-2*****************************************************************
rm(list=ls())
wine <- read.csv("C:/AML-BUAN 6341/selectedwinequality-red.csv")
wine$quality <- as.factor(wine$quality)
em_wine <- Mclust(wine[1:7]) #Execution takes time
em_wine
summary(em_wine)
X = wine[,-12]
class.d = wine$quality
clPairs(X,class.d)
plot(em_wine,what="BIC")
plot(em_wine,what="classification") #too many graphs. each attribute vs other attributes. 11*11 graphs
plot(em_wine,what="uncertainty") #too many graphs. each attribute vs other attributes. 11*11 graphs
plot(em_wine,what="density")

#************************************************************EM clusterings Dataset-2*****************************************************************
rm(list=ls())
wine <- read.csv("C:/AML-BUAN 6341/winequality-red1.csv")

em_wine <- Mclust(wine[1:11]) #Execution takes time
em_wine
summary(em_wine)


plot(em_wine,what="BIC")
plot(em_wine,what="classification") #too many graphs. each attribute vs other attributes. 11*11 graphs
plot(em_wine,what="uncertainty") #too many graphs. each attribute vs other attributes. 11*11 graphs
plot(em_wine,what="density")

#Run your neural network learner from project 3 on the data after dimensionality reduction (from task 2). Explain and plot your observations (error rates, etc.)
#********************************************Artificial Neural Networks dataset1******************************************************#
cds <- read.csv("C:/AML-BUAN 6341/selectedCDSdata.csv",header = TRUE, sep = ",")
cds$NSP <- as.numeric(cds$NSP)
#create test and training data sets
set.seed(123)
train_rows <- sample(nrow(cds),0.7*nrow(cds),replace=FALSE)
cds_train <- as.data.frame(cds[train_rows,])
cds_test <- as.data.frame(cds[-train_rows,])
rm(train_rows)

formula <- "NSP~LB+AC+UC+DS+DP+ASTV+ALTV+MLTV+Width+Max+Mode+Mean+Median+Variance+Tendency"
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
library(DMwR)
library(caret)
library(dplyr)
library(neuralnet)
library(functional)
library(pROC)
library(ModelMetrics)
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
test1 <- compute(trial1,cds_test[1:15])

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

#********************************************Artificial Neural Networks dataset2******************************************************#
rm(list=ls())
library(dplyr)
library(neuralnet)
library(functional)

setwd("C:/AML-BUAN 6341/")
wine <- read.csv("selectedwinequality-red.csv",header = TRUE, sep = ",")


#create test and training data sets
set.seed(123)
train_rows <- sample(nrow(wine),0.7*nrow(wine),replace=FALSE)
wine_train <- as.data.frame(wine[train_rows,])
wine_test <- as.data.frame(wine[-train_rows,])
rm(train_rows)

#=========================== Artificial Neural Networks for wine quality data set(dataset2)===========================#
formula <- "quality ~ volatile_acidity + chlorides + free_sulfur_dioxide + total_sulfur_dioxide + pH + sulphates + alcohol"
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
test1 <- compute(trial1,wine_test[1:7])

#Confusion matrix
library(ModelMetrics)
confusionMatrix(wine_test$quality,test1$net.result)

#ROC Test data
plot(roc(wine_test$quality, as.numeric(test1$net.result)))

#AUC Test Data
auc <- auc(wine_test$quality, as.numeric(test1$net.result))
print(paste("ANN-Logistic- Nodes 3-No Cross Validation-AUC on Test data = ",auc))




