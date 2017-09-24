hour.df <- read.csv(paste("hour.csv", sep=""))
data = hour.df

#Split dataset into training and test(70/30)
set.seed(42)
split <- sample(2,nrow(data),replace = TRUE,prob = c(0.70,0.30))
trainingdata <- data[split==1,]
testdata <- data[split==2,]

#Implement the gradient descent algorithm with batch update rule

gradientdescent<-function(output, input, threshold,alpha, iterations){
  threshold = 0.0001
  input = as.matrix(data.frame(rep(1,length(output)),input))
  m = dim(input)[1]
  beta = as.matrix(rnorm(n=dim(input)[2], mean=0,sd = 1))
  beta = t(beta)
  error = t(output) - beta%*%t(input)
  gradientinitial = -(2/m)%*%(error)%*%input
  betainitial = beta - alpha*(1/m)*gradientinitial
  cost = c()
  for(i in 1:iterations){
    cost = c(cost,sqrt(sum((t(output) - betainitial%*%t(input))^2)))
    error = t(output) - betainitial%*%t(input)
    gradient = -(2/m)%*%error%*%input
    betainitial = betainitial - alpha*(2/m)*gradient
    if(sqrt(sum(gradient^2)) <= threshold){
      break
    }
  }
  print(paste("Global minima of this cost function is",sqrt(sum(gradient^2))))
  
  values<-list("coef" = t(betainitial), "cost" = cost)
  print("beta coefficients are")
  print(t(betainitial))
  return(values)
}

#all features

trainingresult <- gradientdescent(output = trainingdata$cnt, input = data.frame(trainingdata$season,trainingdata$mnth,trainingdata$hr,trainingdata$holiday,trainingdata$weekday,trainingdata$workingday,trainingdata$weathersit,trainingdata$temp,trainingdata$atemp,trainingdata$hum,trainingdata$windspeed), alpha =0.05,  iterations = 1000)
testresult <- gradientdescent(output = testdata$cnt, input = data.frame(testdata$season,testdata$mnth,testdata$hr,testdata$holiday,testdata$weekday,testdata$workingday,testdata$weathersit,testdata$temp,testdata$atemp,testdata$hum,testdata$windspeed), alpha =0.05,  iterations = 1000)

plot(1:length(trainingresult$cost),trainingresult$cost,xlab = "iterations", ylab = "cost")
lines(1:length(trainingresult$cost),trainingresult$cost)

#3 random features
trainingresult <-  gradientdescent(output = trainingdata$cnt, input = data.frame(trainingdata$season,trainingdata$holiday, trainingdata$hum), alpha =0.05,  iterations = 100)
testresult <- gradientdescent(output = testdata$cnt, input = data.frame(testdata$season,testdata$holiday, testdata$hum), alpha =0.05,  iterations = 100)

#3 best features
trainingresult <-  gradientdescent(output = trainingdata$cnt, input = data.frame(trainingdata$season,trainingdata$atemp, trainingdata$hum), alpha =0.05,  iterations = 100)
testresult <- gradientdescent(output = testdata$cnt, input = data.frame(testdata$season,testdata$atemp, testdata$hum), alpha =0.05,  iterations = 100)
#Plotting the graph between Cost and Iterations

                                                          

