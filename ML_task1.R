data <- read.csv("hour.csv")
hours <-data[c(3,5:14,17)]

#scaling of features 
hours$season <- ((hours$season-2.502)/3)
hours$mnth <- ((hours$mnth - 6.538)/11)
hours$hr <- ((hours$hr - 11.55)/23)
hours$weekday <- ((hours$weekday - 3.004)/6)
hours$weathersit <- ((hours$weathersit - 1.425)/3)


#split dataset into training and test(70/30)
set.seed(42)
split <- sample(2,nrow(hours),replace = TRUE,prob = c(0.70,0.30))
trainingdata <- hours[split==1,]
testdata <- hours[split==2,]

hoursinput <- trainingdata[c(1:11)]
hoursoutput <- trainingdata[12]

#gradient batch update
gradientdescent <- function(hoursinput,hoursoutput,learningrate,conversionthreshold,maxiterations)
{
  season <- runif(1,30,32)
  mnth <- runif(1,19,21)
  hr <- runif(1,-1,1)
  holiday <- runif(1,7,9)
  weekday <- runif(1,-21,-19)
  workingday <- runif(1,1,3)
  weathersit <- runif(1,2,4)
  temp <- runif(1,-3,-1)
  atemp <- runif(1,115,120)
  hum <- runif(1,200,205)
  windspeed <- runif(1,-230,-220)
  constant <- runif(1,20,25)
  
  #define length
  m <- length(hoursoutput$cnt)
  yhat <- constant + season*hoursinput$season + mnth*hoursinput$mnth + hr*hoursinput$hr + holiday*hoursinput$holiday + weekday*hoursinput$weekday + workingday*hoursinput$workingday + weathersit*hoursinput$weathersit + temp*hoursinput$temp + atemp*hoursinput$atemp + hum*hoursinput$hum + windspeed*hoursinput$windspeed
  
  MSE <- sum(((hoursoutput$cnt - yhat)^2)/(2*m))
  converged <- F
  iterations <- 0
  
  while(converged == F)
  {
    season_new <- season - learningrate*((1/m)*(sum((hoursoutput$cnt - yhat)*season)))
    mnth_new <- mnth - learningrate*((1/m)*(sum((hoursoutput$cnt - yhat)*mnth)))
    hr_new <- hr - learningrate*((1/m)*(sum((hoursoutput$cnt - yhat)*hr)))
    holiday_new <- holiday - learningrate*((1/m)*(sum((hoursoutput$cnt - yhat)*holiday)))
    weekday_new <- weekday - learningrate*((1/m)*(sum((hoursoutput$cnt - yhat)*weekday)))
    workingday_new <- workingday - learningrate*((1/m)*(sum((hoursoutput$cnt - yhat)*workingday))) 
    weathersit_new <- weathersit - learningrate*((1/m)*(sum((hoursoutput$cnt - yhat)*weathersit)))
    temp_new <- temp - learningrate*((1/m)*(sum((hoursoutput$cnt - yhat)*temp)))
    atemp_new <- atemp - learningrate*((1/m)*(sum((hoursoutput$cnt - yhat)*atemp)))
    hum_new <- hum - learningrate*((1/m)*(sum((hoursoutput$cnt - yhat)*hum)))
    windspeed_new <- windspeed - learningrate*((1/m)*(sum((hoursoutput$cnt - yhat)*windspeed)))
    constant_new <- constant - learningrate*((1/m)*(sum(hoursoutput$cnt - yhat)))
    
    season <- season_new
    mnth <- mnth_new
    hr <- hr_new
    holiday <- holiday_new
    weekday <- weekday_new
    workingday <- workingday_new
    weathersit <- weathersit_new
    temp <- temp_new
    atemp <- atemp_new
    hum <- hum_new
    windspeed <- windspeed_new
    constant <- constant_new
      
    yhat <- constant + season*hoursinput$season + mnth*hoursinput$mnth + hr*hoursinput$hr + holiday*hoursinput$holiday + weekday*hoursinput$weekday + workingday*hoursinput$workingday + weathersit*hoursinput$weathersit + temp*hoursinput$temp + atemp*hoursinput$atemp + hum*hoursinput$hum + windspeed*hoursinput$windspeed
    MSE_new <- sum(((hoursoutput$cnt - yhat)^2)/(2*m))
    
    if(MSE - MSE_new <= conversionthreshold)
    {
      #abline(constant,season,mnth,hr,holiday,weekday,workingday,weathersit,temp,atemp,hum,windspeed)
      converged <- T
      return(c(constant,season,mnth,hr,holiday,weekday,workingday,weathersit,temp,atemp,hum,windspeed))
    }
      iterations = iterations + 1
      
      if(iterations > maxiterations)
      {
        #abline(constant,season,mnth,hr,holiday,weekday,workingday,weathersit,temp,atemp,hum,windspeed)
        converged <- T
        return(c(constant,season,mnth,hr,holiday,weekday,workingday,weathersit,temp,atemp,hum,windspeed))
      }
    }                                       
}

model <-gradientdescent(hoursinput = hoursinput,hoursoutput = hoursoutput,learningrate = 0.01,conversionthreshold = 0.001,maxiterations = 100)
  
model  




  
    