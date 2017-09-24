pls <- function(n)
{
  X1 <- NULL
  X2 <- NULL
  X3 <- NULL
  X4 <- NULL
  X5 <- NULL
  X6 <- NULL
  
  for (k in 1:n){
    t1 <- rbinom(1,1,0.3)
    X1 <- c(X1,t1)
    t2 <- rbinom(1,1,0.7)
    X2 <- c(X2,t2)
    
    if(t1 == 1 && t2 == 1){
      t3 <- rbinom(1,1,0.8)
      t4 <- rbinom(1,1,0.1)
      X3 <- c(X3,t3)
      X4 <- c(X4,t4)
    }
    if(t1 == 1 && t2 == 0){
      t3 <- rbinom(1,1,0.6)
      t4 <- rbinom(1,1,0.1)
      X3 <- c(X3,t3)
      X4 <- c(X4,t4)
    }
    if(t1 == 0 && t2 == 1){
      t3 <- rbinom(1,1,0.6)
      t4 <- rbinom(1,1,0.8)
      X3 <- c(X3,t3)
      X4 <- c(X4,t4)
    }
    if(t1 == 0 && t2 == 0){
      t3 <- rbinom(1,1,0.1)
      t4 <- rbinom(1,1,0.8)
      X3 <- c(X3,t3)
      X4 <- c(X4,t4)
    }
    if(t2 == 1){
      t5 <- rbinom(1,1,0.1)
      X5 <- c(X5,t5)
    }
    if(t2 == 0){
      t5 <- rbinom(1,1,0.5)
      X5 <- c(X5,t5)
    }
    if(t1 == 1 && t5 == 1){
      t6 <- rbinom(1,1,0.9)
      X6 <- c(X6,t6)
    }
    if(t1 == 1 && t5 == 0){
      t6 <- rbinom(1,1,0.5)
      X6 <- c(X6,t6)
    }
    if(t1 == 0 && t5 == 1){
      t6 <- rbinom(1,1,0.5)
      X6 <- c(X6,t6)
    }
    if(t1 == 0 && t5 == 0){
      t6 <- rbinom(1,1,0.1)
      X6 <- c(X6,t6)
    }
  }
    Data <- data.frame(X1,X2,X3,X4,X5,X6)
    return(Data)
}
set.seed(1)
library(bnlearn)
Data <- pls(200)
Data


