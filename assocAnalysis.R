# Example for Association Analysis

library(arules)

# Read in the file
data<-read.csv(file="car.csv")

# Min the associations
rules <- apriori(data,parameter = list(supp = 0.004, conf = 0.50, target = "rules"))
rules <- subset(rules,subset = lift>2.0 )

#rules <- subset(rules,subset = confidence >0.9)
srules<-subset(rules,subset = rhs %pin% c("Rating = good"))

# Look at the rules
inspect(srules)