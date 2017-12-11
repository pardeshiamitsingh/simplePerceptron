
library(perceptron)

#install.packages("perceptron")

datasub <- read.csv("LogicalAnd.csv", stringsAsFactors=FALSE)
names(datasub) <- c("x1", "x2", "ytarget")
xtrain <- datasub[, c(1, 2)]
ytarget <- datasub[, 3]


weight <- c(-2, 3, 1)
#simplePerceptron('', datasub, xtrain, ytarget, weight, 0.4)
simplePerceptron(datasub, xtrain, ytarget, weight, 0.4)


