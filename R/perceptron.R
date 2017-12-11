library(lattice)

#Helper function to plots the classification line
displayWeights <- function(w, color='red') {
  # Plot vector w
  w <- as.numeric(w)  
  
  b = w[1]
  w1 = w[2]
  w2 = w[3]
  
  slope = -(w1/w2)
  intercept = -(b/w2)
  
  abline(a=intercept, b=slope, col=color)
}

displaySigmoid <- function(w, color='red') {
  plot(w, sigmoid(w), col='color')
}


#Helper activation function Z = W0 * X0 + W1 * X1
activationFunction <- function(weight, xtrain){
  return (sum(weight[2:length(weight)] * 
             as.numeric(xtrain)) + weight[1])
}

# step function to return classifier
setClassifierStepFunction <- function(z){
  # based on value of z value of yClassifier is set
  return(ifelse(z > 0, 1, 0))
}

# Relu function to return classifier
setClassifierReLuFunction <- function(z){
  # based on value of z value of yClassifier is set
  return(max(0,z))
}

# Sigmoid function to return classifier
setClassifierSigmoidFunction <- function(z){
  # based on value of z value of yClassifier is set
  return(1 / (1 + exp(-z)))
}

#' Plot the data subset graph and error graph of sample dataset
#'
#' Takes in any subset
#' @author Amitsingh Pardeshi
#'# @param func Pass in the function type (step, sigmoid, relu)
#' @param datasub random sample from training set
#' @param xtrain Feature vector
#' @param ytarget class label of sample
#' @param weight Initial weight vector
#' @param learningRate learning Rate
#' @return weight outputs weight vector and plots classifier line
#' @import lattice
#' @export 
#' 
simplePerceptron <- function(datasub, xtrain, ytarget, weight, learningRate) {
  #if func is null then set default value as step
  # if (!exists("func"))
  # {
  #   func = 'step'
  # }
  
  #assign names to the three columns of datasub for plotting points
  names(datasub) <- c("x1", "x2", "ytarget")
  
  # check for training set classification
  misclassfied <- TRUE
  
  #Repeat this until the entire training set is classified correctly.
  while(misclassfied) {
    
    misclassfied <- FALSE
    
    # loop through training data set
    for (i in 1:length(ytarget)) {
      
      z <- activationFunction(weight, xtrain[i, ])
      
      yClassifier <- setClassifierStepFunction(z)
      
      # if(is.na(func) || func == 'relu') {
      #   yClassifier <- setClassifierReLuFunction(z)
      # } else if(is.na(func) || func == 'sigmoid') {
      #   yClassifier <- setClassifierSigmoidFunction(z)
      # } else {
      #   yClassifier <- setClassifierStepFunction(z)
      # }
      
      # If yClassifier = yTarget do nothing, i.e there is no change in weight
      # Else calculate the weigth difference.
      
      weightdiff <- 0.4 * (ytarget[i] - yClassifier) * 
        c(1, as.numeric(xtrain[i, ]))
      
      # Update the weight vector
      weight <- weight + weightdiff
      
      if ((ytarget[i] - yClassifier) != 0.0) {
        misclassfied <- TRUE
      }
    }
  }
  
  # if(is.na(func) || func == 'sigmoid') {
  #   displaySigmoid(weight, color='blue4')
  # } else {
  
  #plotting the points for step function
    plot(x1 ~ x2, group=ytarget, data=datasub, pch=19,
         col = ifelse(ytarget > 0,'red','blue'),
         auto.key=list(space="top"))
    
    displayWeights(weight, color='blue4')
  #}
  return(weight)
}