
# Load Data
#  The first two columns contains the exam scores and the third column
#  contains the label.
data2.table1 <- read.table("ex2data1.txt", sep=",")
names(data2.table1) <- c("score1", "score2", "label")


## ==================== Part 1: Plotting ====================
#  We start the exercise by first plotting the data to understand the 
#  the problem we are working with.
print("Plotting data with different colors")
library("ggplot2")
qplot(score1,score2, data=data2.table1, color=label, xlab="Exam 1 score", ylab="Exam 2 score", main="Scatter plot of training data")


## ============ Part 2: Compute Cost and Gradient ============
#  In this part implement the cost and gradient for logistic regression.

#  Add intercept term to x and X_test
X <- as.matrix(cbind(data2.table1[, c(1:2)], 1))
y <- as.matrix(data2.table1[,3])
m <- length(y)

#  sigmoid function
sigmoid <- function(x) {
  return(1/(1+exp(-x)))
}

#  Initialize fitting parameters
initial.theta <- as.matrix(rep.int(0, length(names(data2.table1))))

hypothesis <- function(x, theta) {
  return(sigmoid(x %*% theta))
}

Jcost <- function(m, theta, x, y) {
  i <- 1
  cost <- 0;
  while (i <= m) {
    cost <- cost + y[i] * log(hypothesis(X[i,], theta)) + (1 - y[i]) * log(1 - hypothesis(X[i,], theta))
    i <- i + 1;
  }
  cost <- (-1/m) * cost
  
  return(cost)
}

derivative <- function(j, x, y, m, theta) {
  i <- 1
  derivative.value <- 0
  while (i <= m) {
    derivative.value <- derivative.value + (hypothesis(x[i,], theta) - y[i]) * x[i, j]
    i <- i + 1
  }
  
  derivative.value <- (1 / m) * derivative.value
  
  return(derivative.value)
}

alpha <- 0.1
iterations <- 1500
cost.values <- rep(10000, iterations)
sprintf("Cost at initial theta (zeros): %f", cost.values[1] <- Jcost(m, initial.theta, X, y))
grand <- function(initial.theta, cost.values, x, y, m) {
  theta <- initial.theta
  theta.len <- length(theta)
  
  i <- 1
  while (i < iterations) {
    j <- 1
    new.theta <- theta
    while (j <= theta.len) {
      new.theta[j] <- new.theta[j] - alpha * derivative(j, x, y, m, theta)
      j <- j+1
    }
  
    new.cost <- Jcost(m, new.theta, x, y)
    if (new.cost < cost.values[i]) {
      initial.theta <- new.theta
    }
    
    i <- i + 1
    theta <- new.theta
    cost.values[i] <- new.cost
  }
  sprintf("initial.theta = %f", initial.theta)
  return(min(cost.values), initial.theta)
}
