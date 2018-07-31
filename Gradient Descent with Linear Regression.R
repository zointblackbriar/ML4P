#Gradient Descent with Linear Regression
#Choose a learning rate for gradient descent
if(!require(magrittr)) {
  install.packages("magrittr"); require(magrittr)
}
if(!require(dplyr)) {
  install.packages("dplyr"); require(dplyr)
}
if(!require(readxl)) {
  install.packages("readxl"); require(readxl)
}



# thicknessYaxis <- matrix(my_data["thickness"])
# spacingXaxis <- matrix(my_data["spacing"])

# least-squares cost function
cost <- function(X, y, theta) {
  # computes the cost of using theta as the parameter for linear regression
  # to fit the data points in X and y
  sum((X %*% theta - y)^2)/(2 * length(y))
}


delta <- function(x, y, theta) {
  error <- (x %*% theta - y)
  delta <- t(x) %*% error/length(y)
  return((delta))
}

# gradient descent update algorithm
gradescent <- function(x, y, theta, alpha) {
  #theta = gradescent(X, y, alpha)
  theta <- theta - (alpha * delta(as.matrix(x), as.matrix(y), theta))
  return(theta)
}

#predictor linear model set
currentDirectory <- getwd()

setwd("C:\\Users\\zoint\\Desktop\\AllFiles\\Projeler\\R_Projects")
directoryChanged <- getwd()

my_data <- read_excel("Autoform_1000.xlsx")

dependentValue <- my_data[,9]
print(dependentValue)

independentValues <- my_data[1:8]
# fix(dependentValue)
# fix(independentValues)

fitted_model <- lm( HARDNESSP1 ~ ., data=my_data[1:9]) # y=theta0+theta1*x+epsilon(error)
# print(fitted_model)
plot(independentValues,dependentValue,col=rgb(0.2,0.4,0.6,0.4), main='Linear regression by gradient descent')
abline(fitted_model, col='blue')

theta_lm<-coef(fitted_model); #least squares parameter estimates
print(theta_lm) #print the parameters
print(paste0("coefficient of regression: ", summary( fitted_model )$r.squared))   # R ^ 2 coefficient of regression
print(paste0("mean squared error: ",  mean(residuals( fitted_model )^2)))
#The mean squared prediction error measures the expected squared distance 
#between what your predicts for a specific value and what the true value is
#print(paste0("Mean squared prediction error: ", MSPE(lm, my_data, LL = TRUE)))

num_iters <- 100  
independentValues <- cbind(1, matrix(independentValues))  # column of 1 was added for intercept coefficient
cost_history <- double(num_iters)  # to keep the cost history
theta_history <- list(num_iters)
theta_grad <- matrix(c(0, 0), nrow = 2)  # Initialize the parameters
independentValues <- cbind(1, matrix(independentValues))

alpha = 0.05  # set learning rate

for (i in 1:num_iters) {
  theta_grad <- gradescent(independentValues, dependentValue, theta_grad, alpha)
  cost_history[i] <- cost(dependentValue, independentValues, theta_grad)
  theta_history[[i]] <- theta_grad
}

plot(HARDNESSP1 ~ independentValues)  #scatter plot of data (var and response)

plot(independentValues,dependentValue, col=rgb(0.2,0.4,0.6,0.4), main='Linear regression by gradient descent')
for (i in c(1,3,6,10,14,seq(20,num_iters,by=10))) {
  abline(coef=theta_history[[i]], col=rgb(0.8,0,0,0.3))
}
abline(coef=theta, col='blue')

plot(cost_history[1:100], type = "line", col = "blue", lwd = 2, main = "Cost function", 
     ylab = "cost", xlab = "Iterations")
