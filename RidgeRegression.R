#Rigde regression test
#L2 regularization
#Euclidian Distance
#If you are using regression without regularization, you have to be very special @OwenZhang

if(!require(glmnet)) {
  install.packages("glmnet"); require(glmnet)
}
if(!require(readxl)) {
  install.packages("readxl"); require(readxl)
}

if(!require(magrittr)) {
  install.packages("magrittr"); require(magrittr)
}

if(!require(dplyr)) {
  install.packages("dplyr"); require(dplyr)
}


currentDirectory <- getwd()

setwd("C:\\Users\\zoint\\Desktop\\AllFiles\\Projeler\\R_Projects")
directoryChanged <- getwd()

my_data <- read_excel("Autoform_1000.xlsx")
fix(my_data)

#Create scatterplot matrices
#They are a great way to roughly determine if you have a linear correlation between multiple variable
# pairs(my_data[1:8])
# pairs(my_data[1:15])
lm_all_P1 <- lm(HARDNESSP1 ~ ., data = my_data[,1:9]) 
# print(summary(lm_all_P1))
lm_all_P2 <- lm(THICKNESSP2 ~ ., data = my_data[, 1:14])
# print(paste0("summary of lm_all_P2", summary(lm_all_P2)))


dependentValue <- my_data$HARDNESSP1
independentValues <- my_data %>% select(`sim#`, thickness, TransportTimeAfterHeating, EnforcedTemperatureOfEntireSheet, QuenchingTimeInTool, QuenchingForce, spacing, DefaultToolTemperature) %>% data.matrix()
lambdas <- 10 ^ seq(7, -2, by = -.1)
# fit <- glmnet (x, y, alpha = 0, lambda = lambdas)
# print(summary(fit))
cv_fit <- cv.glmnet(independentValues, dependentValue, alpha = 0, lambda = lambdas)
plot(cv_fit)
opt_lambda <- cv_fit$lambda.min
print(opt_lambda)

y_predicted <- predict(cv_fit, s = opt_lambda, newx = independentValues)

# R squared error calculation
sst <- sum((dependentValue - mean(dependentValue))^2)
sse <- sum((y_predicted - dependentValue)^2)

#R squared
rsq <- 1 - sse / sst
print(paste0("R squared error", rsq))



