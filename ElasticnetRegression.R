#Elastic Net Regression
#Hybrid Model

# ElasticRegression <- function(linearRegressionUse = TRUE, rigdeRegressionUse=FALSE)
# {
#   
#   objectDescriptor <- list(hasLinearRegression = linearRegressionUse,
#                            hasRidgeRegression = rigdeRegressionUse, 
#                            findXSquare <- function(input)
#                            {
#                              return (input * input) 
#                            }
#   )
#   
#   class(objectDescriptor) <- append(class, "RegressionMethod")
#   return (objectDescriptor)
#   
#   
# }
# 
# objectElasticReg <- ElasticRegression()
# print(objectElasticReg+findXSquare(12))

if(!require(readxl)) {
  install.packages("readxl"); require(readxl)
}
if(!require(corrplot)) {
  install.packages("corrplot"); require(corrplot)
}

if(!require(glmnet)) {
  install.packages("glmnet"); require(glmnet)
}

if(!require(stats)) {
  install.packages("stats"); require(stats)
}

if(!require(limSolve)) {
  install.packages("limSolve"); require(limSolve)
}

if(!require(rapportools)) {
  install.packages("rapportools"); require(rapportools)
}

if(!require(caret)) {
  install.packages("caret"); require(caret)
}

if(!require(magrittr)) {
  install.packages("magrittr"); require(magrittr)
}

if(!require(dplyr)) {
  install.packages("dplyr"); require(dplyr)
}




#predictor linear model set
currentDirectory <- getwd()


setwd("C:\\Users\\zoint\\Desktop\\AllFiles\\Projeler\\R_Projects")
directoryChanged <- getwd()

my_data <- read_excel("Autoform_1000.xlsx")
fix(my_data)
correlationValue <- cor(my_data[1:9])
corrplot.mixed(correlationValue)

dependentValue.train <- my_data[1:1000, 9]
independentValues.train <- my_data[1:1000, 1:8]

# print(length(dependentValue.train))
# print(length(independentValues.train))
# fit <- glmnet(dependentValue.train, independentValues.train, family="gaussian", alpha=.3)
#fit model
# fit <- glmnet(dependentValue, independentValues, family="gaussian", alpha = 0.3, lambda=0.0001)

# lm_all_P1 <- lm(HARDNESSP1 ~ ., data = my_data[,1:9])
dependentValue.train <- my_data$HARDNESSP1
independentValues.train <- my_data %>% select(`sim#`, thickness, TransportTimeAfterHeating, EnforcedTemperatureOfEntireSheet, QuenchingTimeInTool, QuenchingForce, spacing, DefaultToolTemperature) %>% data.matrix()
lambdas <- 10 ^ seq(7, -2, by = -.1)
fit <- cv.glmnet(independentValues.train, dependentValue.train, nfolds = 5, alpha=.3, type.measure = "mse", lambda = lambdas, family="gaussian")
plot(fit)

best_lambda = fit$lambda.min
en_coeff = predict(fit,s = best_lambda,type = "coefficients")
print(en_coeff)

y_predicted = predict(fit,s = best_lambda,newx = independentValues.train)

sst <- sum((dependentValue.train - mean(dependentValue.train))^2)
sse <- sum((y_predicted - dependentValue.train)^2)

#R squared
rsq <- 1 - sse / sst
print(paste0("R squared error", rsq))

# predict_P1_Hardness = predict(lm_all_P1, fit)
