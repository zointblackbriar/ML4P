#Lasso Regression (Least Absolute Shrinkage and Selection Operator)
#L1 Regularization
#Manhattan Distance
if(!require(readxl)) {
  install.packages("readxl"); require(readxl)
}

if(!require(dplyr)) {
  install.packages("dplyr"); require(dplyr)
}


if(!require(glmnet)) {
  install.packages("glmnet"); require(glmnet)
}

#predictor linear model set
currentDirectory <- getwd()

setwd("C:\\Users\\zoint\\Desktop\\AllFiles\\Projeler\\R_Projects")
directoryChanged <- getwd()

my_data <- read_excel("Autoform_1000.xlsx")
fix(my_data)
# print(summary(my_data))
lm_all_P1 <- lm(HARDNESSP1 ~ ., data = my_data[,1:9])
print(summary(lm_all_P1))

#Reduced Model "Hardness at P1"
lm_reduced_P1 <- update(lm_all_P1, .~. - sim. -thickness)
# print(summary(lm_reduced_P1))
P1_Hardness_coeffs=lm_reduced_P1$coefficients
print(P1_Hardness_coeffs)

#Sheet thickness P2
lm_all_P2 = lm(THICKNESSP2 ~ ., data = my_data[,1:13])

#Reduced model "Thickness at P2"
lm_reduced_P2 <- update(lm_all_P2, .~. - sim. - EnforcedTemperatureOfEntireSheet) #QuenchingTimeInTool
print(summary(lm_reduced_P2))
P2_PD_coeffs = lm_reduced_P2$coefficients



dependentValue <- my_data$HARDNESSP1
independentValues <- my_data %>% select(`sim#`, thickness, TransportTimeAfterHeating, EnforcedTemperatureOfEntireSheet, QuenchingTimeInTool, QuenchingForce, spacing, DefaultToolTemperature) %>% data.matrix()
#lasso <- glmnet (independentValues, dependentValue, family="gaussian", alpha=1)
#cross validation counterpart for glmnet function
cv_fit <- cv.glmnet(independentValues, dependentValue, family="gaussian", nfolds = 5, alpha = 1)
plot(cv_fit)

opt_lambda = cv_fit$lambda.min

y_predicted <- predict(cv_fit, s = opt_lambda, newx = independentValues)

#R squared error calculation
sst <- sum((dependentValue - mean(dependentValue))^2)
sse <- sum((y_predicted - dependentValue)^2)

#R squared
rsq <- 1 - sse / sst
print(paste0("R squared error", rsq))

#another test method
# lm_reduced_P1 = update(lm_all_P1, .~. -sim. - thickness)
# print(summary(lm_reduced_P1))
# P1_Hardness_coeffs = lm_reduced_P1$coefficients
# train = sample(1:999, 800)
# print(train)
# test = my_data[-train, 1:13]
# print(test)
# predict_P1_hardness = predict(lm_reduced_P2, test)
# print(summary(predict_P1_hardness))
# vergleich_P1=rbind(predict_P1_hardness,my_data[-train,9])
# # print(my_data[-train,9])
# 
# SSE <- sum((my_data[-train,9] - predict_P1_hardness) ^ 2)
# SST <- sum((my_data[-train,9] - mean(my_data[-train,9])) ^ 2)
# R_squared = 1 - SSE/SST
# print(paste0("R²=", R_squared))

