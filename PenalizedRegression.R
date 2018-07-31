# Test penalties of regression
# High Correlation between predictors

if(!require(glmnet)) {
  install.packages("glmnet"); require(glmnet)
}
if(!require(MASS)) {
  install.packages("MASS"); require(MASS)
}



# Generate Data
set.seed(20000)
dependentValue.train <- my_data$HARDNESSP1
independentValues.train <- my_data  %>% data.matrix()
CovMatrix <- outer(1:dependentValue.train, 1:dependentValue.train, function(x, y) {.7^abs(x-y)})
x <- mvrnorm (independentValues.train, rep(0,dependentValue.train), CovMatrix)
y <- 10 * apply(x[, 1:2], 1, sum) +
  5 * apply(x[, 3:4], 1, sum) + 
  apply(x[, 5:14], 1, sum) +
  rnorm(independentValues.train)

# Split data into train and test sets
# train_rows <- sample(1:n, 0.66*n)
# x.train <- x[train_rows, ]
# x.test <- x[-train_rows, ]
# 
# y.train <- y[train_rows]
# y.test <- y[-train_rows]
# 
# fit.lasso <- glmnet(x.train, y.train, family="gaussian", alpha=1)
# fit.ridge <- glmnet(x.train, y.train, family="gaussian", alpha=0)
# fit.elastic <- glmnet(x.train, y.train, family="gaussian", alpha=0.5)
# 
# # 10 fold cross validation for each alpha
# # MSE calculation
# fit.lasso.cv <- cv.glmnet(x.train, y.train, type.measure = "mse", alpha = 1, family="gaussian")
# fit.ridge.cv <- cv.glmnet(x.train, y.train, type.measure = "mse", alpha = 0, family="gaussian")
# fit.elastic.cv <- cv.glmnet(x.train, y.train, type.measure = "mse", alpha = 0.5, family="gaussian")
# 
# for(i in 0:10)
# {
#   assign(paste("fit", i, sep=""), cv.glmnet(x.train, y.train, type.measure = "mse", alpha=i/10, family="gaussian"))
# }
# # graphics.off()
# # par(mfrow = c(3, 2))
# # plot(fit.lasso, xvar="lambda")
# # plot(fit10, main="LASSO")
# # 
# # plot(fit.ridge, xvar="lambda")
# # plot(fit0, main="Ridge")
# # 
# # plot(fit.elnet, xvar="lambda")
# # plot(fit5, main="Elastic Net")
# 
# yhat0 <- predict(fit0, s=fit0$lambda.1se, newx=x.test)
# yhat1 <- predict(fit1, s=fit1$lambda.1se, newx=x.test)
# yhat2 <- predict(fit2, s=fit2$lambda.1se, newx=x.test)
# yhat3 <- predict(fit3, s=fit3$lambda.1se, newx=x.test)
# yhat4 <- predict(fit4, s=fit4$lambda.1se, newx=x.test)
# yhat5 <- predict(fit5, s=fit5$lambda.1se, newx=x.test)
# yhat6 <- predict(fit6, s=fit6$lambda.1se, newx=x.test)
# yhat7 <- predict(fit7, s=fit7$lambda.1se, newx=x.test)
# yhat8 <- predict(fit8, s=fit8$lambda.1se, newx=x.test)
# yhat9 <- predict(fit9, s=fit9$lambda.1se, newx=x.test)
# yhat10 <- predict(fit10, s=fit10$lambda.1se, newx=x.test)
# 
# print(mse0 <- mean((y.test - yhat0)^2))
# print(mse1 <- mean((y.test - yhat1)^2))
# print(mse2 <- mean((y.test - yhat2)^2))
# print(mse3 <- mean((y.test - yhat3)^2))
# print(mse4 <- mean((y.test - yhat4)^2))
# print(mse5 <- mean((y.test - yhat5)^2))
# print(mse6 <- mean((y.test - yhat6)^2))
# print(mse7 <- mean((y.test - yhat7)^2))
# print(mse8 <- mean((y.test - yhat8)^2))
# print(mse9 <- mean((y.test - yhat9)^2))
# print(mse10 <- mean((y.test - yhat10)^2))
# 
# sst <- sum((y.test - mean(y.test))^2)
# sse <- sum((yhat3 - y.test)^2)
# 
# rsq <- 1 - sse / sst
# print(paste0("R squared error: ", rsq))


