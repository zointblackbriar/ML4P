
if(!require(readxl)) {
  install.packages("readxl"); require(readxl)
}
if(!require(neuralnet)) {
  install.packages("neuralnet"); require(neuralnet)
}
if(!require(hydroGOF)) {
  install.packages("hydroGOF"); require(hydroGOF)
}


currentDirectory <- getwd()

setwd("C:\\Users\\zoint\\Desktop\\AllFiles\\Projeler\\R_Projects")
directoryChanged <- getwd()

my_data <- read_excel("Autoform_1000.xlsx")
# fix(my_data)

#Clear data from blank values
apply(my_data, 2, function(x) sum(is.na(x)))
# print(summary(my_data$HARDNESSP1))

#model linear regression
index = sample(1:nrow(my_data), round(0.9 * nrow(my_data)))
# test = my_data[1:400,1:9]
# train = my_data[401:999,1:9]
test <- my_data[-index, 1:9]
train <- my_data[index, 1:9]
# print(train)
lm.fit <- glm(HARDNESSP1~., data=train)
# print(summary(lm.fit))
lm.predict <- predict(lm.fit, test)
MSE.lm <- sum((lm.predict - test$HARDNESSP1)^2) /nrow(test)
# print(paste0("MSE.LM ", MSE.lm))
RMSE.LM=rmse(lm.predict,test$HARDNESSP1)
# print(paste0("RMSE.LM ", RMSE.LM))

maxs <- apply(my_data[1:9], 2, max)
mins <- apply(my_data[1:9], 2, min)

scaled <- as.data.frame(scale(my_data[1:9], center = mins, scale = maxs - mins))

# trainNN <- my_data[1:400,1:9]
# testNN <- my_data[401:999,1:9]
trainNN <- scaled[index, 1:9]
testNN <- scaled[-index, 1:9]
f <- as.formula(paste("HARDNESSP1 ~ ", paste("`sim#` + thickness + TransportTimeAfterHeating + EnforcedTemperatureOfEntireSheet + QuenchingTimeInTool + QuenchingForce + spacing + DefaultToolTemperature")))
#hidden neuran number = input size * 2/3
NN = neuralnet(f, data=trainNN, hidden = c(5) , linear.output = TRUE, threshold = 0.01)
plot(NN)
#Neural net fitting

temp_test <- subset(testNN, select=c("sim#", "thickness", "TransportTimeAfterHeating", "EnforcedTemperatureOfEntireSheet", "QuenchingTimeInTool", "QuenchingForce", "spacing", "DefaultToolTemperature"))
# print(temp_test)
predictedNN <- neuralnet::compute(NN, temp_test)

predictedNN_test <- predictedNN$net.result * (max(my_data[1:9]$HARDNESSP1) - min(my_data[1:9]$HARDNESSP1)) + min(my_data[1:9]$HARDNESSP1)
testPredict <- (testNN$HARDNESSP1) * (max(my_data[1:9]$HARDNESSP1) - min(my_data[1:9]$HARDNESSP1)) + min(my_data[1:9]$HARDNESSP1)
MSE.nn <- sum((testPredict - predictedNN_test)^2) / nrow(testNN)
print(paste(MSE.lm,MSE.nn))

# plot(test$HARDNESSP1,predictedNN_test,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
# abline(0,1,lwd=2)
# legend('bottomright',legend='NN',pch=18,col='red', bty='n')
# 
# plot(test$HARDNESSP1,lm.predict,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
# abline(0,1,lwd=2)
# legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

plot(test$HARDNESSP1,predictedNN_test,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$HARDNESSP1,lm.predict,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('Neural Network','Linear Model'),pch=18,col=c('red','blue'))

# # Calculate Root Mean Square Error (RMSE)
RMSE.NN = sqrt(mean(testPredict - predictedNN_test) ^ 2)
print(paste0 ("RMSE NN and LM ", paste(RMSE.LM,RMSE.NN)))

