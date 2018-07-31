#Ms. Stoll's code
rm(list = ls())

if(!require(stats)) {
  install.packages("stats"); require(stats)
}
if(!require(limSolve)) {
  install.packages("limSolve"); require(limSolve)
}

if(!require(rapportools)) {
  install.packages("rapportools"); require(rapportools)
}


files = list.files(pattern="csv")
print(files)
data_autoform = read.csv(files[1], sep=";")
fix(data_autoform)
# pairs(data_autoform[,2:7])
# pairs(data_autoform[,2:15])
print(summary(data_autoform))
lm_all_P1 = lm(HARDNESS.P1 ~ ., data=data_autoform[,1:8]) #Object 'HARDNESS.P1' not found because of the column numbers
print(summary(lm_all_P1))

lm_reduced_P1 <- update(lm_all_P1, .~. - sim - thickness)
print(summary(lm_reduced_P1))
P1_Hardness_coeffs = lm_reduced_P1$coefficients
lm_all_P2 = lm(THICKNESS.P2 ~., data = data_autoform[,1:13]) #Object 'THICKNESS.P2' not found because of the column numbers
print(summary(lm_all_P2))

lm_reduced_P2 <- update(lm_all_P2, .~. -sim. -EnforcedTemperatureOfEntireSheet) # QuenchingTimeinTool
print(summary(lm_reduced_P2))
P2_BD_coeffs = lm_reduced_P2$coefficients

#Testing linear models P1_Hardness
train = sample(1:999, 800)
test = data_autoform[-train, 1:8]# error object 'HARDNESS.P1' not found
predict_P1_Hardness = predict(lm_reduced_P1, test)
vergleich_P1 = rbind(predict_P1_Hardness, data_autoform[-train, 9])
 
SSE <- sum((data_autoform[-train, 9] - predict_P1_Hardness) ^ 2)
SST <- sum((data_autoform[-train, 9] - mean(data_autoform[-train, 9])) ^ 2)
R_squared = 1 - SSE/SST 
print(paste0("R squared =", R_squared))

#Testing linear models P2_BD
predict_P2_BD = predict(lm_reduced_P2, test)
vergleich_P1 = rbind(predict_P2_BD, data_autoform[-train, 13]) #there is naming error probably should be vergleich_P2

SSE <- sum((data_autoform[-train, 13] - predict_P2_BD) ^ 2)
SST <- sum((data_autoform[-train, 13] - mean(data_autoform[-train, 13])) ^ 2)
R_Squared = 1 - SSE / SST
print(paste0("R squared for P2_BD: ", R_Squared))

 