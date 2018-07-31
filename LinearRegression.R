#Linear Regression Assumptions
library("readxl")
library(ggplot2)
library(DataCombine)


#my_data <- read_excel(file.choose())
currentDirectory <- getwd()

setwd("C:\\Users\\zoint\\Desktop\\AllFiles\\Projeler\\R_Projects")
directoryChanged <- getwd()

my_data <- read_excel("Autoform_1000.xlsx")
#Anzeigen alle Werte
# print(my_data)
# print(my_data["thickness"])
# print(my_data["TransportTimeAfterHeating"])
# print(my_data["EnforcedTemperatureOfEntireSheet"])
# print(my_data["QuenchingTimeInTool"])
# print(my_data["QuenchingForce"])
# print(my_data["spacing"])
# print(my_data["DefaultToolTemperature"])

#mutmaßung von Werten
#The means of residuals is zero
meanResidual <- function(){
  mod <- lm(thickness ~ TransportTimeAfterHeating, data = my_data)
  mean (mod$residuals)
  
}


#Homoscedasticity of residuals  or equal variance
homosCedasticity <- function(){
  par(mfrow = c(2,2)) # set 2 rows and 2 column  plot layout
  mod_layout <- lm(thickness ~ TransportTimeAfterHeating, data = my_data)
  plot(mod_layout)
  
}

#zum kleinen Maßstab
benchmark <- function() {
  mod_kleiner <- lm(thickness ~ spacing, data = my_data[1:20, ])
  plot(mod_kleiner)
  
}

#No autocorrelation of residuals
autocorrelation <- function()
{
  lmMod <- lm(thickness ~ spacing, data = my_data)
  acf(lmMod$residuals)
  
}
#autocorrelated residuals
#View(lmtest::dwtest(lmMod))

#check all of our dataframe rows contain at least one missing value 
# So when you run regression wit lm() and na.action=na.omit all lines of data frame are removed and there are no data to fit regression.
# 
# But this is not the main problem. If your provided data contains all information you have, 
# then you are trying to apply regression with 165 
# independent variables (X variables) while having only 22 observations. 
# Number of independent variables have to be less than number of observations.

#View(apply(my_data,1,function(x) sum(is.na(x))))


#data cleaning for autocorrelation
# productionData <- data.frame(my_data, residualMod = lmMod$residuals)
# productionDataLag1 <- slide(productionData, Var="residualMod", NewVar = "lag1", slideBy=-1)
# productionDataLag2 <- na.omit(productionDataLag1)
# lmModVerSecond <- lm(thickness ~ spacing + lag1, data = productionDataLag2)
# 
# acf(lmModeVerSecond$residuals)

#The variability in X values is positive 
#This means the X values in a given sample must not be all the same (or even nearly the same)
# print(var(my_data$thickness))
# print(var(my_data$TransportTimeAfterHeating))
# print(var(my_data$EnforcedTemperatureOfEntireSheet))
# print(var(my_data$QuenchingTimeInTool))
# print(var(my_data$QuenchingForce))
# print(var(my_data$spacing))
# print(var(my_data$DefaultToolTemperature))
variabilityInX <- function(){
  if(var(my_data$thickness) > 0.0 && var(my_data$TransportTimeAfterHeating) > 0.0
     && var(my_data$EnforcedTemperatureOfEntireSheet) > 0.0 && var(my_data$QuenchingTimeInTool) > 0.0 &&
     var(my_data$QuenchingForce) > 0.0 && var(my_data$spacing) > 0.0 && var(my_data$DefaultToolTemperature) > 0.0)
  {
    View("Die Ergebnis ist ok")
  } else
  {
    View("Die Ergebnis ist nicht gut")
  }
  
}

#Check important assumptions automatically 
par(mfrow=c(2,2)) 
gvlmaMod <- lm(thickness ~ spacing, data = my_data[1:8, ])
gvlma::gvlma(gvlmaMod)
#plot(mod)
#View(influence.measures(gvlmaMod))

#The following assumptions has been assured
#Error terms are normally distributed
#There is constant variance when observing the error terms
#Observations of data are independently and identically distributed
#There is no multicolinearity across explanatory variables




