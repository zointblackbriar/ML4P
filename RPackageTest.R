require(stats)
require(graphics)
#data(economics)
library(ggplot2)
library("readxl")
library(tidyverse) # for easy data manipulation and visualization
library(caret) # for easy machine learning
library(leaps) #for computing stepwise regression
library(MASS)
set.seed(500)

library(MASS)
data <- Boston



#my_data <- read_excel(file.choose())
# currentDirectory <- getwd()
# 
# setwd("C:\\Users\\zoint\\Desktop\\AllFiles\\Projeler\\R_Projects")
# directoryChanged <- getwd()
# 
# my_data <- read_excel("Autoform_1000.xlsx")
# print(my_data)


# Check that no data is missing
# apply(data,2,function(x) sum(is.na(x)))

# Train-test random splitting for linear model
index <- sample(1:nrow(data),round(0.75*nrow(data)))
print(nrow(data))
train <- data[index,]
test <- data[-index,]
print(test)


