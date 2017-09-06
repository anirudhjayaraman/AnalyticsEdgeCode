setwd("~/Work In Progress/Analytics Edge/Unit 02")
library(readr)
# load the dataset
climate_change <- read_csv("~/Work In Progress/Analytics Edge/Unit 02/climate_change.csv")
names(climate_change)[7:8] <- c('CFC.11','CFC.12')
# check the data types of the dataset
as.character(sapply(climate_change, class))
train <- subset(climate_change, Year <= 2006)
test <- subset(climate_change, Year > 2006)
model_01 <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols,
               data = train)
summary(model_01)
# Which of the following independent variables is N2O highly correlated with 
# (absolute correlation greater than 0.7)?
names(cor(train[,-1])[5,][which(cor(train[,-1])[5,] > 0.7)])
# Which of the independent variables is CFC.11 highly correlated with? 
names(cor(train[,-1])[6,][which(cor(train[,-1])[6,] > 0.7)])
# Simplifying the Model
model_02 <- lm(Temp ~ MEI + N2O + TSI + Aerosols,
               data = train)
summary(model_02)

# Automatically Building the Model
model_03 <- step(model_01)
summary(model_03)

# Using the model produced from the step function, calculate temperature 
# predictions for the testing data set, using the predict function.
# Also calculate test set R squared
predictions <- predict(model_03, test)
actualTemp <- test$Temp
SSE = sum((actualTemp - predictions)^2)
SST = sum((actualTemp - mean(train$Temp))^2)
R_sq <- 1 - SSE/SST
