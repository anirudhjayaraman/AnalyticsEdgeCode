setwd("~/Work In Progress/Analytics Edge/Unit 02")
library(readr)
train <- read_csv("~/Work In Progress/Analytics Edge/Unit 02/FluTrain.csv")
test <- read_csv("~/Work In Progress/Analytics Edge/Unit 02/Flutest.csv")

# which week corresponds to the highest percentage of ILI-related physician visits?
train$Week[which.max(train$ILI)] # [1] "2009-10-18 - 2009-10-24"

# Which week corresponds to the highest percentage of ILI-related query fraction?
train$Week[which.max(train$Queries)] # [1] "2009-10-18 - 2009-10-24"

# Understanding the Data
hist(train$ILI, breaks = 50) # Right Skew

plot(train$Queries,log(train$ILI), pch =  20, col = 'blue')

# Linear Regression Model
FluTrend1 <- lm(log(ILI) ~ Queries, data = train)
summary(FluTrend1)

# Test Set Performance
PredTest1 <- exp(predict(FluTrend1, newdata=test))

# What is our estimate for the percentage of ILI-related physician visits for 
# the week of March 11, 2012?
PredTest1[which(test$Week == '2012-03-11 - 2012-03-17')]

# What is the relative error betweeen the estimate (our prediction) 
# and the observed value for the week of March 11, 2012?
-(PredTest1[which(test$Week == '2012-03-11 - 2012-03-17')] - 
  test$ILI[which(test$Week == '2012-03-11 - 2012-03-17')]) / 
  test$ILI[which(test$Week == '2012-03-11 - 2012-03-17')]

# RMSE on the Test Set
RMSE_Test_1 <- ((1/nrow(test))*sum((PredTest1 - test$ILI)^2))^0.5

library(zoo)
ILILag2 <- lag(zoo(train$ILI), -2, na.pad=TRUE)
train$ILILag2 <- coredata(ILILag2)

plot(log(train$ILI), log(train$ILILag2), col = 'blue', pch = 20)

# New Regression Model
FluTrend2 <- lm(log(ILI) ~ log(ILILag2) + Queries, train)
summary(FluTrend2)

# Add lagged ILI variable to test set
test$ILILag2 <- coredata(lag(zoo(test$ILI),-2,na.pad = T))
# Fill missing values of lagged variable from data given in training set
test$ILILag2[2] <- train$ILI[nrow(train)]
test$ILILag2[1] <- train$ILI[nrow(train) - 1]

# What is the test-set RMSE of the FluTrend2 model?
RMSE_Test_2 <- ((1/nrow(test))*sum((exp(predict(FluTrend2,test)) - test$ILI)^2))^0.5
