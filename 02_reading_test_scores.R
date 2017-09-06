library(readr)
train <- read_csv("~/Work In Progress/Analytics Edge/Unit 02/pisa2009train.csv")
test <- read_csv("~/Work In Progress/Analytics Edge/Unit 02/pisa2009test.csv")

# How many students are there in the training set?
nrow(train)

# what is the average reading test score of males?
tapply(train$readingScore, train$male, FUN = mean)
tapply(train$readingScore, train$male, FUN = mean)[[2]] # males
tapply(train$readingScore, train$male, FUN = mean)[[1]] # females

# Which variables are missing data in at least one observation in the training set?
names(which(sapply(train, function(x) sum(is.na(x)))>0))

# Removing missing values
train <- na.omit(train)           
test <- na.omit(test)
nrow(train)
nrow(test)

# Factor variables
as.character(sapply(train, class))
# [1] "integer"   "integer"   "character" "integer"   "integer"  
# [6] "integer"   "integer"   "integer"   "integer"   "integer"  
# [11] "integer"   "integer"   "integer"   "integer"   "integer"  
# [16] "integer"   "integer"   "integer"   "integer"   "integer"  
# [21] "integer"   "integer"   "integer"   "numeric"  
train <- as.data.frame(unclass(train))
test <- as.data.frame(unclass(test))
as.character(sapply(train,class))
# [1] "integer" "integer" "factor"  "integer" "integer" "integer"
# [7] "integer" "integer" "integer" "integer" "integer" "integer"
# [13] "integer" "integer" "integer" "integer" "integer" "integer"
# [19] "integer" "integer" "integer" "integer" "integer" "numeric"

levels(train$raceeth)
# [1] "American Indian/Alaska Native"         
# [2] "Asian"                                 
# [3] "Black"                                 
# [4] "Hispanic"                              
# [5] "More than one race"                    
# [6] "Native Hawaiian/Other Pacific Islander"
# [7] "White"
levels(relevel(train$raceeth, 'White'))
# [1] "White"                                 
# [2] "American Indian/Alaska Native"         
# [3] "Asian"                                 
# [4] "Black"                                 
# [5] "Hispanic"                              
# [6] "More than one race"                    
# [7] "Native Hawaiian/Other Pacific Islander"

# Set the reference level of the factor as the most common level
train$raceeth <- relevel(train$raceeth, 'White')

# Building a model
lmScore <- lm(readingScore ~ ., data = train)
summary(lmScore)


predict_train <- predict(lmScore, train)

# Training set Rsq
SSE <- sum((train$readingScore - predict_train)^2)
SST <- sum((train$readingScore - mean(train$readingScore))^2)
RsqTrain <- 1 - SSE/SST

# Training set RMSE
RMSE_Train <- ((1/nrow(train))*SSE)^0.5

# Predicting test set reading scores
predict_test <- predict(lmScore, test)
# What is the range between the maximum and minimum predicted 
# reading score on the test set?
max(predict_test) - min(predict_test)

# What is the sum of squared errors (SSE) of lmScore on the testing set?
SSE_Test <- sum((predict_test - test$readingScore)^2)
RMSE_Test <- ((1/nrow(test))*SSE_Test)^0.5

# Baseline prediction and test-set SSE
# Remember to compute this value using the training set and not the test set.
mean(train$readingScore)
SST_Test <- sum((test$readingScore - mean(train$readingScore))^2)
RsqTest <- 1- SSE_Test / SST_Test