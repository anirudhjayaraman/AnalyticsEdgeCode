library(readr)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(caTools)

letters <- read.csv('letters_ABPR.csv')
letters$isB <- as.factor(letters$letter == "B")

set.seed(1000)
split <- sample.split(letters$isB, SplitRatio = 0.5)
lettersTrain <- subset(letters, split == T)
lettersTest <- subset(letters, split == F)

# Before building models, let's consider a baseline method that always predicts 
# the most frequent outcome, which is "not B". 
# What is the accuracy of this baseline method on the test set?

baseline_preds <- as.factor(rep(FALSE, nrow(lettersTest)))
levels(baseline_preds) <- c('FALSE', 'TRUE')

table(lettersTest$isB, baseline_preds)
1175 / nrow(lettersTest) # 0.754172


# CART MODEL
CARTb = rpart(isB ~ . - letter, data=lettersTrain, method="class")
# What is the accuracy of the CART model on the test set? 
CARTpreds <- predict(CARTb, newdata = lettersTest, type = 'class')
table(lettersTest$isB, CARTpreds)
(1118+340)/nrow(lettersTest)

# Random Forest Model
set.seed(1000)
RFmodel <- randomForest(isB ~ . - letter, data = lettersTrain)
predRF <- predict(RFmodel, newdata = lettersTest)
table(lettersTest$isB, predRF)
(1165+374)/nrow(lettersTest)

letters$letter <- as.factor( letters$letter )
set.seed(2000)
split <- sample.split(letters$letter, SplitRatio = 0.5)
lettersTrain <- subset(letters, split == T)
lettersTest <- subset(letters, split == F)

# In a multiclass classification problem, a simple baseline model is to 
# predict the most frequent class of all of the options.
summary(lettersTrain$letter) # predict P
summary(lettersTest$letter)
# What is the baseline accuracy on the testing set?
401/nrow(lettersTest)

# Predicting the letters A, B, P, R
CARTall <- rpart(letter ~ . - isB, data = lettersTrain, method = 'class')
prp(CARTall)
predsCARTall <- predict(CARTall, newdata = lettersTest, type = 'class')
# What is the test set accuracy of your CART model? 
table(lettersTest$letter, predsCARTall)
(348+318+363+340)/nrow(lettersTest)

set.seed(1000)
RFall <- randomForest(letter ~ . - isB, data = lettersTrain)
predsRFall <- predict(RFall, newdata = lettersTest)
table(lettersTest$letter, predsRFall)
(390+380+393+364)/nrow(lettersTest)
