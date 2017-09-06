setwd("~/Work In Progress/Analytics Edge/Unit 06")
stocks <- read.csv('StocksCluster.csv')

# What proportion of the observations have positive returns in December?
mean(stocks$PositiveDec) # 0.546114

# What is the maximum correlation between any two return variables in the dataset?
cors <- as.vector(cor(stocks))
max(abs(cors[cors!=1])) # 0.1916728

# Which month (from January through November) has the largest mean return 
# across all observations in the dataset?
which.max(colSums(stocks[-12])) # April

# Which month (from January through November) has the smallest mean return 
# across all observations in the dataset?
which.min(colSums(stocks[-12])) # September

# Initial Logistic Regression Model
library(caTools)
set.seed(144)
spl <- sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain <- subset(stocks, spl == T)
stocksTest <- subset(stocks, spl == F)

StocksModel <- glm(PositiveDec ~ ., data = stocksTrain, family = 'binomial')
predsLogTrain <- predict(StocksModel, type = 'response')

# What is the overall accuracy on the training set, using a threshold of 0.5?
table(stocksTrain$PositiveDec, predsLogTrain >= 0.5)
(990+3640)/nrow(stocksTrain) # 0.5711818

# What is the overall accuracy of the model on the test, 
# again using a threshold of 0.5?
predsLogTest <- predict(StocksModel, newdata = stocksTest, type = 'response')
table(stocksTest$PositiveDec, predsLogTest >= 0.5)
(417+1553)/nrow(stocksTest) # 0.5670697

# What is the accuracy on the test set of a baseline model?
(1553+344)/nrow(stocksTest) # 0.5460564

# Clustering Stocks
limitedTrain <- stocksTrain
limitedTrain$PositiveDec <- NULL
limitedTest <- stocksTest
limitedTest$PositiveDec <- NULL

library(caret)
preproc <- preProcess(limitedTrain)
normTrain <- predict(preproc, limitedTrain)
normTest <- predict(preproc, limitedTest)

mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)

set.seed(144)
km <- kmeans(as.vector(normTrain), centers = 3)

# Which cluster has the largest number of observations?
table(km$cluster)

library(flexclust)

km.kcca <- as.kcca(km, normTrain)

clusterTrain <- predict(km.kcca)

clusterTest <- predict(km.kcca, newdata=normTest)

# How many test-set observations were assigned to Cluster 2?
table(clusterTest)

# Cluster-Specific Predictions
for(i in 1:3){
  assign(paste0('stocksTrain',i),subset(stocksTrain, clusterTrain == i))
}

for(i in 1:3){
  assign(paste0('stocksTest',i),subset(stocksTest, clusterTest == i))
}

# Which training set data frame has the highest average value 
# of the dependent variable?
mean(stocksTrain1$PositiveDec) # 0.6024707
mean(stocksTrain2$PositiveDec) # 0.5140545
mean(stocksTrain3$PositiveDec) # 0.4387352

# Build Logistic Models
StocksModel1 <- glm(PositiveDec ~., data = stocksTrain1, family = 'binomial')
StocksModel2 <- glm(PositiveDec ~., data = stocksTrain2, family = 'binomial')
StocksModel3 <- glm(PositiveDec ~., data = stocksTrain3, family = 'binomial')

summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

PredictTest1 <- predict(StocksModel1, newdata = stocksTest1, type = 'response')
PredictTest2 <- predict(StocksModel2, newdata = stocksTest2, type = 'response')
PredictTest3 <- predict(StocksModel3, newdata = stocksTest3, type = 'response')

table(stocksTest1$PositiveDec, PredictTest1 >= 0.5)
(30+774)/nrow(stocksTest1) # 0.6194145

table(stocksTest2$PositiveDec, PredictTest2 >= 0.5)
(388+757)/nrow(stocksTest2) # 0.5504808

table(stocksTest3$PositiveDec, PredictTest3 >= 0.5)
(49+13)/nrow(stocksTest3) # 0.6458333


AllPredictions <- c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes <- c(stocksTest1$PositiveDec, 
                 stocksTest2$PositiveDec, 
                 stocksTest3$PositiveDec)

# What is the overall test-set accuracy of the cluster-then-predict approach, 
# again using a threshold of 0.5?
table(AllOutcomes, AllPredictions > 0.5)
(467+1544)/(467+1544+353+1110)







