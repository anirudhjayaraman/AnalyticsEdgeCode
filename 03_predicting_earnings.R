library(readr)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(caTools)
library(randomForest)

census <- read.csv('census.csv')
set.seed(2000)
split <- sample.split(census$over50k, SplitRatio = 0.6)
censusTrain <- subset(census, split == T)
censusTest <- subset(census, split == F)
modelLog01 <- glm(over50k ~ ., data = censusTrain, family = 'binomial')
# Which variables are significant, or have factors that are significant? 
summary(modelLog01)

# What is the accuracy of the model on the testing set? Use a threshold of 0.5. 
predsmodelLog01 <- predict(modelLog01, censusTest)
table(censusTest$over50k, predsmodelLog01 >= 0.5)
(9351+1515)/nrow(censusTest)

# What is the baseline accuracy for the testing set?
(9351+362)/nrow(censusTest)

# What is the area-under-the-curve (AUC) for this model on the test set?
ROCRpredsLog01 <- prediction(predsmodelLog01, censusTest$over50k)
ROCRperfLog01 <- performance(ROCRpredsLog01,'tpr','fpr')
plot(ROCRperfLog01)
AUClog01 <- as.numeric(performance(ROCRpredsLog01, 'auc')@y.values)
AUClog01

# A CART Model
# Use the default parameters, so don't set a value for minbucket or cp.

modelCART1 <- rpart(over50k ~ ., data = censusTrain, method = 'class')
prp(modelCART1)

# What is the accuracy of the model on the testing set?
predCART1 <- predict(modelCART1, newdata = censusTest)
table(censusTest$over50k, predCART1[,2] >= 0.5)
(9243+1596)/nrow(censusTest)

# Plot the ROC curve for the CART model you have estimated. 
ROCRpredsCART1 <- prediction(predCART1[,2], censusTest$over50k)
ROCRperfCART1 <- performance(ROCRpredsCART1, 'tpr','fpr')
plot(ROCRperfCART1)

# What is the AUC of the CART model on the test set?
AUCcart1 <- as.numeric(performance(ROCRpredsCART1, 'auc')@y.values)
AUCcart1

# A Random Forest Model
set.seed(1000)
trainSmall <- censusTrain[sample(nrow(censusTrain), 2000), ]
set.seed(1000)
modelRF <- randomForest(over50k ~ ., data = trainSmall)
predsRF <- predict(modelRF, censusTest)

# What is the accuracy of the model on the test set, using a threshold of 0.5? 
table(censusTest$over50k, predsRF)
(9643+851)/nrow(censusTest)

# Which of the variables is the most important in terms of the number of splits?
vu <- varUsed(modelRF, count=TRUE)
vusorted <- sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(modelRF$forest$xlevels[vusorted$ix]))

# most important in terms of mean reduction in impurity
varImpPlot(modelRF)

# Selecting cp by Cross-Validation
numFolds = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
train(over50k ~ ., data = censusTrain,
      method = "rpart", trControl = numFolds, tuneGrid = cartGrid)
# The final value used for the model was cp = 0.002.

# Fit a CART model to the training data using this value of cp. 
# What is the prediction accuracy on the test set?
modelCART2 <- rpart(over50k ~ ., data = censusTrain, method = 'class', cp = 0.002)
CART2preds <- predict(modelCART2, newdata = censusTest)
table(censusTest$over50k, CART2preds[,2] >= 0.5)
(9178+1838)/nrow(censusTest)
prp(modelCART2)


prp(rpart(over50k ~ ., data = censusTrain, method = 'class', cp = 0.002))
prp(rpart(over50k ~ ., data = censusTrain, method = 'class', cp = 0.004))
prp(rpart(over50k ~ ., data = censusTrain, method = 'class', cp = 0.020))
prp(rpart(over50k ~ ., data = censusTrain, method = 'class', cp = 0.040))
prp(rpart(over50k ~ ., data = censusTrain, method = 'class', cp = 0.100))
