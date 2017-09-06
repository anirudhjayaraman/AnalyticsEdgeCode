library(tm)
library(SnowballC)
library(randomForest)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(caTools)
library(ROCR)

setwd("~/Work In Progress/Analytics Edge/Unit 05")
emails <- read.csv('emails.csv',stringsAsFactors = F)

# How many of the emails are spam?
table(emails$spam) # 1368

# The nchar() function counts the number of characters in a piece of text. 
# How many characters are in the longest email in the dataset 
# (where longest is measured in terms of the maximum number of characters)?
chars <- as.numeric(sapply(emails$text, nchar))
chars[which.max(chars)] # 43952

# Which row contains the shortest email in the dataset? 
which.min(chars) # 1992

######################### Building the corpus #########################
# Follow the standard steps to build and pre-process the corpus:
#
# 1) Build a new corpus variable called corpus.
corpus <- VCorpus(VectorSource(emails$text))
# 2) Using tm_map, convert the text to lowercase.
corpus <- tm_map(corpus, content_transformer(tolower))
# 3) Using tm_map, remove all punctuation from the corpus.
corpus <-  tm_map(corpus, removePunctuation)
# 4) Using tm_map, remove all English stopwords from the corpus.
corpus <- tm_map(corpus, removeWords, stopwords('english')) 
# 5) Using tm_map, stem the words in the corpus.
corpus <- tm_map(corpus, stemDocument) 
# 6) Build a document term matrix from the corpus, called dtm.
dtm <- DocumentTermMatrix(corpus)
dtm # the number of terms are 28687

# limit dtm to contain terms appearing in at least 5% of documents, 
# and store this result as spdtm 
spdtm <- removeSparseTerms(dtm, 0.95) 
spdtm # the number of terms now are 330

# Build a data frame called emailsSparse from spdtm, and use the 
# make.names function to make the variable names of emailsSparse valid.
emailsSparse <- as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) <- make.names(colnames(emailsSparse))

# What is the word stem that shows up most frequently 
# across all the emails in the dataset?
names(emailsSparse)[which.max(colSums(emailsSparse))]

emailsSparse$spam <- emails$spam

# How many word stems appear at least 5000 times in the ham emails in the dataset?
ham <- subset(emailsSparse, spam == 0)
wordCountHam <- colSums(ham[,-ncol(ham)])
which(wordCountHam >= 5000)
length(which(wordCountHam >= 5000))

# How many word stems appear at least 1000 times in the spam emails in the dataset?
spam <- subset(emailsSparse, spam == 1)
wordCountSpam <- colSums(spam[,-ncol(spam)])
length(which(wordCountSpam >= 1000))

# Building a Machine Learning Model

emailsSparse$spam <- as.factor(emailsSparse$spam)

set.seed(123)
split <- sample.split(emailsSparse$spam, SplitRatio = 0.7)
emailsTrain <- subset(emailsSparse, split == T)
emailsTest <- subset(emailsSparse, split == F)

# Logistic Regression Model
spamLog <- glm(spam ~ ., data = emailsTrain, family = 'binomial')
# CART Model
spamCART <- rpart(spam ~ ., data = emailsTrain, method = 'class')
# Random Forest Model
spamRF <- randomForest(spam ~ ., data = emailsTrain)

# For each model, obtain the predicted spam probabilities for the training set. 
spamLogpreds <- predict(spamLog, newdata = emailsTrain,type = 'response')
spamCARTpreds <- predict(spamCART, newdata = emailsTrain)
spamRFpreds <- predict(spamRF, newdata = emailsTrain)
spamRFpredsProbs <- predict(spamRF, newdata = emailsTrain, type = 'prob')
# How many of the training set predicted probabilities from 
# spamLogpreds are less than 0.00001?
length(which(spamLogpreds < 0.00001))

# How many of the training set predicted probabilities from 
# spamLogpreds are more than 0.99999?
length(which(spamLogpreds > 0.99999))

# How many of the training set predicted probabilities from 
# spamLogpreds are between 0.00001 and 0.99999?
length(which( 0.99999 > spamLogpreds & spamLogpreds > 0.00001))

# How many variables are labeled as significant (at the p=0.05 level) in the 
# logistic regression summary output?
summary(spamLog)
# we see that none of the variables are labeled as significant 
# (a symptom of the logistic regression algorithm not converging).

# How many of the word stems "enron", "hou", "vinc", and "kaminski" 
# appear in the CART tree? 
prp(spamCART) # 2

# What is the training set accuracy of spamLog, 
# using a threshold of 0.5 for predictions?
table(emailsTrain$spam, spamLogpreds >= 0.5)
(3052+954)/nrow(emailsTrain)

# What is the training set AUC of spamLog?
ROCRpredsLog <- prediction(spamLogpreds, emailsTrain$spam)
ROCRperfLog <- performance(ROCRpredsLog, 'tpr','fpr')
plot(ROCRperfLog)
AUCLog <- as.numeric(performance(ROCRpredsLog, 'auc')@y.values)
AUCLog

# What is the training set accuracy of spamCART, using a threshold of 0.5 
# for predictions?
table(emailsTrain$spam, spamCARTpreds[,2] >= 0.5)
(2885+894)/nrow(emailsTrain)

# What is the training set AUC of spamCART? 
ROCRpredsCART <- prediction(spamCARTpreds[,2], emailsTrain$spam)
ROCRperfCART <- performance(ROCRpredsCART, 'tpr', 'fpr')
plot(ROCRperfCART)
AUCCART <- as.numeric(performance(ROCRpredsCART, 'auc')@y.values)

# What is the training set AUC of spamRF?
sapply(c(is.vector, is.matrix, is.list, is.data.frame), do.call, list(spamRFpreds))
sapply(c(is.vector, is.matrix, is.list, is.data.frame), do.call, list(emailsTrain$spam))

ROCRpredsRF <- prediction(as.numeric(spamRFpreds), 
                          as.numeric(emailsTrain$spam))
ROCRperfRF <- performance(ROCRpredsRF, 'tpr','fpr')
plot(ROCRperfRF)
AUCRF <- as.numeric(performance(ROCRpredsRF, 'auc')@y.values)
AUCRF

# What is the training set accuracy of spamRF, using a threshold 
# of 0.5 for predictions? 
table(emailsTrain$spam, spamRFpreds)
(3046+958)/nrow(emailsTrain)

# Predictions on test set
spamLogpredsTest <- predict(spamLog, newdata = emailsTest,type = 'response')
spamCARTpredsTest <- predict(spamCART, newdata = emailsTest)
spamRFpredsProbsTest <- predict(spamRF, newdata = emailsTest, type = 'prob')

ROCRpredsLogTest <- prediction(spamLogpredsTest, emailsTest$spam)
ROCRpredsCARTTest <- prediction(spamCARTpredsTest[,2], emailsTest$spam)
ROCRpredsRFTest <- prediction(spamRFpredsProbsTest[,2], emailsTest$spam)
                           
# What is the testing set accuracy of spamLog, using a threshold of 0.5 
# for predictions?
table(emailsTest$spam, spamLogpredsTest > 0.5)
(1257+376)/nrow(emailsTest) # 0.9505239

# What is the testing set accuracy of spamCART, using a threshold of 0.5 
# for predictions?
table(emailsTest$spam, spamCARTpredsTest[,2] > 0.5)
(1228+386)/nrow(emailsTest) # 0.9394645

# What is the testing set accuracy of spamRF, using a threshold of 0.5 
# for predictions?
table(emailsTest$spam, spamRFpredsProbsTest[,2] > 0.5)
(1292+389)/nrow(emailsTest) # 0.9784633

# What is the testing set AUC of each model?
AUCLogTest <- as.numeric(performance(ROCRpredsLogTest, 'auc')@y.values)
AUCCARTTest <- as.numeric(performance(ROCRpredsCARTTest, 'auc')@y.values)
AUCRFTest<- as.numeric(performance(ROCRpredsRFTest, 'auc')@y.values)

AUCLogTest # 0.9627517
AUCCARTTest # 0.963176
AUCRFTest # 0.963176




