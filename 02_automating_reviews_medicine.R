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

trials <- read.csv('clinical_trial.csv') 

# How many characters are there in the longest abstract? 
chars <- as.numeric(sapply(as.character(trials$abstract), nchar))
chars[which.max(chars)] # 3708

# How many search results provided no abstract? 
length(which(chars == 0))

# Find the observation with the minimum number of characters in the title 
title_chars <- as.numeric(sapply(as.character(trials$title), nchar))
as.character(trials$title[which.min(title_chars)])

# Preparing the Corpus
corpusTitle <- VCorpus(VectorSource(trials$title))
corpusAbstract <- VCorpus(VectorSource(trials$abstract))

# Convert corpusTitle and corpusAbstract to lowercase.
corpusTitle <- tm_map(corpusTitle, content_transformer(tolower))
corpusAbstract <- tm_map(corpusAbstract, content_transformer(tolower))

# Remove the punctuation in corpusTitle and corpusAbstract.
corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)

# Remove the English language stop words from corpusTitle and corpusAbstract.
corpusTitle <- tm_map(corpusTitle, removeWords, stopwords('English'))
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords('English'))

# Stem the words in corpusTitle and corpusAbstract 
corpusTitle <- tm_map(corpusTitle, stemDocument)
corpusAbstract <- tm_map(corpusAbstract, stemDocument)

# Build a document term matrix
dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmAbstract <- DocumentTermMatrix(corpusAbstract)

# Limit dtmTitle and dtmAbstract to terms with sparseness of at most 95%
dtmTitle <- removeSparseTerms(dtmTitle, 0.95)
dtmAbstract <- removeSparseTerms(dtmAbstract, 0.95)

# Convert dtmTitle and dtmAbstract to data frames 
dtmTitle <- as.data.frame(as.matrix(dtmTitle))
dtmAbstract <- as.data.frame(as.matrix(dtmAbstract))

colnames(dtmTitle) <- make.names(colnames(dtmTitle))
colnames(dtmAbstract) <- make.names(colnames(dtmAbstract))

# How many terms remain in dtmTitle after removing sparse terms 
# (aka how many columns does it have)?
length(names(dtmTitle))

# How many terms remain in dtmAbstract?
length(names(dtmAbstract))

# What is the most frequent word stem across all the abstracts? 
wordCountAbstract <- as.numeric(sapply(dtmAbstract, sum))
names(dtmAbstract)[which.max(wordCountAbstract)]

# We want to combine dtmTitle and dtmAbstract into a single data frame to make 
# predictions. However, some of the variables in these data frames have the 
# same names.

colnames(dtmTitle) <- paste0('T',colnames(dtmTitle))
colnames(dtmAbstract) <- paste0('A',colnames(dtmAbstract))

# Now merge the 2 datasets in to a single dataset dtm
dtm <- cbind(dtmTitle, dtmAbstract)
dtm$trial <- trials$trial

# How many columns are in this combined data frame?
ncol(dtm)

# Splitting the dataset
set.seed(144)
split <- sample.split(dtm$trial, SplitRatio = 0.7)
dtmTrain <- subset(dtm, split == T)
dtmTest <- subset(dtm, split == F)

# What is the accuracy of the baseline model on the training set? 
table(dtmTrain$trial) # baseline model predicts trial == 0
730/nrow(dtmTrain)
# 0.5606759

# Build a CART model called trialCART, using all the independent variables
trialCART <- rpart(trial ~ ., data = dtmTrain, method = 'class')
prp(trialCART)

# Obtain the training set predictions for the model
# What is the maximum predicted probability for any result?
trialCARTpreds <- predict(trialCART, newdata = dtmTrain, type = 'prob')
max(trialCARTpreds[,2])

# Without running the analysis, how do you expect the maximum predicted 
# probability to differ in the testing set?
max(predict(trialCART, newdata = dtmTest, type = 'prob')[,2])
# Because the CART tree assigns the same predicted probability to each leaf node

# What is the training set accuracy of the CART model?
table(dtmTrain$trial, trialCARTpreds[,2] >= 0.5)
(631+441)/nrow(dtmTrain) # 0.8233487

# What is the training set sensitivity of the CART model?
441/(441+131) # 0.770979

# What is the training set specificity of the CART model?
631/(631+99) # 0.8643836

# What is the testing set accuracy, assuming a probability threshold of 0.5 
# for predicting that a result is a clinical trial?
trialCARTpredsTest <- predict(trialCART, newdata = dtmTest, type = 'prob')
table(dtmTest$trial, trialCARTpredsTest[,2] >= 0.5)
(261+162)/nrow(dtmTest)

# what is the testing set AUC of the prediction model?
ROCRpredsCART <- prediction(trialCARTpredsTest[,2],dtmTest$trial)
AUCCARTTest <- as.numeric(performance(ROCRpredsCART, 'auc')@y.values)
AUCCARTTest
