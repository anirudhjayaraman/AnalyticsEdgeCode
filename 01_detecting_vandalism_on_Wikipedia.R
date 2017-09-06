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
wiki <- read.csv('wiki.csv', stringsAsFactors = FALSE)
names(wiki)
wiki$X.1 <- NULL
wiki$X <- NULL

# How many cases of vandalism were detected in the history of this page?
sum(wiki$Vandal)

# The text already is lowercase and stripped of punctuation. 
# So to pre-process the data, just complete the following four steps:

# 1) Create the corpus for the Added column, and call it "corpusAdded".
corpusAdded <- VCorpus(VectorSource(wiki$Added))

# 2) Remove the English-language stopwords.
corpusAdded <- tm_map(corpusAdded, removeWords, stopwords('English'))

# 3) Stem the words.
corpusAdded <- tm_map(corpusAdded, stemDocument)

# 4) Build the DocumentTermMatrix, and call it dtmAdded.
dtmAdded <- DocumentTermMatrix(corpusAdded)

# How many terms appear in dtmAdded?
dtmAdded # 6675 terms

# Filter out sparse terms by keeping only terms that appear in 0.3% or more 
# of the revisions, and call the new matrix sparseAdded. How many terms appear 
# in sparseAdded?
sparseAdded <- removeSparseTerms(dtmAdded, 0.997)
sparseAdded # 166 terms


# Create a data frame of this matrix
wordsAdded <- as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) <- paste('A', colnames(wordsAdded))


# Now repeat all of the steps we've done so far to create a Removed bag-of-words 
# dataframe, called wordsRemoved, except this time, prepend all of the words 
# with the letter R
sparseRemoved <- VCorpus(VectorSource(wiki$Removed))
sparseRemoved <- tm_map(sparseRemoved, removeWords, stopwords('English'))
sparseRemoved <- tm_map(sparseRemoved, stemDocument)
dtmRemoved <- DocumentTermMatrix(sparseRemoved)
dtmRemoved <- removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved <- as.data.frame(as.matrix(dtmRemoved))
colnames(wordsRemoved) <- paste('R', colnames(wordsRemoved))

# How many words are in the wordsRemoved data frame?
ncol(wordsRemoved)

# Combining the 2 data frames
wikiWords <- cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal <- wiki$Vandal

# Building an ML model
set.seed(123)
split <- sample.split(wikiWords$Vandal, SplitRatio = 0.7)
wikiWordsTrain <- subset(wikiWords, split == T)
wikiWordsTest <- subset(wikiWords, split == F)

# What is the accuracy on the test set of a baseline method 
# that always predicts "not vandalism"?
table(wikiWordsTest$Vandal)
618/nrow(wikiWordsTest) # 0.5313844

# Build a CART model to predict Vandal
modelCART <- rpart(Vandal ~ ., data = wikiWordsTrain, method = 'class')

# What is the accuracy of the model on the test set, using a threshold of 0.5? 
predsCART <- predict(modelCART, newdata = wikiWordsTest, type = 'prob')
table(wikiWordsTest$Vandal, predsCART[,2] > 0.5)
(618+12)/nrow(wikiWordsTest) # 0.5417025

# Plot the CART tree
prp(modelCART)

table(wikiWordsTrain$Vandal, predict(modelCART, wikiWordsTrain, type = 'prob')[,2] > 0.5)

# Problem-specific Knowledge
wikiWords2 <- wikiWords
wikiWords2$HTTP <- ifelse(grepl('http',wiki$Added),1,0)

# Based on this new column, how many revisions added a link?
sum(wikiWords2$HTTP)

# Split wikiWords2 into train and test sets
wikiTrain <- subset(wikiWords2, split == T)
wikiTest <- subset(wikiWords2, split == F)

# What is the new accuracy of the CART model on the test set, 
# using a threshold of 0.5?
modelCART2 <- rpart(Vandal ~ ., data = wikiTrain, method = 'class')
predsCART2 <- predict(modelCART2, newdata = wikiTest, type = 'prob')
table(wikiTest$Vandal, predsCART2[,2] >= 0.5)
(609 +57)/nrow(wikiTest) # 0.5726569

# Another possibility is that the number of words added and removed is 
# predictive, perhaps more so than the actual words themselves
wikiWords2$NumWordsAdded <- rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved <- rowSums(as.matrix(dtmRemoved))

# What is the average number of words added?
mean(wikiWords2$NumWordsAdded)

# What is the new accuracy of the CART model on the test set?
# Split wikiWords2 into train and test sets
wikiTrain2 <- subset(wikiWords2, split == T)
wikiTest2 <- subset(wikiWords2, split == F)

# What is the new accuracy of the CART model on the test set, 
# using a threshold of 0.5?
set.seed(123)
modelCART3 <- rpart(Vandal ~ ., data = wikiTrain2, method = 'class')
predsCART3 <- predict(modelCART3, newdata = wikiTest2, type = 'class')
table(wikiTest2$Vandal, predsCART3)
(330+401)/nrow(wikiTest2) # 0.6285469

wikiWords3 <- wikiWords2
wikiWords3$Minor <- wiki$Minor
wikiWords3$Loggedin <- wiki$Loggedin

wikiTrain3 <- subset(wikiWords3, split == T)
wikiTest3 <- subset(wikiWords3, split == F)

modelCART4 <- rpart(Vandal ~ ., data = wikiTrain3, method = 'class')
predsCART4 <- predict(modelCART4, newdata = wikiTest3)
table(wikiTest3$Vandal, predsCART4[,2] >= 0.5)
(596+239)/nrow(wikiTest3)
prp(modelCART4) # 0.7179708
