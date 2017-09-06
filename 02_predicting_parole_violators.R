parole <- read.csv('parole.csv')

# How many of the parolees in the dataset violated the terms of their parole?
sum(parole$violator)

# Which variables in this dataset are unordered factors with at least three levels?
# state, crime
parole$state <- factor(parole$state)
parole$crime <- factor(parole$crime)

# Splitting into a Training and Testing Set
set.seed(144)
library(caTools)
split <- sample.split(parole$violator, SplitRatio = 0.7)
train <- subset(parole, split == TRUE)
test <- subset(parole, split == FALSE)

# Building a Logistic Regression Model
model1 <- glm(violator ~ ., data = train, family = 'binomial')
summary(model1)
# Our model predicts that a parolee who committed multiple offenses has 5.01 
# times higher odds of being a violator than a parolee who did not commit 
# multiple offenses but is otherwise identical.

# Consider a parolee who is male, of white race, aged 50 years at prison release,
# from the state of Maryland, served 3 months, had a maximum sentence 
# of 12 months, did not commit multiple offenses, and committed a larceny. 
dat <- data.frame(male = 1, race = 1, age = 50,
                  time.served = 3, max.sentence = 12,
                  state = factor(1), multiple.offenses = 0, crime = factor(2))

# According to the model, what is the probability this individual is a violator?
pr <- predict(model1, type = 'response',newdata = dat)

# According to the model, what are the odds this individual is a violator?
pr/(1-pr)

# obtain the model's predicted probabilities for parolees in the testing set
predTest <- predict(model1, type = 'response', newdata = test)

# What is the maximum predicted probability of a violation?
max(predTest)

# Evaluating the Model on the Testing Set
table(test$violator, predTest > 0.5)

# What is the model's sensitivity?
12/23

# What is the model's specificity?
167/179

# What is the model's accuracy?
179/(179+23)

# What is the accuracy of a simple model that predicts that every parolee 
# is a non-violator?
table(test$violator, predTest > 0.5)
table(test$violator, predTest > 0.7)
table(test$violator, predTest > 0.3)
# The board assigns more cost to a false negative than a false positive, and should therefore use a logistic regression cutoff less than 0.5. 

# Using the ROCR package, what is the AUC value for the model?
library(ROCR)
ROCRpred <- prediction(predTest, test$violator)
as.numeric(performance(ROCRpred, 'auc')@y.values)

ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)















