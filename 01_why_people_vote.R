library(readr)
library(ROCR)
gerber <- read_csv('gerber.csv')

# What proportion of people in this dataset voted in this election?
mean(gerber$voting)

# Which of the four "treatment groups" had the largest percentage 
# of people who actually voted ?
sapply(gerber[,-2], function(x) mean(subset(gerber,x == 1)$voting)) 

# Exploration and Logistic Regression
VotingLog <- glm(voting ~ civicduty + hawthorne + self + neighbors,
                 data = gerber, family = 'binomial')
summary(VotingLog) # all variable seem to be significant
# Model predictions on the data
predsVotingLog <- predict(VotingLog,type = 'response')

# Using a threshold of 0.3, what is the accuracy of the logistic regression model?
table(gerber$voting, predsVotingLog >= 0.3)
(134513+51966)/nrow(gerber)

# Using a threshold of 0.5, what is the accuracy of the logistic regression model?
table(gerber$voting, predsVotingLog >= 0.5)
235388/nrow(gerber) # basically, this is same as baseline accuracy

# Model AUC
ROCRpredLOG <- prediction(predsVotingLog, gerber$voting)
perfLOG <- performance(ROCRpredLOG,'tpr','fpr')
plot(perfLOG)
AUCLog <- as.numeric(performance(ROCRpredLOG, 'auc')@y.values)
AUCLog

ROCRbaseline <- prediction(rep(0,nrow(gerber)), gerber$voting)
perfbaseline <- performance(ROCRbaseline, 'tpr','fpr')
plot(perfbaseline, col = 'blue')
AUCbaseline <- as.numeric(performance(ROCRbaseline,'auc')@y.values)
AUCbaseline

# CART Model
CARTmodel <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

# for the complete CART Model to be built
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, 
                   data=gerber, cp=0.0)
prp(CARTmodel2)

# Using only the CART tree plot, determine what fraction 
# (a number between 0 and 1) of "Civic Duty" people voted
# 0.31

# Make a new tree that includes the "sex" variable, again with cp = 0.0.
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, 
                   data=gerber, cp=0.0)
prp(CARTmodel3)

# In the control group, which gender is more likely to vote?
controlGroup <- subset(gerber, control == 1)
table(controlGroup$voting, controlGroup$sex)
# males voting
29015/(29015+66809) # 0.3027947
# females voting
27715 / (27715 + 67704) # 0.2904558

# In the "Civic Duty" group, which gender is more likely to vote?
civicDuty <- subset(gerber, civicduty == 1)
table(civicDuty$voting, civicDuty$sex)
# males voting
6165/(6165+12937) # 0.3227411
# females voting
5856/(5856+13260) # 0.3063402

# Create a regression tree using just the "control" variable, then create 
# another tree with the "control" and "sex" variables, both with cp=0.0.
CARTmodel4 <- rpart(voting ~ control, data = gerber, cp = 0.0)
CARTmodel5 <- rpart(voting ~ control + sex, data = gerber, cp = 0.0)
prp(CARTmodel4, digits = 6)
0.34 - 0.296638

# using the second tree (with control and sex), determine who is affected 
# more by NOT being in the control group
prp(CARTmodel5, digits = 7) # men and woment are affected about the same

votingLog2 <- glm(voting ~ sex + control, data = gerber, family = 'binomial')
summary(votingLog2)



Possibilities <-  data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
prds <- predict(votingLog2, newdata=Possibilities, type="response")
Possibilities$preds <- prds

# What is the absolute difference between the tree and the logistic 
# regression for the (Woman, Control) case? 
abs(0.2904558-0.2908065) # 0.0003507

# Yet another logit model with an Interaction term
votingLog3 <- glm(voting ~ sex + control + sex:control, 
                  data=gerber, family="binomial")
# How do you interpret the coefficient for the new variable in isolation?
summary(votingLog3)
# If a person is a woman and in the control group, the chance that she 
# voted goes down.
Possibilities$preds <- predict(votingLog3, newdata=Possibilities, type="response")


