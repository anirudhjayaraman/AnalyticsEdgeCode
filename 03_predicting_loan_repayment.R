library(readr)
loans <- read_csv('loans.csv')
# What proportion of the loans in the dataset were not paid in full? 
mean(loans$not.fully.paid)

# Which of the following variables has at least one missing observation?
sapply(loans, function(x) sum(is.na(x)))

# Preparing the Dataset
library(mice)
set.seed(144)
