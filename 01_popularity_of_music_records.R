library(readr)
songs <- read.csv("~/Work In Progress/Analytics Edge/Unit 03/songs.csv")

# How many observations (songs) are from the year 2010?
nrow(subset(songs, songs$year == 2010))

# How many songs does the dataset include for which the artist name is 
# "Michael Jackson"?
sum(songs$artistname == 'Michael Jackson')

# Which of the songs by Michael Jackson made it to the Top 10?
MJ <- subset(songs, artistname == 'Michael Jackson')
MJ$songtitle[as.logical(MJ$Top10)]

# What are the values of time signature that occur in our dataset?
unique(songs$timesignature)

# Which timesignature value is the most frequent among songs in our dataset?
hist(songs$timesignature)

# Out of all of the songs in our dataset, the song with the highest tempo is?
songs$songtitle[which.max(songs$tempo)]

# Creating Our Prediction Model
SongsTrain <- subset(songs, year <= 2009)
SongsTest <- subset(songs, year == 2010)

nonvars <- c("year", "songtitle", "artistname", "songID", "artistID")

SongsLog1 <- glm(Top10 ~ . - year - songtitle - artistname - songID - artistID,
                 data = SongsTrain, family = 'binomial' )
summary(SongsLog1)
# higher confidence leads to a higher predicted probability of a Top 10 hit.

# What is the correlation between the variables "loudness" and "energy" in the training set?
cor(SongsTrain$loudness, SongsTrain$energy) # 0.7399067
# Given that these two variables are highly correlated, 
# Model 1 suffers from multicollinearity.

SongsLog2 <- glm(Top10 ~ . - year - songtitle - artistname - songID - artistID - loudness,
                 data = SongsTrain, family = 'binomial')
summary(SongsLog2)


SongsLog3 <- glm(Top10 ~ . - year - songtitle - artistname - songID - artistID - energy,
                 data = SongsTrain, family = 'binomial')
summary(SongsLog3)

SongsTrain <- SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest <- SongsTest[ , !(names(SongsTest) %in% nonvars) ]

Model1 <- glm(Top10 ~ ., data = SongsTrain, family = 'binomial')
Model2 <- glm(Top10 ~ . - loudness, data = SongsTrain, family = 'binomial')
Model3 <- glm(Top10 ~ . - energy, data = SongsTrain, family = 'binomial')

# Make predictions on the test set using Model 3. 
# What is the accuracy of Model 3 on the test set, using a threshold of 0.45?
predicted3 <- predict(Model3, type = 'response', newdata = SongsTest)
table(SongsTest$Top10, predicted3 > 0.45)
328/(328+45)

# What would the accuracy of the baseline model be on the test set? 
313/(328+45)

# How many songs does Model 3 correctly predict as Top 10 hits in 2010?
# How many non-hit songs does Model 3 predict will be Top 10 hits?
table(SongsTest$Top10, predicted3 > 0.45) # 19; 5

# Sensitivity and Specificty
19/59; 309/314