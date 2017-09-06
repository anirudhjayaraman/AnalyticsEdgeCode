setwd("~/Work In Progress/Analytics Edge/Unit 01")
library(readr)
CPS <- read_csv("~/Work In Progress/Analytics Edge/Unit 01/CPSData.csv")
View(CPS)

# How many interviewees are in the dataset?
nrow(CPS) # 131302

# Among the interviewees with a value reported for the Industry variable, 
# what is the most common industry of employment? 
table(CPS$Industry)
sort(table(CPS$Industry), decreasing = T)[1]
names(sort(table(CPS$Industry), decreasing = T)[1])
# Educational and health services

# Which state has the fewest interviewees?
names(sort(table(CPS$State)))[1] # New Mexico

# Which state has the largest number of interviewees?
names(sort(table(CPS$State), decreasing = T))[1] # California

# What proportion of interviewees are citizens of the United States?
CPS$Citizenship <- factor(CPS$Citizenship)
1 - summary(CPS$Citizenship)[3]/nrow(CPS) # 0.9421943

# For which races are there at least 250 interviewees in the CPS dataset 
# of Hispanic ethnicity?
table(CPS$Race, CPS$Hispanic) # American Indian, Black, Multiracial, White

# Which variables have at least one interviewee with a missing (NA) value?
apply(CPS,2,function(x) sum(is.na(x)))
names(which(apply(CPS,2,function(x) sum(is.na(x))) > 0))
# [1] "MetroAreaCode"    "Married"          "Education"       
# [4] "EmploymentStatus" "Industry"

# We will try to determine if there is a pattern in the missing values of the 
# Married variable. 
table(is.na(CPS$Married),CPS$Region)
table(is.na(CPS$Married),CPS$Sex)
table(is.na(CPS$Married),CPS$Age) # Missing for Age < 14
table(is.na(CPS$Married),CPS$Citizenship)

# How many states had all interviewees living in a non-metropolitan area?
table(CPS$State, is.na(CPS$MetroAreaCode))
names(which(table(CPS$State, is.na(CPS$MetroAreaCode))[,1] == 0))
# "Alaska"  "Wyoming"

# How many states had all interviewees living in a metropolitan area?
names(which(table(CPS$State, is.na(CPS$MetroAreaCode))[,2] == 0))
# [1] "District of Columbia" "New Jersey"          
# [3] "Rhode Island" 

# Which region of the United States has the largest proportion of 
# interviewees living in a non-metropolitan area?
table(CPS$Region, is.na(CPS$MetroAreaCode))
names(which.max(table(CPS$Region, is.na(CPS$MetroAreaCode))[,2] /
                  table(CPS$Region, is.na(CPS$MetroAreaCode))[,1]))
# [1] "Midwest"


# Which state has a proportion of interviewees living in a 
# non-metropolitan area closest to 30%?
sort(tapply(CPS$MetroAreaCode, factor(CPS$State), 
            function(x) mean(is.na(x)))) # Wisconsin

# Which state has the largest proportion of non-metropolitan interviewees, 
# ignoring states where all interviewees were non-metropolitan?
sort(tapply(CPS$MetroAreaCode, factor(CPS$State), 
       function(x) mean(is.na(x)))) # Montana

# How many observations (codes for metropolitan areas) are there in MetroAreaMap?
MetroAreaMap <- read_csv("~/Work In Progress/Analytics Edge/Unit 01/MetroAreaCodes.csv")
CountryMap <- read_csv("~/Work In Progress/Analytics Edge/Unit 01/CountryCodes.csv")
nrow(MetroAreaMap) # 271

# How many observations (codes for countries) are there in CountryMap?
nrow(CountryMap)  # 149

# Integrating Metropolitan Area Data
CPS <- merge(CPS, MetroAreaMap, by.x = 'MetroAreaCode', 
             by.y = 'Code', all.x = TRUE)

# What is the name of the variable that was added to the data frame by the 
# merge() operation 
names(CPS)[length(names(CPS))] # MetroArea

# How many interviewees have a missing value for the new metropolitan area 
# variable?
sum(is.na(CPS$MetroArea)) # 34238

# Metropolitan areas with largest number of interviewees?
sort(table(CPS$MetroArea), decreasing = TRUE)[1:10]

# Which metropolitan area has the highest proportion of interviewees of 
# Hispanic ethnicity?
names(sort(table(CPS$MetroArea, CPS$Hispanic)[,2] /
             table(CPS$MetroArea, CPS$Hispanic)[,1], decreasing = TRUE)[1])
# [1] "Laredo, TX"

# determine the number of metropolitan areas in the United States from which 
# at least 20% of interviewees are Asian
names(tapply(CPS$Race == 'Asian', 
             CPS$MetroArea,mean)[which(tapply(CPS$Race == 'Asian', 
                                              CPS$MetroArea,mean) >= 0.2)])
# [1] "Honolulu, HI"                      
# [2] "San Francisco-Oakland-Fremont, CA" 
# [3] "San Jose-Sunnyvale-Santa Clara, CA"
# [4] "Vallejo-Fairfield, CA" 
# 4

# determine which metropolitan area has the smallest proportion of interviewees 
# who have received no high school diploma
names(sort(tapply(CPS$Education == "No high school diploma", 
            CPS$MetroArea, mean, na.rm = TRUE))[1])
# [1] "Iowa City, IA"

# Integrating Country of Birth Data
CPS <- merge(CPS, CountryMap, by.x = 'CountryOfBirthCode',
             by.y = 'Code', all.x = TRUE)

# What is the name of the variable added to the CPS data frame by this 
# merge operation?
names(CPS)[length(names(CPS))] # Country

# How many interviewees have a missing value for the new country of birth 
# variable?
sum(is.na(CPS$Country)) # 176

# Among all interviewees born outside of North America, 
# which country was the most common place of birth?
names(sort(table(CPS$Country), decreasing = TRUE)[1:10]) # Philippines

# What proportion of the interviewees from the 
# "New York-Northern New Jersey-Long Island, NY-NJ-PA" metropolitan area 
# have a country of birth that is not the United States?
mean(subset(CPS, 
            MetroArea == 'New York-Northern New Jersey-Long Island, NY-NJ-PA')$Country != 'United States',
     na.rm = TRUE) # 0.8774995

# Which metropolitan area has the largest number (note -- not proportion) of 
# interviewees with a country of birth in India? 
sort(table(subset(CPS, Country == 'India')$MetroArea),
     decreasing = TRUE, na.rm = TRUE)[1]
# ...in Brazil
sort(table(subset(CPS, Country == 'Brazil')$MetroArea),
     decreasing = TRUE, na.rm = TRUE)[1]
# ... in Somalia
sort(table(subset(CPS, Country == 'Somalia')$MetroArea),
     decreasing = TRUE, na.rm = TRUE)[1]
