setwd("~/Work In Progress/Analytics Edge/Unit 01")
library(readr)
mvt <- read_csv("~/Work In Progress/Analytics Edge/Unit 01/mvtWeek1.csv")
View(mvt)

# How many rows of mvta (observations) are in this mvtaset?
nrow(mvt)

# How many variables are in this mvtaset?
ncol(mvt)

# Using the "max" function, what is the maximum value of the variable "ID"?
max(mvt$ID)

# What is the minimum value of the variable "Beat"?
min(mvt$Beat)

# How many observations have value TRUE in the Arrest variable 
# (this is the number of crimes for which an arrest was made)?
sum(mvt$Arrest)

# How many observations have a LocationDescription value of ALLEY?
sum(mvt$LocationDescription == 'ALLEY')

# What is the month and year of the median mvte in our mvtaset?
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
median(DateConvert) # May 2006

# In which month did the fewest motor vehicle thefts occur?
mvt$Month <- months(DateConvert)
mvt$Weekday <- weekdays(DateConvert)
mvt$Date <- DateConvert
table(mvt$Month) # February

# On which weekday did the most motor vehicle thefts occur?
table(mvt$Weekday) # Friday

# Which month has the largest number of motor vehicle thefts for which 
# an arrest was made?
table(mvt$Month, mvt$Arrest) # December

# In general, does it look like crime increases or decreases from 2002 - 2012?
hist(mvt$Date, breaks = 100) # decreases
dev.copy(png, 'plot1.png', height = 480, width = 480)
dev.off()
# In general, does it look like crime increases or decreases from 2005 - 2008?

# In general, does it look like crime increases or decreases from 2009 - 2011?



























