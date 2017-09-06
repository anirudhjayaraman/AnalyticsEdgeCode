setwd("~/Work In Progress/Analytics Edge/Unit 01")
library(readr)
IBM <- read_csv("~/Work In Progress/Analytics Edge/Unit 01/IBMStock.csv")
GE <- read_csv("~/Work In Progress/Analytics Edge/Unit 01/GEStock.csv")
ProcterGamble <- read_csv("~/Work In Progress/Analytics Edge/Unit 01/ProcterGambleStock.csv")
CocaCola <- read_csv("~/Work In Progress/Analytics Edge/Unit 01/CocaCOlaStock.csv")
Boeing <- read_csv("~/Work In Progress/Analytics Edge/Unit 01/BoeingStock.csv")

IBM$Date <- as.Date(IBM$Date, "%m/%d/%y")
GE$Date <- as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date <- as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date <- as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date <- as.Date(Boeing$Date, "%m/%d/%y")

# How many observations are there in each data set?
nrow(IBM) # 480

# What is the earliest year in our datasets?
as.numeric(substring(IBM$Date[1],1,4)) # 1970

# What is the latest year in our datasets?
as.numeric(substring(IBM$Date[nrow(IBM)],1,4)) # 2009

# What is the mean stock price of IBM over this time period?
mean(IBM$StockPrice) # 144.375

# What is the minimum stock price of General Electric (GE) over this time period?
min(GE$StockPrice) # 9.293636

# What is the maximum stock price of Coca-Cola over this time period?
max(CocaCola$StockPrice) # 146.5843

# What is the median stock price of Boeing over this time period?
median(Boeing$StockPrice) # 44.8834

# What is the standard deviation of the stock price of Procter & Gamble 
# over this time period?
sd(ProcterGamble$StockPrice) # 18.19414

# Around what year did Coca-Cola has its highest stock price in this time period?
plot(CocaCola$Date, CocaCola$StockPrice, type = 'l')
as.numeric(substring(CocaCola$Date[which.max(CocaCola$StockPrice)],1,4)) # 1973

# Around what year did Coca-Cola has its lowest stock price in this time period?
as.numeric(substring(CocaCola$Date[which.min(CocaCola$StockPrice)],1,4)) # 1980

# In March of 2000, the technology bubble burst, and a stock market crash 
# occurred. According to this plot, which company's stock dropped more?
ProcterGamble$Year <- as.numeric(substring(ProcterGamble$Date,
                                           1,4))
CocaCola$Year <- as.numeric(substring(CocaCola$Date, 1, 4))

plot(CocaCola$Date, 
     CocaCola$StockPrice, type = 'l', col = 'red',
     xlab = 'Dates', ylab = 'Stock Prices',
     ylim = c(30,150))
lines(ProcterGamble$Date, 
      ProcterGamble$StockPrice, 
      col = 'blue', lty = 2)
legend('topright',lty = c(1,2), col = c('red','blue'),
       legend = c('Coca Cola', 'Procter Gamble'),bty = 'n')
dev.copy(png, 'plot04.png', height = 480, width = 480)
dev.off()

plot(subset(CocaCola, Year >= 2000)$Date, 
     subset(CocaCola, Year >= 2000)$StockPrice, type = 'l', col = 'red',
     xlab = 'Dates', ylab = 'Stock Prices',
     ylim = c(30,130))
lines(subset(ProcterGamble, Year >= 2000)$Date, 
      subset(ProcterGamble, Year >= 2000)$StockPrice, 
      col = 'blue', lty = 2)
legend('topright',lty = c(1,2), col = c('red','blue'),
       legend = c('Coca Cola', 'Procter Gamble'),bty = 'n')
dev.copy(png, 'plot05.png', height = 480, width = 480)
dev.off()

(subset(CocaCola, Year >= 2000)$StockPrice[nrow(subset(CocaCola, Year >= 2000))] / 
  subset(CocaCola, Year >= 2000)$StockPrice[1]) -1
(subset(ProcterGamble, Year >= 2000)$StockPrice[nrow(subset(ProcterGamble, 
                                                           Year >= 2000))] /
  subset(ProcterGamble, Year >= 2000)$StockPrice[1]) - 1
# ProcterGamble

# Around 1983, the stock for one of these companies (Coca-Cola or Procter and 
# Gamble) was going up, while the other was going down. Which one was going up?
# In the time period shown in the plot, which stock generally has lower values?

plot(subset(CocaCola, Year >= 1982)$Date, 
     subset(CocaCola, Year >= 1982)$StockPrice, type = 'l', col = 'red',
     xlab = 'Dates', ylab = 'Stock Prices',
     ylim = c(30,130))
lines(subset(ProcterGamble, Year >= 1982)$Date, 
      subset(ProcterGamble, Year >= 1982)$StockPrice, 
      col = 'blue', lty = 2)
legend('topright',lty = c(1,2), col = c('red','blue'),
       legend = c('Coca Cola', 'Procter Gamble'),bty = 'n')
dev.copy(png, 'plot06.png', height = 480, width = 480)
dev.off()
# Coca Cola

# Which stock fell the most right after the technology bubble burst in March 2000?
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432],
     xlab = 'Dates', ylab = 'Stock Prices',
     type="l", col="red", ylim=c(0,210), lwd = 2)
lines(IBM$Date[301:432], IBM$StockPrice[301:432],
      col = 'blue', lwd = 2)
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432],
      col = 'green', lwd = 2)
lines(GE$Date[301:432], GE$StockPrice[301:432],
      col = 'purple', lwd = 2)
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432],
      col = 'orange', lwd = 2)
legend('topright', lty = c(1,1,1,1,1), lwd = c(2,2,2,2,2), 
       col = c('red','blue','green','purple','orange'),
       legend = c('Coca Cola','IBM','Boeing','GE','Procter Gamble'),
       bty = 'n', cex = 0.75)
abline(v=IBM$Date[which(IBM$Date=='2000-03-01')])
dev.copy(png, 'plot07.png', height = 480, width = 480) # GE
dev.off()

# Which stock reaches the highest value in the time period 1995-2005?
# Clearly, from the previous graph, IBM.

# In October of 1997, there was a global stock market crash that was caused 
# by an economic crisis in Asia. Comparing September 1997 to November 1997, 
# which companies saw a decreasing trend in their stock price?

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432],
     xlab = 'Dates', ylab = 'Stock Prices',
     type="l", col="red", ylim=c(0,210), lwd = 2)
lines(IBM$Date[301:432], IBM$StockPrice[301:432],
      col = 'blue', lwd = 2)
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432],
      col = 'green', lwd = 2)
lines(GE$Date[301:432], GE$StockPrice[301:432],
      col = 'purple', lwd = 2)
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432],
      col = 'orange', lwd = 2)
legend('topright', lty = c(1,1,1,1,1), lwd = c(2,2,2,2,2), 
       col = c('red','blue','green','purple','orange'),
       legend = c('Coca Cola','IBM','Boeing','GE','Procter Gamble'),
       bty = 'n', cex = 0.75)
abline(v=IBM$Date[which(IBM$Date=='1997-09-01')], lty = 2, col = 'brown')
abline(v=IBM$Date[which(IBM$Date=='1997-11-01')], lty = 3, col = 'brown')
dev.copy(png, 'plot08.png', height = 480, width = 480) # GE
dev.off()

IBM$StockPrice[which(IBM$Date=='1997-11-01')] - 
  IBM$StockPrice[which(IBM$Date=='1997-09-01')] # increasing trend
CocaCola$StockPrice[which(CocaCola$Date=='1997-11-01')] - 
  CocaCola$StockPrice[which(CocaCola$Date=='1997-09-01')] # increasing trend
Boeing$StockPrice[which(Boeing$Date=='1997-11-01')] - 
  Boeing$StockPrice[which(Boeing$Date=='1997-09-01')] # decreasing trend
GE$StockPrice[which(GE$Date=='1997-11-01')] - 
  GE$StockPrice[which(GE$Date=='1997-09-01')] # increasing trend
ProcterGamble$StockPrice[which(ProcterGamble$Date=='1997-11-01')] - 
  ProcterGamble$StockPrice[which(ProcterGamble$Date=='1997-09-01')] # decreasing 
# trend

# In the last two years of this time period (2004 and 2005) which stock seems 
# to be performing the best, in terms of increasing stock price?

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432],
     xlab = 'Dates', ylab = 'Stock Prices',
     type="l", col="red", ylim=c(0,210), lwd = 2)
lines(IBM$Date[301:432], IBM$StockPrice[301:432],
      col = 'blue', lwd = 2)
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432],
      col = 'green', lwd = 2)
lines(GE$Date[301:432], GE$StockPrice[301:432],
      col = 'purple', lwd = 2)
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432],
      col = 'orange', lwd = 2)
legend('topright', lty = c(1,1,1,1,1), lwd = c(2,2,2,2,2), 
       col = c('red','blue','green','purple','orange'),
       legend = c('Coca Cola','IBM','Boeing','GE','Procter Gamble'),
       bty = 'n', cex = 0.75)
abline(v=IBM$Date[which(IBM$Date=='2004-01-01')], lty = 2, col = 'brown')
abline(v=IBM$Date[which(IBM$Date=='2005-12-01')], lty = 3, col = 'brown')
dev.copy(png, 'plot09.png', height = 480, width = 480) # GE
dev.off()


IBM$StockPrice[which(IBM$Date=='2005-12-01')] - 
  IBM$StockPrice[which(IBM$Date=='2004-01-01')] # -9.824881
CocaCola$StockPrice[which(CocaCola$Date=='2005-12-01')] - 
  CocaCola$StockPrice[which(CocaCola$Date=='2004-01-01')] # -8.262738
Boeing$StockPrice[which(Boeing$Date=='2005-12-01')] - 
  Boeing$StockPrice[which(Boeing$Date=='2004-01-01')] # 27.83583
GE$StockPrice[which(GE$Date=='2005-12-01')] - 
  GE$StockPrice[which(GE$Date=='2004-01-01')] # 2.918381
ProcterGamble$StockPrice[which(ProcterGamble$Date=='2005-12-01')] - 
  ProcterGamble$StockPrice[which(ProcterGamble$Date=='2004-01-01')] # -41.23874

# Clearly, Boeing performed best in terms of increasing stock prices


# For IBM, compare the monthly averages to the overall average stock price. 
# In which months has IBM historically had a higher stock price (on average)?
tapply(IBM$StockPrice, months(IBM$Date), mean)
which.max(tapply(IBM$StockPrice, months(IBM$Date), mean)) # February
which(tapply(IBM$StockPrice, months(IBM$Date), mean) > 
        mean(IBM$StockPrice)) # April February  January    March      May


# Repeat the tapply function for each of the other four companies
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
tapply(Boeing$StockPrice, months(Boeing$Date), mean)
tapply(GE$StockPrice, months(GE$Date), mean)
tapply(ProcterGamble$StockPrice, months(ProcterGamble$Date), mean)

# General Electric and Coca-Cola both have their highest average stock 
# price in the same month. Which month is this?
which.max(tapply(GE$StockPrice, months(GE$Date), mean)) # April
which.max(tapply(CocaCola$StockPrice, months(CocaCola$Date), mean))
