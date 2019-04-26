#' Author: Ted Kwartler
#' Date: 4-26-2019
#' Purpose: Simple Moving Avg Example As Indicator
#'

# Opts
options(scipen=999)

# Libs
library(TTR)
library(quantmod)
library(PerformanceAnalytics)

# Get Chipotle
getSymbols("CMG")
CMG <- CMG['2017-01-01/2019-01-01'] 

# Calculate moving averages
CMGma50  <- SMA(CMG$CMG.Close, 50)
CMGma200 <- SMA(CMG$CMG.Close, 200)

# Review
CMGma50[49:60]
CMGma200[199:210]

# Organize
df          <-data.frame(CMG$CMG.Close,
                         CMGma50, 
                         CMGma200)

# Create a trading indicator
?Lag
df$tradeSig <- Lag(ifelse(df$SMA > df$SMA.1  , 1, 0)) # not discussing shorting stocks (-1)


# Examine
df[325:335,]

# Now let's perform a backtest starting 2018
getSymbols("CMG")
CMG      <- CMG['2018-01-01/']
CMGma50  <- SMA(CMG$CMG.Close, 50)
CMGma200 <- SMA(CMG$CMG.Close, 200)

tradeSignal <- Lag(ifelse(CMGma50 > CMGma200  , 1, 0))
ret         <- ROC(Cl(CMG))*tradeSignal #Rate of Change TTR::ROC()


# Review your return
charts.PerformanceSummary(ret)

# Now let's be knight cap and switch a sign!
getSymbols("CMG") # have to use all data to demonstrate the point
CMGma50     <- SMA(CMG$CMG.Close, 50)
CMGma200    <- SMA(CMG$CMG.Close, 200)
tradeSignal <- Lag(ifelse(CMGma50 < CMGma200  , 1, 0))
ret         <- ROC(Cl(CMG))*tradeSignal #Rate of Change TTR::ROC()

# Review your return
charts.PerformanceSummary(ret)

# End