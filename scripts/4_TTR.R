#' Author: Ted Kwartler
#' Date: 4-26-2019
#' Purpose: MACD Example As Indicator
#'

# Opts
options(scipen=999)

# Libs
library(TTR)
library(quantmod)
library(PerformanceAnalytics)
library(dygraphs)
library(htmltools)

# Get Chipotle
getSymbols("CMG")
CMG      <- CMG['2018-01-01/']

## Manual MACD
# FAST MA
CMGsma12 <- SMA(CMG$CMG.Close, 12)
tail(CMGsma12, 10) 

# SLOW MA
CMGsma26 <- SMA(CMG$CMG.Close, 26)
tail(CMGsma26, 10) 

# MA Difference
SMAdiff <- CMGsma12 - CMGsma26
tail(SMAdiff, 10) 
tail(CMGsma12, 1) - tail(CMGsma26, 1)

# Another MA of the difference
manualSig <- SMA(SMAdiff,9)

# Calculate the moving Avgs with a TTR function
CMGmacd <- MACD(CMG$CMG.Close,
                nFast = 12, nSlow = 26, nSig = 9, 
                maType="SMA", #Usually EMA
                percent = F) # Values or Percents

# Examine
tail(CMGmacd)
tail(manualSig)

# Easier to interpret as a percent
CMGmacdPer <- MACD(CMG$CMG.Close,
                   nFast = 12, nSlow = 26, nSig = 9, 
                   maType="SMA", #Usually EMA; not covered
                   percent = T)

# Now let's visualize in a stacked dynamic plot
browsable(
  tagList(
    dygraph(CMG$CMG.Close, group = "Price", height = 200, width = "100%"),
    dygraph(CMGmacdPer,group = "Price", height = 200, width = "100%") %>%
      dySeries('macd',label='MACD') %>%
      dySeries('signal',label='SIGNAL') %>%
      dyRangeSelector()
  )
)

# Now let's perform a backtest starting 2018
getSymbols("CMG")
CMG      <- CMG['2018-01-01/']
CMGmacdPer <- MACD(CMG$CMG.Close,
                   nFast = 12, nSlow = 26, nSig = 9, 
                   maType="SMA", #Usually EMA; not covered
                   percent = T)

tradeSignal <- Lag(ifelse(CMGmacdPer$macd > CMGmacdPer$signal  , 1, 0))
ret         <- ROC(Cl(CMG))*tradeSignal #Rate of Change TTR::ROC()

# Review your return
charts.PerformanceSummary(ret)

# End
