library("quantmod")
library("PerformanceAnalytics")
library("zoo")
library("gplots")

#INPUTS
marketSymbol <- "^GSPC"

nLookback <- 20 #The lookback to calcute the moving average / linear regression curve / average true range / standard deviation
nDeviation <- 2

#Specify dates for downloading data, training models and running simulation
startDate = as.Date("2013-01-01") #Specify what date to get the prices from
symbolData <- new.env() #Make a new environment for quantmod to store data in

stockCleanNameFunc <- function(name){
  return(sub("^","",name,fixed=TRUE))
}

getSymbols(marketSymbol, env = symbolData, src = "yahoo", from = startDate)
cleanName <- stockCleanNameFunc(marketSymbol)
mktData <- get(cleanName,symbolData)

linearRegressionCurve <- function(data,n){
  
  regression <- function(dataBlock){
    fit <-lm(dataBlock~seq(1,length(dataBlock),1))
    return(last(fit$fitted.values))
  }
  return (rollapply(data,width=n,regression,align="right",by.column=FALSE,na.pad=TRUE))
}

linearRegressionCurveStandardDeviation <- function(data,n){
  
  deviation <- function(dataBlock){
    fit <-lm(dataBlock~seq(1,length(dataBlock),1))
    quasiMean <- (last(fit$fitted.values))
    quasiMean <- rep(quasiMean,length(dataBlock))
    stDev <- sqrt((1/length(dataBlock))* sum((dataBlock - quasiMean)^2))
    return (stDev)
  }
  return (rollapply(data,width=n,deviation,align="right",by.column=FALSE,na.pad=TRUE))
}

reduceLongTradeEntriesToTradOpenOrClosedSignal <- function(trades){
  #Takes something like
  #000011110000-1-1000011 (1 = go long, -1 = go short)
  #and turns it into
  #00001111111100000011
  
  #trades[is.na(trades)] <- 0
  out <- trades #copy the datastructure over
  currentPos <-0
  for(i in 1:length(out[,1])){
    if((currentPos == 0) & (trades[i,1]==1)){
      currentPos <- 1
      out[i,1] <- currentPos
      next
    }
    if((currentPos == 1) & (trades[i,1]==-1)){
      currentPos <- 0
      out[i,1] <- currentPos
      next
    }
    out[i,1] <- currentPos
  }
  
  return(out)
}

reduceShortTradeEntriesToTradOpenOrClosedSignal <- function(trades){
  return(-1*reduceLongTradeEntriesToTradOpenOrClosedSignal(-1*trades))
}

generateTradingReturns <- function(mktPrices, nLookback, nDeviation, avgFunction, deviationFunction,title,showGraph=TRUE){
  quasiMean <- avgFunction(mktPrices,n=nLookback)
  quasiDeviation <- deviationFunction(mktPrices,n=nLookback)
  colnames(quasiMean) <- "QuasiMean"
  colnames(quasiDeviation) <- "QuasiDeviation"
  price <- Cl(mktPrices)
  
  upperThreshold = quasiMean + nDeviation*quasiDeviation
  lowerThreshold = quasiMean - nDeviation*quasiDeviation
  
  aboveUpperBand <- price>upperThreshold
  belowLowerBand <- price<lowerThreshold
  
  aboveMAvg <- price>quasiMean
  belowMAvg <- price<quasiMean
  
  aboveUpperBand[is.na(aboveUpperBand)]<-0
  belowLowerBand[is.na(belowLowerBand)]<-0
  aboveMAvg[is.na(aboveMAvg)]<-0
  belowMAvg[is.na(belowMAvg)]<-0
  
  
  rawShort <- (-1)*aboveUpperBand+belowMAvg
  shortPositions <- reduceShortTradeEntriesToTradOpenOrClosedSignal(rawShort)
  rawLong <- (-1)*aboveMAvg+belowLowerBand
  longPositions <- reduceLongTradeEntriesToTradOpenOrClosedSignal(rawLong)
  positions = longPositions + shortPositions
  
  signal <- positions
  
  if(showGraph){
    dev.new()
    par(mfrow=c(2,1))
    plot(Cl(mktPrices),type="l",main=paste(marketSymbol, "close prices"))
    lines(upperThreshold,col="red",type="l")
    lines(lowerThreshold,col="red",type="l")
    lines(quasiMean,col="blue",type="l")
    legend('bottomright',c("Close",paste("Band - ",title),paste("Average - ",title)),lty=1, col=c('black', 'red', 'blue'), bty='n', cex=.75)
    plot(signal)
  }
  
  mktReturns <- Cl(mktPrices)/Lag(Cl(mktPrices)) - 1
  tradingReturns <- Lag(signal)*mktReturns
  tradingReturns[is.na(tradingReturns)] <- 0
  colnames(tradingReturns) <- title
  return (tradingReturns)
}

strategySMAandSTDEV <- function(mktData,nLookback,nDeviation){
  generateTradingReturns(mktData,nLookback,nDeviation,function(x,n) { SMA(Cl(x),n) },function(x,n) { rollapply(Cl(x),width=n, align="right",sd) },"Simple Moving Avg - Standard Deviation",FALSE)
}

strategySMAandATR <- function(mktData,nLookback,nDeviation){
  generateTradingReturns(mktData,nLookback,nDeviation,function(x,n) { SMA(Cl(x),n) },function(x,n) { atr <- ATR(x,n); return(atr$atr) },"Simple Moving Avg - Average True Range",FALSE)
}

strategySMAandLRCDev <- function(mktData,nLookback,nDeviation){
  generateTradingReturns(mktData,nLookback,nDeviation,function(x,n) { SMA(Cl(x),n) },function(x,n) { linearRegressionCurveStandardDeviation(Cl(x),n) },"Simple Moving Avg - LRC Deviation",FALSE)
}

strategyLRCandSTDEV <- function(mktData,nLookback,nDeviation){
  generateTradingReturns(mktData,nLookback,nDeviation,function(x,n) { linearRegressionCurve(Cl(x),n) },function(x,n) { rollapply(Cl(x),width=n, align="right",sd) },"Linear Regression Curve - Standard Deviation",FALSE)
}

strategyLRCandATR <- function(mktData,nLookback,nDeviation){
  generateTradingReturns(mktData,nLookback,nDeviation,function(x,n) { linearRegressionCurve(Cl(x),n) },function(x,n) { atr <- ATR(x,n); return(atr$atr) },"Linear Regression Curve - Average True Range",FALSE)
}

strategyLRCandLRCDev <- function(mktData,nLookback,nDeviation){
  generateTradingReturns(mktData,nLookback,nDeviation,function(x,n) { linearRegressionCurve(Cl(x),n) },function(x,n) { linearRegressionCurveStandardDeviation(Cl(x),n) },"Linear Regression Curve - LRC Deviation",FALSE)
}

if(TRUE){
  bollingerBandsSMAandSTDEVTradingReturns <- strategySMAandSTDEV(mktData,nLookback,nDeviation)
  bollingerBandsSMAandATRTradingReturns <- strategySMAandATR(mktData,nLookback,nDeviation)
  bollingerBandsSMAandLRCDevTradingReturns <- strategySMAandLRCDev(mktData,nLookback,nDeviation)
  
  bollingerBandsLRCandSTDEVTradingReturns <- strategyLRCandSTDEV(mktData,nLookback,nDeviation)
  bollingerBandsLRCandATRTradingReturns <- strategyLRCandATR(mktData,nLookback,nDeviation)
  bollingerBandsLRCandLRCDevTradingReturns <- strategyLRCandLRCDev(mktData,nLookback,nDeviation)
  
  
  mktClClRet <- Cl(mktData)/Lag(Cl(mktData))-1
  tradingReturns <- merge(as.zoo(mktClClRet),
                          as.zoo(bollingerBandsSMAandSTDEVTradingReturns),
                          as.zoo(bollingerBandsSMAandATRTradingReturns),
                          as.zoo(bollingerBandsSMAandLRCDevTradingReturns),
                          as.zoo(bollingerBandsLRCandSTDEVTradingReturns),
                          as.zoo(bollingerBandsLRCandATRTradingReturns),
                          as.zoo(bollingerBandsLRCandLRCDevTradingReturns))
  
  dev.new()
  charts.PerformanceSummary(tradingReturns,main=paste("Mean Reversion using nLookback",nLookback,"and nDeviation",nDeviation,"bands"),geometric=FALSE)
  print(table.Stats(tradingReturns))
  cat("Sharpe Ratio")
  print(SharpeRatio.annualized(tradingReturns))
}


colorFunc <- function(x){
  x <- max(-4,min(4,x))
  if(x > 0){
    colorFunc <- rgb(0,(255*x/4)/255 , 0/255, 1)
  } else {
    colorFunc <- rgb((255*(-1*x)/4)/255,0 , 0/255, 1)
  }
}

optimiseTradingStrat <- function(mktData,lookbackStart,lookbackEnd,lookbackStep,deviationStart,deviationEnd,deviationStep,strategy,title){
  lookbackRange <- seq(lookbackStart,lookbackEnd,lookbackStep)
  deviationRange <- seq(deviationStart,deviationEnd,deviationStep)
  combinations <- length(lookbackRange)*length(deviationRange)
  combLookback <- rep(lookbackRange,each=combinations/length(lookbackRange))
  combDeviation <- rep(deviationRange,combinations/length(deviationRange))
  
  optimisationMatrix <- t(rbind(t(combLookback),t(combDeviation),rep(NA,combinations),rep(NA,combinations),rep(NA,combinations)))
  colnames(optimisationMatrix) <- c("Lookback","Deviation","SharpeRatio","CumulativeReturns","MaxDrawDown")
  
  for(i in 1:length(optimisationMatrix[,1])){
    print(paste("On run",i,"out of",length(optimisationMatrix[,1]),"nLookback=",optimisationMatrix[i,"Lookback"],"nDeviation=",optimisationMatrix[i,"Deviation"]))
    runReturns <- strategy(mktData,optimisationMatrix[i,"Lookback"],optimisationMatrix[i,"Deviation"])
    optimisationMatrix[i,"SharpeRatio"] <- SharpeRatio.annualized(runReturns)
    optimisationMatrix[i,"CumulativeReturns"] <- sum(runReturns)
    optimisationMatrix[i,"MaxDrawDown"] <-  maxDrawdown(runReturns,geometric=FALSE)
    print(optimisationMatrix)
  }
  print(optimisationMatrix)
  
  
  
  dev.new()
  z <- matrix(optimisationMatrix[,"SharpeRatio"],nrow=length(lookbackRange),ncol=length(deviationRange),byrow=TRUE)
  colors <- colorFunc(optimisationMatrix[,"SharpeRatio"])
  
  rownames(z) <- lookbackRange
  colnames(z) <-deviationRange
  heatmap.2(z, key=TRUE,trace="none",cellnote=round(z,digits=2),Rowv=NA, Colv=NA, scale="column", margins=c(5,10),xlab="Deviation",ylab="Lookback",main=paste("Sharpe Ratio for Strategy",title))
  
}

if(FALSE){
  dev.new()
  plot(Cl(mktData),type="l",main=paste(marketSymbol, "close prices"))
  lines(SMA(Cl(mktData),n=50),col="red",type="l")
  lines(linearRegressionCurve(Cl(mktData),n=50),col="blue",type="l")
  legend('bottomright',c("Close",paste("Simple Moving Average Lookback=50"),paste("Linear Regression Curve Lookback=50")),lty=1, col=c('black', 'red', 'blue'), bty='n', cex=.75)
}

nLookbackStart <- 20
nLookbackEnd <- 200
nLookbackStep <- 20
nDeviationStart <- 1
nDeviationEnd <- 2.5
nDeviationStep <- 0.1
#optimiseTradingStrat(mktData,nLookbackStart,nLookbackEnd,nLookbackStep,nDeviationStart,nDeviationEnd,nDeviationStep,strategySMAandSTDEV,"AvgFunc=SMA and DeviationFunc=STDEV")
#optimiseTradingStrat(mktData,nLookbackStart,nLookbackEnd,nLookbackStep,nDeviationStart,nDeviationEnd,nDeviationStep,strategySMAandATR,"AvgFunc=SMA and DeviationFunc=ATR")
#optimiseTradingStrat(mktData,nLookbackStart,nLookbackEnd,nLookbackStep,nDeviationStart,nDeviationEnd,nDeviationStep,strategySMAandLRCDev,"AvgFunc=SMA and DeviationFunc=LRCDev")
#optimiseTradingStrat(mktData,nLookbackStart,nLookbackEnd,nLookbackStep,nDeviationStart,nDeviationEnd,nDeviationStep,strategyLRCandSTDEV,"AvgFunc=LRC and DeviationFunc=STDEV")
#optimiseTradingStrat(mktData,nLookbackStart,nLookbackEnd,nLookbackStep,nDeviationStart,nDeviationEnd,nDeviationStep,strategyLRCandATR,"AvgFunc=LRC and DeviationFunc=ATR")
#doptimiseTradingStrat(mktData,nLookbackStart,nLookbackEnd,nLookbackStep,nDeviationStart,nDeviationEnd,nDeviationStep,strategyLRCandLRCDev,"AvgFunc=LRC and DeviationFunc=LRCDev")
