#Basic Portfolio Optimixation: Minimum Variance Portfolio & Efficent Frontier

#Installing necessary packages
install.packages("timeSeries")
install.packages("fPortfolio")
install.packages("quantmod")
install.packages("caTools")
install.packages("PerformanceAnalytics")
install.packages("ggplot2")
install.packages("timeDate")

#Including the necessary packages for use
library(timeSeries)
library(fPortfolio)
library(quantmod)
library(caTools)
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)
library(timeDate)

start_date <- "2015-1-1"

AAPL <- getSymbols("AAPL", from =start_date, auto.assign = F)[,4]
MSFT <- getSymbols("MSFT", from = start_date, auto.assign= F)[,4]
AMZN <- getSymbols("AMZN", from = start_date, auto.assign = F)[,4]
GOOGL <- getSymbols("GOOGL", from = start_date, auto.assign = F)[,4]
ADBE <- getSymbols("ADBE", from = start_date, auto.assign = F)[,4]
ORCL <- getSymbols("ORCL", from = start_date, auto.assign = F)[,4]
SAP <- getSymbols("SAP", from =start_date, auto.assign = F)[,4]
NVDA <- getSymbols("NVDA", from = start_date, auto.assign= F)[,4]
CSCO <- getSymbols("CSCO", from = start_date, auto.assign = F)[,4]
QCOM <- getSymbols("QCOM", from = start_date, auto.assign = F)[,4]
VMW <- getSymbols("VMW", from = start_date, auto.assign = F)[,4]
HTHIY <- getSymbols("HTHIY", from = start_date, auto.assign = F)[,4]
CRM <- getSymbols("CRM", from =start_date, auto.assign = F)[,4]
IBM <- getSymbols("IBM", from = start_date, auto.assign= F)[,4]
CTSH <- getSymbols("CTSH", from = start_date, auto.assign = F)[,4]
JPM <- getSymbols("JPM", from = start_date, auto.assign = F)[,4]
MA <- getSymbols("MA", from =start_date, auto.assign = F)[,4]
AMD <- getSymbols("AMD", from = start_date, auto.assign= F)[,4]
INTC <- getSymbols("INTC", from = start_date, auto.assign = F)[,4]
MS <- getSymbols("MS", from = start_date, auto.assign = F)[,4]
GS <- getSymbols("GS", from = start_date, auto.assign = F)[,4]
WMT <- getSymbols("WMT", from = start_date, auto.assign = F)[,4]
AXP <- getSymbols("AXP", from =start_date, auto.assign = F)[,4]
BAC <- getSymbols("BAC", from = start_date, auto.assign= F)[,4]
C <- getSymbols("C", from = start_date, auto.assign = F)[,4]



#Make a data frame with your tickers prices
tickers <- cbind.data.frame(AAPL,MSFT,AMZN,GOOGL,ADBE,ORCL,SAP,NVDA,CSCO,QCOM,VMW,HTHIY,CRM,IBM,CTSH,JPM,MA,AMD,INTC,MS,GS,WMT,AXP,BAC,C)
# ,BABA,BRKA,V,ADBE,ORCL,SAP,SFTBY,VZ,QCOM,VMW,UBER,HTHIY,HNHAF,TWTR,TCEHY,CRM,IBM,ADSK,XIACF,CTSH)

#Turn that data frame into timeseries class data 
portfolioReturns <- as.timeSeries(tickers)

#Make & plot efficent frontier
efficient_frontier <- portfolioFrontier(portfolioReturns, constraints = "longOnly")
frontierPlot(efficient_frontier,  frontier = c("both", "lower", "upper"), col = c("black", "grey"), add = FALSE, labels = TRUE, return = c("mean", "mu"), risk = c("Cov", "Sigma", "CVaR", "VaR"), auto = TRUE, title = TRUE)

#get the efficient frontier weights
#frontier_weights <- getWeights(efficient_frontier)
#View(frontier_weights)

#get minimum variance portfolio
minvariancePortfolio(portfolioReturns, constraints = "longOnly")