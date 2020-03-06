# Portfolio Optimization
# Minimum Variance Portfolio & Efficient Frontier


# Install the necessary packages


# [NOTE] Package "timeSeries" also installs package "timeDate" as dependency.
install.packages("timeSeries")

# [NOTE] Package "fportfolio" also installs packages: "cubature", "mvtnorm", "mnormt", "numDeriv", "Rcpp", "bitops", "gss", "stabledist", "fMultivar", "sn", "ecodist", "mvnormtest", "energy", "DEoptimR", "truncnorm", "RCurl", "XML", "fBasics", "fAssets", "fCopulae", "robustbase", "Rglpk", "slam", "Rsolnp", "quadprog", "kernlab", "rneos" as dependencies.
install.packages("fPortfolio")

# [NOTE] Package "quantmod" also installs packages: "xts", "zoo", "TTR", "curl" as dependencies.
install.packages("quantmod")
install.packages("caTools")
install.packages("PerformanceAnalytics")

# [NOTE] Package "ggplot2" also installs packages: "ps", "processx", "callr", "prettyunits", "backports", "desc", "pkgbuild", "rprojroot", "rstudioapi", "ellipsis", "evaluate", "magrittr", "pkgload", "praise", "colorspace", "assertthat", "utf8", "vctrs", "testthat", "farver", "labeling", "munsell", "R6", "RColorBrewer", "viridisLite", "lifecycle", "cli", "crayon", "fansi", "pillar", "pkgconfig", "digest", "glue", "gtable", "isoband", "rlang", "scales", "tibble", "withr" as dependencies.
install.packages("ggplot2")

# [NOTE] Package "dplyr" also installs packages: "purrr", "tidyselect", "BH", "plogr" as dependencies.
install.packages("dplyr")


# Include the required packages


library(timeDate)
library(timeSeries)
library(fPortfolio)
library(quantmod)
library(caTools)
library(PerformanceAnalytics)
library(ggplot2)
library(dplyr)


# Declare global variables


start_date <- "2015-1-1"


# Declare 25 stocks' tickers


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


# Make a data frame with your tickers prices


tickers <- cbind.data.frame(AAPL,MSFT,AMZN,GOOGL,ADBE,ORCL,SAP,NVDA,CSCO,QCOM,VMW,HTHIY,CRM,IBM,CTSH,JPM,MA,AMD,INTC,MS,GS,WMT,AXP,BAC,C)


# Turn that data frame into timeseries class data 


stock_series <- as.timeSeries(tickers)

daily_price <- stock_series %>% as.data.frame()
daily_price <- daily_price[colSums(is.na(daily_price)) < 100] %>% na.locf(na.rm = FALSE) %>% xts(order.by = as.Date(rownames(daily_price)))

write.csv(as.data.frame(daily_price),"daily_price.csv")

daily_log_ret<- Return.calculate(xts(daily_price), method = "log")[-1,] %>% timeSeries::timeSeries()
xts_monthly_nav <- to.monthly(xts(daily_price), indexAt = "last", OHLC = FALSE)
monthly_log_ret<- Return.calculate(xts_monthly_nav, method = "log")[-1,] %>% timeSeries::timeSeries()

Spec = portfolioSpec()
setRiskFreeRate(Spec)<-0.02


# Make & plot efficent frontier


efficient_frontier <- portfolioFrontier(monthly_log_ret,Spec, constraints = "longOnly")
tailoredFrontierPlot(efficient_frontier)


# Unutilised code segment


# frontierPlot(efficient_frontier,  frontier = c("both", "lower", "upper"), col = c("black", "grey"), add = FALSE, labels = TRUE, return = c("mean", "mu"), risk = c("Cov", "Sigma", "CVaR", "VaR"), auto = TRUE, title = TRUE)
# plot(efficient_frontier,c(1,2,3))
# get the efficient frontier weights
# frontier_weights <- getWeights(efficient_frontier)
# View(frontier_weights)
# frontierPlot(efficient_frontier,pch=19,col = c('blue','light blue'),add = T);


# Find minimum variance portfolio


minvariancePortfolio(monthly_log_ret, constraints = "longOnly")
tangencyPortfolio(monthly_log_ret, constraints = "longOnly")


# Unutilised code segment


# portfolioData()

