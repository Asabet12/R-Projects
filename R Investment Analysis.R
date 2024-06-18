
#### Created by Ahmed Sabet
#### Hult International Business Schol

#linking the data for Yahoo Finance
#Install.Packages Quantmod
library(quantmod)

#Step 1 for Ishares Global ETF 
Stock1 <- getSymbols("IXN", auto.assign = FALSE)
#Step 2  for NASDAQ 100
Stock2 <- getSymbols("QQQ", auto.assign = FALSE)
#Step 3 for iShares 7-10 Year Treasury Bond ETF	
Stock3 <- getSymbols("IEF", auto.assign = FALSE)
#Step4 for Vangurad Real Estate 
Stock4 <- getSymbols("VNQ", auto.assign = FALSE)
#Step5   for 	SPDR Gold Shares 
Stock5 <- getSymbols("GLD", auto.assign = FALSE)

#### Suggestions for new Tickers

Stock6 <- getSymbols("OMFL", auto.assign = FALSE)
Stock7 <- getSymbols("PGHY", auto.assign = FALSE)
Stock8 <- getSymbols("IXN", auto.assign = FALSE)
Stock9 <- getSymbols("QQQ", auto.assign = FALSE)
Stock10 <- getSymbols("GLD", auto.assign = FALSE)


joined_prices_new <- merge.xts(Stock6,Stock7,Stock8,Stock9,Stock10,benchmark_returns)
joined_prices_only_new <- joined_prices_new[, c(6,12,18,24,30,36)]



  time_index <- nrow(joined_returns_12M_new)

OMFL_Sigma <- sd(joined_returns_12M_new$OMFL_ROR[time_index:(time_index - 11)])
PGHY_Sigma <- sd(joined_returns_12M_new$PGHY_ROR[time_index:(time_index - 11)])



### Setting up the New Portfolio after Reallocation

### Setting up the New Tickers
IXN_alloc_1 <- 0.175									
QQQ_alloc_1 <- 0.221
PGHY_alloc_1 <- 0.285
OMFL_alloc_1 <- 0.089
GLD_alloc_1 <- 0.23


joined_returns_12M$Portfolio_ROR_new <- with(joined_returns_12M,
                                         IXN_alloc_1 * IXN_ROR +
                                           QQQ_alloc_1 * QQQ_ROR +
                                           PGHY_alloc_1 * PGHY_ROR +
                                           OMFL_alloc_1 * OMFL_ROR +
                                           GLD_alloc_1 * GLD_ROR)




### Joinging the Data in one Dataframe
joined_prices <- merge.xts(Stock1,Stock2,Stock3,Stock4,Stock5)
                          
## Getting the Adjusted Prices Only
joined_prices_only<- joined_prices[ ,c(6,12,18,24,30) ]

Stock1_returns <- monthlyReturn(getSymbols('IXN', auto.assign = FALSE))
Stock2_returns <- monthlyReturn(getSymbols('QQQ', auto.assign = FALSE))
Stock3_returns <- monthlyReturn(getSymbols('IEF', auto.assign = FALSE))
Stock4_returns <- monthlyReturn(getSymbols('VNQ', auto.assign = FALSE))
Stock5_returns <- monthlyReturn(getSymbols('GLD', auto.assign = FALSE))

joined.monthylyreturns <- merge.xts(Stock1_returns,Stock2_returns,Stock3_returns,
                                    Stock4_returns,Stock5_returns )


### Distribution Shape and Spread

spread <- function(x){
  my_min <- min(x,na.rm = TRUE)
  my_max <- max(x,na.rm = TRUE)
  my_spread <- my_max - my_min
  my_sd <- sd(x,na.rm = TRUE)
  return(c(my_spread,my_sd))
}

## Calling the spread function
spread(x= joined.monthylyreturns$monthly.returns) #for IXN
spread(x= joined.monthylyreturns$monthly.returns.1) #for NASDAQ 100
spread(x= joined.monthylyreturns$monthly.returns.2) #for iShares Fixed_Income
spread(x= joined.monthylyreturns$monthly.returns.3) #for Vanguard
spread(x= joined.monthylyreturns$monthly.returns.4) #for SPDR GOLD SHARES


### Standard Deviation

#Defining the Time_Index Object
time_index <- nrow(joined_returns_12M)

### Calculating the Standard Deviation for the last for 12
#Ishares Global ETF
IXN_Sigma <-sd(joined_returns_12M$IXN_ROR[time_index:(time_index-11)])
#QQQ Sigma
QQQ_Sigma <-sd(joined_returns_12M$QQQ_ROR[time_index:(time_index-11)])
#IEF Sigma
IEF_Sigma <-sd(joined_returns_12M$IEF_ROR[time_index:(time_index-11)])
#VNQ Sigma
VNQ_Sigma <-sd(joined_returns_12M$VNQ_ROR[time_index:(time_index-11)])
#GLD Sigma
GLD_Sigma <-sd(joined_returns_12M$GLD_ROR[time_index:(time_index-11)])
# Potfolio Sigma
Portfolio_Sigma_12M <-sd(joined_returns_12M$Portfolio_ROR[time_index:(time_index-11)])
Portfolio_Sigma_18M <-sd(joined_returns_18M$Portfolio_ROR[time_index:(time_index-17)])
Portfolio_Sigma_24M <-sd(joined_returns_12M$Portfolio_ROR[time_index:(time_index-23)])


### Setting up the Portfolio Weights

IXN_alloc <- 0.175									
QQQ_alloc <- 0.221
IEF_alloc <- 0.285
VNQ_alloc <- 0.089
GLD_alloc <- 0.23

library(dplyr)
#### Calculating Return for the Each Security and the Entire Portfolio

### Importnat
joined_returns_12M <- as.data.frame(joined_prices_only) %>%
  mutate( IXN_ROR = (IXN.Adjusted - lag(IXN.Adjusted, 250))/ lag(IXN.Adjusted, 250)  )%>%
  mutate( QQQ_ROR = (QQQ.Adjusted - lag(QQQ.Adjusted, 250))/ lag(QQQ.Adjusted, 250)  )%>%
  mutate( IEF_ROR = (IEF.Adjusted - lag(IEF.Adjusted, 250))/ lag(IEF.Adjusted, 250)  )%>%
  mutate( VNQ_ROR = (VNQ.Adjusted - lag(VNQ.Adjusted, 250))/ lag(VNQ.Adjusted, 250)  )%>%
  mutate( GLD_ROR = (GLD.Adjusted - lag(GLD.Adjusted, 250))/ lag(GLD.Adjusted, 250)  )

joined_returns_12M$Portfolio_ROR <- with(joined_returns_12M,
                                     IXN_alloc * IXN_ROR +
                                       QQQ_alloc * QQQ_ROR +
                                       IEF_alloc * IEF_ROR +
                                       VNQ_alloc * VNQ_ROR +
                                       GLD_alloc * GLD_ROR)



joined_returns_18M <- as.data.frame(joined_prices_only) %>%
  mutate( IXN_ROR = (IXN.Adjusted - lag(IXN.Adjusted, 375))/ lag(IXN.Adjusted, 375)  )%>%
  mutate( QQQ_ROR = (QQQ.Adjusted - lag(QQQ.Adjusted, 375))/ lag(QQQ.Adjusted, 375)  )%>%
  mutate( IEF_ROR = (IEF.Adjusted - lag(IEF.Adjusted, 375))/ lag(IEF.Adjusted, 375)  )%>%
  mutate( VNQ_ROR = (VNQ.Adjusted - lag(VNQ.Adjusted, 375))/ lag(VNQ.Adjusted, 375)  )%>%
  mutate( GLD_ROR = (GLD.Adjusted - lag(GLD.Adjusted, 375))/ lag(GLD.Adjusted, 375)  )

joined_returns_18M$Portfolio_ROR <- with(joined_returns_18M,
                                         IXN_alloc * IXN_ROR +
                                           QQQ_alloc * QQQ_ROR +
                                           IEF_alloc * IEF_ROR +
                                           VNQ_alloc * VNQ_ROR +
                                           GLD_alloc * GLD_ROR)


joined_returns_24M <- as.data.frame(joined_prices_only) %>%
  mutate( IXN_ROR = (IXN.Adjusted - lag(IXN.Adjusted, 500))/ lag(IXN.Adjusted, 500)  )%>%
  mutate( QQQ_ROR = (QQQ.Adjusted - lag(QQQ.Adjusted, 500))/ lag(QQQ.Adjusted, 500)  )%>%
  mutate( IEF_ROR = (IEF.Adjusted - lag(IEF.Adjusted, 500))/ lag(IEF.Adjusted, 500)  )%>%
  mutate( VNQ_ROR = (VNQ.Adjusted - lag(VNQ.Adjusted, 500))/ lag(VNQ.Adjusted, 500)  )%>%
  mutate( GLD_ROR = (GLD.Adjusted - lag(GLD.Adjusted, 500))/ lag(GLD.Adjusted, 500)  )

joined_returns_24M$Portfolio_ROR <- with(joined_returns_24M,
                                         IXN_alloc * IXN_ROR +
                                           QQQ_alloc * QQQ_ROR +
                                           IEF_alloc * IEF_ROR +
                                           VNQ_alloc * VNQ_ROR +
                                           GLD_alloc * GLD_ROR)

### Correlation Between the Assets
returns_only_12M <- data.frame(
  IXN_ROR = joined_returns_12M$IXN_ROR,
  QQQ_ROR = joined_returns_12M$QQQ_ROR,
  IEF_ROR = joined_returns_12M$IEF_ROR,
  VNQ_ROR = joined_returns_12M$VNQ_ROR,
  GLD_ROR = joined_returns_12M$GLD_ROR
) 

cov(returns_only_12M, use= 'complete.obs')
cor(returns_only_12M, use= 'complete.obs')



returns_only_18M <- data.frame(
  IXN_ROR = joined_returns_18M$IXN_ROR,
  QQQ_ROR = joined_returns_18M$QQQ_ROR,
  IEF_ROR = joined_returns_18M$IEF_ROR,
  VNQ_ROR = joined_returns_18M$VNQ_ROR,
  GLD_ROR = joined_returns_18M$GLD_ROR
)

cov(returns_only_18M, use= 'complete.obs')
cor(returns_only_18M, use= 'complete.obs')

returns_only_24M <- data.frame(
  IXN_ROR = joined_returns_24M$IXN_ROR,
  QQQ_ROR = joined_returns_24M$QQQ_ROR,
  IEF_ROR = joined_returns_24M$IEF_ROR,
  VNQ_ROR = joined_returns_24M$VNQ_ROR,
  GLD_ROR = joined_returns_24M$GLD_ROR
)

cov(returns_only_24M, use= 'complete.obs')
cor(returns_only_24M, use= 'complete.obs')


time_index <- nrow(joined_returns_12M_new)

### New Portfolio for 12M
### Important
joined_returns_12M_new <- as.data.frame(joined_prices_only_new) %>%
  mutate(OMFL_ROR = (OMFL.Adjusted - lag(OMFL.Adjusted, 250)) / lag(OMFL.Adjusted, 250)) %>%
  mutate(PGHY_ROR = (PGHY.Adjusted - lag(PGHY.Adjusted, 250)) / lag(PGHY.Adjusted, 250)) %>%
  mutate(IXN_ROR = (IXN.Adjusted - lag(IXN.Adjusted, 250)) / lag(IXN.Adjusted, 250)) %>%
  mutate(QQQ_ROR = (QQQ.Adjusted - lag(QQQ.Adjusted, 250)) / lag(QQQ.Adjusted, 250)) %>%
  mutate(GLD_ROR = (GLD.Adjusted - lag(GLD.Adjusted, 250)) / lag(GLD.Adjusted, 250)) %>%
  mutate(SPY_ROR = (SPY.Adjusted - lag(SPY.Adjusted, 250)) / lag(SPY.Adjusted, 250)) 
  

#### Portfolio Return for 12M
joined_returns_12M_new$Portfolio_ROR_new <- with(joined_returns_12M_new,
                                                 OMFL_alloc_1 * OMFL_ROR +
                                                   PGHY_alloc_1 * PGHY_ROR +
                                                   IXN_alloc_1 * IXN_ROR +
                                                   QQQ_alloc_1 * QQQ_ROR +
                                                   GLD_alloc_1 * GLD_ROR)

Portfolio_Sigma_12M_new <-sd(joined_returns_12M_new$Portfolio_ROR_new[time_index:(time_index-11)])


### New Portfolio for 18M
joined_returns_18M_new <- as.data.frame(joined_prices_only_new) %>%
  mutate(OMFL_ROR = (OMFL.Adjusted - lag(OMFL.Adjusted, 375)) / lag(OMFL.Adjusted, 375)) %>%
  mutate(PGHY_ROR = (PGHY.Adjusted - lag(PGHY.Adjusted, 375)) / lag(PGHY.Adjusted, 375)) %>%
  mutate(IXN_ROR = (IXN.Adjusted - lag(IXN.Adjusted, 375)) / lag(IXN.Adjusted, 375)) %>%
  mutate(QQQ_ROR = (QQQ.Adjusted - lag(QQQ.Adjusted, 375)) / lag(QQQ.Adjusted, 375)) %>%
  mutate(GLD_ROR = (GLD.Adjusted - lag(GLD.Adjusted, 375)) / lag(GLD.Adjusted, 375)) %>%
  mutate(SPY_ROR = (SPY.Adjusted - lag(SPY.Adjusted, 375)) / lag(SPY.Adjusted, 375)) 


time_index <- nrow(joined_returns_18M_new)

Portfolio_Sigma_18_new <-sd(joined_returns_18M_new$Portfolio_ROR_new[time_index:(time_index-17)])


#### Portfolio Return for 18M
joined_returns_18M_new$Portfolio_ROR_new <- with(joined_returns_18M_new,
                                                 OMFL_alloc_1 * OMFL_ROR +
                                                   PGHY_alloc_1 * PGHY_ROR +
                                                   IXN_alloc_1 * IXN_ROR +
                                                   QQQ_alloc_1 * QQQ_ROR +
                                                   GLD_alloc_1 * GLD_ROR)


### New Portfolio for 24M
joined_returns_24M_new <- as.data.frame(joined_prices_only_new) %>%
  mutate(OMFL_ROR = (OMFL.Adjusted - lag(OMFL.Adjusted, 500)) / lag(OMFL.Adjusted, 500)) %>%
  mutate(PGHY_ROR = (PGHY.Adjusted - lag(PGHY.Adjusted, 500)) / lag(PGHY.Adjusted, 500)) %>%
  mutate(IXN_ROR = (IXN.Adjusted - lag(IXN.Adjusted, 500)) / lag(IXN.Adjusted, 500)) %>%
  mutate(QQQ_ROR = (QQQ.Adjusted - lag(QQQ.Adjusted, 500)) / lag(QQQ.Adjusted, 500)) %>%
  mutate(GLD_ROR = (GLD.Adjusted - lag(GLD.Adjusted, 500)) / lag(GLD.Adjusted, 500))%>%
  mutate(SPY_ROR = (SPY.Adjusted - lag(SPY.Adjusted, 500)) / lag(SPY.Adjusted, 500)) 


#### Portfolio Return for 24M
joined_returns_24M_new$Portfolio_ROR_new <- with(joined_returns_24M_new,
                                                 OMFL_alloc_1 * OMFL_ROR +
                                                   PGHY_alloc_1 * PGHY_ROR +
                                                   IXN_alloc_1 * IXN_ROR +
                                                   QQQ_alloc_1 * QQQ_ROR +
                                                   GLD_alloc_1 * GLD_ROR)
time_index <- nrow(joined_returns_24M_new)

Portfolio_Sigma_24M_new <-sd(joined_returns_24M_new$Portfolio_ROR_new[time_index:(time_index-23)])


#Calculating the Sharpe Ratio
risk_free <- 0.01
returns_only_12M$IXN_Sharpe <-(returns_only_12M$IXN_ROR - risk_free)/IXN_Sigma
returns_only_12M$QQQ_Sharpe <-(returns_only_12M$QQQ_ROR - risk_free)/QQQ_Sigma
returns_only_12M$IEF_Sharpe <-(returns_only_12M$IEF_ROR - risk_free)/IEF_Sigma
returns_only_12M$VNQ_Sharpe <-(returns_only_12M$VNQ_ROR - risk_free)/VNQ_Sigma
returns_only_12M$GLD_Sharpe <-(returns_only_12M$GLD_ROR - risk_free)/GLD_Sigma

## Creating a Benchmark to see how Securities Perform versus the Benchmark
### Important

benchmark_returns <- (getSymbols("SPY", auto.assign = FALSE))

IXN_te <- sd(joined_returns_12M_new$IXN_ROR[time_index:(time_index-11)] - 
               joined_returns_12M_new$SPY_ROR[time_index:(time_index-11)])
print(IXN_te)

QQQ_te <- sd(joined_returns_12M_new$QQQ_ROR[time_index:(time_index-11)] - 
               joined_returns_12M_new$SPY_ROR[time_index:(time_index-11)])
print(QQQ_te)


GLD_te <- sd(joined_returns_12M_new$GLD_ROR[time_index:(time_index-11)] - 
               joined_returns_12M_new$SPY_ROR[time_index:(time_index-11)])
print(GLD_te)

OMFL_te <- sd(joined_returns_12M_new$OMFL_ROR[time_index:(time_index-11)] - 
               joined_returns_12M_new$SPY_ROR[time_index:(time_index-11)])
print(OMFL_te)

PGHY_te <- sd(joined_returns_12M_new$PGHY_ROR[time_index:(time_index-11)] - 
                joined_returns_12M_new$SPY_ROR[time_index:(time_index-11)])
print(PGHY_te)

spread <- function(x){
  my_min <- min(x,na.rm = TRUE)
  my_max <- max(x,na.rm = TRUE)
  my_spread <- my_max - my_min
  my_sd <- sd(x,na.rm = TRUE)
  return(c(my_spread,my_sd))
}

## Calling the spread function
spread(x= joined_returns_12M_new$OMFL_ROR) 
spread(x= joined_returns_12M_new$PGHY_ROR) 
spread(x= joined_returns_12M_new$IXN_ROR)
spread(x= joined_returns_12M_new$QQQ_ROR)
spread(x= joined_returns_12M_new$GLD_ROR)
