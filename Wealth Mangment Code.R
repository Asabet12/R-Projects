
#linking the data for Yahoo Finance
#Install.Packages Quantmod
  
library(quantmod)


#Step 1 for Apple

Stock1 <- getSymbols("AAPL", auto.assign = FALSE)

#Step 2  for Bank of America

Stock2 <- getSymbols("BAC", auto.assign = FALSE)

#Step 3 for American Express

Stock3 <- getSymbols("AXP", auto.assign = FALSE)

#Step4 for Coca Cola
Stock4 <- getSymbols("KO", auto.assign = FALSE)

#Step5 for Chevron 

Stock5 <- getSymbols("CVX", auto.assign = FALSE)

#Step6 for Occidental Pete Corp 

Stock6 <- getSymbols("OXY", auto.assign = FALSE)

#Step7 for  Kraft Heinz

Stock7 <- getSymbols("KHC", auto.assign = FALSE)

#Step8 for  Moodys Corp

Stock8 <- getSymbols("MCO", auto.assign = FALSE)

#Step9 for  HP INC
Stock9 <- getSymbols("HPQ", auto.assign = FALSE)

#Step10 for  DAVITA INC
Stock10 <- getSymbols("DVA", auto.assign = FALSE)

#Step11 for  for the Market
Stock11 <- getSymbols("SPY", auto.assign = FALSE)

#Consolidating the Data in One Data Frame

joined_prices <- merge.xts(Stock1,Stock2,Stock3,Stock4,
                           Stock5,Stock6,Stock7,Stock8,Stock9,
                           Stock10)
joined_prices_only<- joined_prices[ ,c(6,12,18,24,30,36,42,48,54,60) ]

Stock1_returns <- monthlyReturn(getSymbols('AAPL', auto.assign = FALSE))
Stock2_returns <- monthlyReturn(getSymbols('BAC', auto.assign = FALSE))
Stock3_returns <- monthlyReturn(getSymbols('AXP', auto.assign = FALSE))
Stock4_returns <- monthlyReturn(getSymbols('KO', auto.assign = FALSE))
Stock5_returns <- monthlyReturn(getSymbols('CVX', auto.assign = FALSE))
Stock6_returns <- monthlyReturn(getSymbols('OXY', auto.assign = FALSE))
Stock7_returns <- monthlyReturn(getSymbols('KHC', auto.assign = FALSE))
Stock8_returns <- monthlyReturn(getSymbols('MCO', auto.assign = FALSE))
Stock9_returns <- monthlyReturn(getSymbols('HPQ', auto.assign = FALSE))
Stock10_returns <- monthlyReturn(getSymbols('DVA', auto.assign = FALSE))
Stock11_returns <- monthlyReturn(getSymbols('SPY', auto.assign = FALSE))



joined.monthylyreturns <- merge.xts(Stock1_returns,Stock2_returns,Stock3_returns,
                                   Stock4_returns,Stock5_returns,Stock6_returns,
                                   Stock7_returns,Stock8_returns,Stock9_returns
                                    ,Stock10_returns)
                                   
                                            
### Distribution Shape and Spread

spread <- function(x){
  my_min <- min(x,na.rm = TRUE)
  my_max <- max(x,na.rm = TRUE)
  my_spread <- my_max - my_min
  my_sd <- sd(x,na.rm = TRUE)
  return(c(my_spread,my_sd))
}


## Calling the spread function
spread(x= joined.monthylyreturns$monthly.returns) #for Apple
spread(x= joined.monthylyreturns$monthly.returns.1) #for Bank of America
spread(x= joined.monthylyreturns$monthly.returns.2) #for AXP
spread(x= joined.monthylyreturns$monthly.returns.3) #for Coca Cola
spread(x= joined.monthylyreturns$monthly.returns.4) #for Chevron
spread(x= joined.monthylyreturns$monthly.returns.5) #for Occidental Pete Corp 
spread(x= joined.monthylyreturns$monthly.returns.6) #for Kraft Heinz
spread(x= joined.monthylyreturns$monthly.returns.7) #for Moodys Corp
spread(x= joined.monthylyreturns$monthly.returns.8) #for HP INC
spread(x= joined.monthylyreturns$monthly.returns.9) #for DAVITA INC

#Calculating the Weights

AAPL_alloc <- 0.517781817
BAC_alloc <- 0.101306629
AXP_alloc <- 0.085771117
KO_alloc <- 0.085085222
CVX_alloc <- 0.074076614
OXY_alloc <- 0.044686032
KHC_alloc <- 0.043177064
MCO_alloc <- 0.02589252
HPQ_alloc <- 0.012174629
DVA_alloc <- 0.010048356


#Installing the Dplyr Package
library(dplyr)

joined_portfolio_return <- as.data.frame(joined.monthylyreturns)%>%
  mutate(portfolio=  AAPL_alloc*monthly.returns+ 
           BAC_alloc * monthly.returns.1+
           AXP_alloc * monthly.returns.2 + KO_alloc * monthly.returns.3 +
           KO_alloc * monthly.returns.4 +  CVX_alloc * monthly.returns.5 +
           KHC_alloc * monthly.returns.6 + MCO_alloc * monthly.returns.7 +
           HPQ_alloc * monthly.returns.8 + DVA_alloc * monthly.returns.9
          )
#Defining the Time_Index Object
time_index <- nrow(joined.monthylyreturns)

### Calculating the Standard Deviation for the last 3 Years
#Apple Sigma for the Last Year
AAPL_Sigma <-sd(joined.monthylyreturns$monthly.returns[time_index:(time_index-35)])
#BAC Sigma
BAC_Sigma <-sd(joined.monthylyreturns$monthly.returns.1[time_index:(time_index-35)])
#AXP
AXP_Sigma <-sd(joined.monthylyreturns$monthly.returns.2[time_index:(time_index-35)])
#KO Express
KO_Sigma <-sd(joined.monthylyreturns$monthly.returns.3[time_index:(time_index-35)])
#CVX Express
CVX_Sigma <-sd(joined.monthylyreturns$monthly.returns.4[time_index:(time_index-35)])
#OXY Express
OXY_Sigma <-sd(joined.monthylyreturns$monthly.returns.5[time_index:(time_index-35)])
#KHC Express
KHC_Sigma <-sd(joined.monthylyreturns$monthly.returns.6[time_index:(time_index-35)])
#MCO Express
MCO_Sigma <-sd(joined.monthylyreturns$monthly.returns.7[time_index:(time_index-35)])
#HPQ Express
HPQ_Sigma <-sd(joined.monthylyreturns$monthly.returns.8[time_index:(time_index-35)])
#DVA Express
DVA_Sigma <-sd(joined.monthylyreturns$monthly.returns.9[time_index:(time_index-35)])

####### Return for 3 years for each of thet Stocks
#install.packages("dplyr")
library(dplyr)
joined_returns <- as.data.frame(joined_prices_only) %>%
  mutate( AAPL_ROR = (AAPL.Adjusted - lag(AAPL.Adjusted, 750))/ lag(AAPL.Adjusted, 750)  )%>%
  mutate( BAC_ROR = (BAC.Adjusted - lag(BAC.Adjusted, 750))/ lag(BAC.Adjusted, 750)  )%>%
  mutate( AXP_ROR = (AXP.Adjusted - lag(AXP.Adjusted, 750))/ lag(AXP.Adjusted, 750)  )%>%
  mutate( KO_ROR = (KO.Adjusted - lag(KO.Adjusted, 750))/ lag(KO.Adjusted, 750)  )%>%
  mutate( CVX_ROR = (CVX.Adjusted - lag(CVX.Adjusted, 750))/ lag(CVX.Adjusted, 750)  )%>%
  mutate( OXY_ROR = (OXY.Adjusted - lag(OXY.Adjusted, 750))/ lag(OXY.Adjusted, 750)  )%>%
  mutate( KHC_ROR = (KHC.Adjusted - lag(KHC.Adjusted, 750))/ lag(KHC.Adjusted, 750)  )%>%
  mutate( MCO_ROR = (MCO.Adjusted - lag(MCO.Adjusted, 750))/ lag(MCO.Adjusted, 750)  )%>%
  mutate( HPQ_ROR = (HPQ.Adjusted - lag(HPQ.Adjusted, 750))/ lag(HPQ.Adjusted, 750)  )%>%
  mutate( DVA_ROR = (DVA.Adjusted - lag(DVA.Adjusted, 750))/ lag(DVA.Adjusted, 750)  )
  
### Today 3 Year Returns

Portfolio_ret_3Y <- joined_returns[nrow(joined_returns), c("AAPL_ROR", "BAC_ROR", "AXP_ROR", "KO_ROR", "CVX_ROR", "OXY_ROR", "KHC_ROR", "MCO_ROR", "HPQ_ROR", "DVA_ROR")]
# Transpose the data of 'Portfolio_ret_3Y'
transposed_data <- t(Portfolio_ret_3Y)
#Converting the transposed data to a data frame
transposed_data_df <- as.data.frame(transposed_data)

#Row Binding the Securities to the Data Frame
names <- c("AAPL", "BAC", "AXP", "KO", "CVX",
          "OXY", "KHC","MCO", "HPQ", "DVA")

#Standard Deviation of the Portfolio
Portfolio_BYSEC_SD <- c(AAPL_Sigma, BAC_Sigma,AXP_Sigma,KO_Sigma,
                                    CVX_Sigma,OXY_Sigma,KHC_Sigma,MCO_Sigma,
                                  HPQ_Sigma, DVA_Sigma)

#Merging the Data in 1 Data Frame
Portfolio_BYSEC_SD <- as.data.frame(Portfolio_BYSEC_SD)
rownames(Portfolio_BYSEC_SD) <- names
Risk_Return_combined <- cbind(Portfolio_BYSEC_SD, transposed_data_df)

#Changing the column name
names(Risk_Return_combined)[names(Risk_Return_combined) == "2023-07-24"] <- "Expected Return"


#Adding a Column adding calculating the Sharpe Ratio

risk_free_rate <- 0.02
Risk_Return_combined$Sharpe_Ratio <- (Risk_Return_combined$`Expected Return`-risk_free_rate
                                      /Portfolio_BYSEC_SD)

#Correlation Between Securities

####################################################################################

### Covariance Matrix and Correlations
####################################################################################

cov(joined.monthylyreturns, use= 'complete.obs')
cor(joined.monthylyreturns, use= 'complete.obs')
####################################################################################
### Researching Potential Sticks to Recommend to the Client

#Getting the Ticker Data
Stock1_recom <- getSymbols("PG", auto.assign = FALSE)
Stock2_recom <- getSymbols("PEP", auto.assign = FALSE)

#Calculating the Rate of Return
joined_prices_recom <- merge.xts(Stock1_recom,Stock2_recom)
joined_prices_only_recom<- joined_prices_recom[ ,c(6,12) ]

joined_prices_only_recom <- as.data.frame(joined_prices_only_recom) %>%
mutate(PG_ROR = (PG.Adjusted - lag(PG.Adjusted, 750))/ lag(PG.Adjusted, 750)  ) %>%
mutate(PEP_ROR = (PEP.Adjusted - lag(PEP.Adjusted, 750))/ lag(PEP.Adjusted, 750)  )

SD_PG <- sd(joined_prices_only_recom$PG.Adjusted[time_index:(time_index-35)])
SD_PEP <- sd(joined_prices_only_recom$PEP.Adjusted[time_index:(time_index-35)])










