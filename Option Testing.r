OptionTest <- function(S = 100, X = 90, Vol = 0.2, rf = 0.0045, T = 0.2) {


output <- matrix(0, 13, 2)
rownames(output) <- c('Price',
'Delta',
'Theta',
'Rho',
'Vega',
'Gamma',
'Vanna',
'Vomma',
'Veta',
'Speed',
'Zomma',
'Colour',
'Ultima')
colnames(output) <- c('Call','Put')

output[1,1] <- round(CalcOptionPrice(S, X, T, rf, rf, Vol, callOrPut = 'call'),4)
output[2,1] <- round(CalcDelta(S, X, T, rf, rf, Vol, callOrPut = 'call'),4)
output[3,1] <- round(CalcTheta(S, X, T, rf, rf, Vol, callOrPut = 'call'),4)
output[4,1] <- round(CalcRho(S, X, T, rf, rf, Vol, callOrPut = 'call'),4)
output[5,1] <- round(CalcVega(S, X, T, rf, rf, Vol, callOrPut = 'call'),4)
output[6,1] <- round(CalcGamma(S, X, T, rf, rf, Vol, callOrPut = 'call'),4)
output[7,1] <- round(CalcVanna(S, X, T, rf, rf, Vol, callOrPut = 'call'),4)
output[8,1] <- round(CalcVomma(S, X, T, rf, rf, Vol, callOrPut = 'call'),4)
output[9,1] <- round(CalcVeta(S, X, T, rf, rf, Vol, callOrPut = 'call'),4)
output[10,1] <- round(CalcSpeed(S, X, T, rf, rf, Vol, callOrPut = 'call'),4)
output[11,1] <- round(CalcZomma(S, X, T, rf, rf, Vol, callOrPut = 'call'),4)
output[12,1] <- round(CalcColour(S, X, T, rf, rf, Vol, callOrPut = 'call'),4)
output[13,1] <- round(CalcUltima(S, X, T, rf, rf, Vol, callOrPut = 'call'),4)

output[1,2] <- round(CalcOptionPrice(S, X, T, rf, rf, Vol, callOrPut = 'put'),4)
output[2,2] <- round(CalcDelta(S, X, T, rf, rf, Vol, callOrPut = 'put'),4)
output[3,2] <- round(CalcTheta(S, X, T, rf, rf, Vol, callOrPut = 'put'),4)
output[4,2] <- round(CalcRho(S, X, T, rf, rf, Vol, callOrPut = 'put'),4)
output[5,2] <- round(CalcVega(S, X, T, rf, rf, Vol, callOrPut = 'put'),4)
output[6,2] <- round(CalcGamma(S, X, T, rf, rf, Vol, callOrPut = 'put'),4)
output[7,2] <- round(CalcVanna(S, X, T, rf, rf, Vol, callOrPut = 'put'),4)
output[8,2] <- round(CalcVomma(S, X, T, rf, rf, Vol, callOrPut = 'put'),4)
output[9,2] <- round(CalcVeta(S, X, T, rf, rf, Vol, callOrPut = 'put'),4)
output[10,2] <- round(CalcSpeed(S, X, T, rf, rf, Vol, callOrPut = 'put'),4)
output[11,2] <- round(CalcZomma(S, X, T, rf, rf, Vol, callOrPut = 'put'),4)
output[12,2] <- round(CalcColour(S, X, T, rf, rf, Vol, callOrPut = 'put'),4)
output[13,2] <- round(CalcUltima(S, X, T, rf, rf, Vol, callOrPut = 'put'),4)

output
}

GraphGreekVsTime <- function(n = 365, ...) {

CalcFunc <- list(...)

x <- data.frame(1:n, 1:n)
for (i in 1:365) {
	x[i,1] <- CalcFunc(stockPrice,strikePrice,i/365,riskFreeRate,costOfCarry,
		annVolatility,callOrPut)
next
}
x <- x[order(-x[,2]),]

plot.ts(x)
}


# Test Data for option 3D plotting




Option3DPlot <- function() {

# Creates a 3D plot of an option Greek, time to expiration, and strike price
# relative to the underlying price (i.e. % away)
#
# args: 


# Concatenate the given greek argument (e.g. 'Theta') in to the function that we
# want to use to calculate the data (e.g. 'CalcTheta')


# Calculate the X (time to expiration), Y (Greek), and Z (strike) axes



# Plot the data1
wireframe()


}




	
	
library(c('cwhmisc','quantmod','ggplot2')	

# retrieving options routine after sourcing all the functions from the options module
x <- getOptionChain("K", Exp = NULL)	#will retrieve all expiry dates for options
price <- getQuote('K')$Last	#most recent price
getSymbols('K')	#the price vector for the stock (to calc vol)
priceVec <- K$K.Adjusted	#choose relevent price vector
riskFreeRate <- 0.001
costOfCarry <- 0.001
xDF <- OptionDF(x, price, riskFreeRate, costOfCarry)
xGreeks <- 






googDecCalls <- x$`Dec 2013`$calls #simple dataframe of just the january calls
rownames(googDecCalls) #retrieves the individual options codes for all the options




#create a dataframe of implied volatilities based off of an option chain
S <- getQuote('GOOG')$Last #google price
T <- as.integer((ExtractOpExpiry(rownames(x$`Dec 2013`$calls)[1]) - Sys.Date())) / 365
rf = 0.005
impVols <- data.frame(1:nrow(googDecCalls),1:nrow(googDecCalls),1:nrow(googDecCalls),1:nrow(googDecCalls))
colnames(impVols) <- (c('Code','Strike','Buy Vol @', 'Sell Vol @'))
for (i in 1:length(googDecCalls[,1])) {
	impVols[i,1] <- rownames(googDecCalls)[i]
	impVols[i,2] <- googDecCalls[i,1]
	impVols[i,3] <- CalcImpVol(googDecCalls[i,5],S,googDecCalls[i,1],T,rf,rf)
	impVols[i,4] <- CalcImpVol(googDecCalls[i,4],S,googDecCalls[i,1],T,rf,rf)
}
	
plot.ts(impVols[215:281,2:3], plot.type = 'single', xy.lables = TRUE)	

#look in to using ggplot to plot the volatility smile
#also, look in to referencing the list that is created when you download the full
#option chain, and creating a 3d volaility surface with days to expiration on an axis




#Creating the time series of equity prices
library(quantmod)

TheoreticalPrice
getSymbols('GOOG')
GOOG <- GOOG[,6]
last(GOOG, '7 days') #function used with xts



#Playing around with the number of days to expiration
#first step, referencing the date in the option chain you're using

x <- getOptionChain('GOOG', exp = NULL)
names(x) #gives all the dates
names(x)[1] #references the first date






# TODO:

#A. Take out any rows with NA for Bid or ASk in the main OptionDF function


#1. Box Spread Strategy
# (X2 - X1)(1+ i)^(-T) = C1 - C2 - P1 + P2
# The IRR is the internal rate of return. If this is greater than the risk
# free rate, then the box spread is a good investment. Otherwise, it should be sold
	
#2. Realized Volatility funciton
#	Given a list of tickers, and a period, download all values, put in data frame, 
#	And returns a string of realized volatilites

HistVol <- function(priceVec, n = length(priceVec), periodsPerYear = 252) {

	subPriceVec <- tail(priceVec, n+1) #n+1 so that # of returns == n
	returnVec <- diff(log(subPriceVec))
	sd(returnVec, na.rm = TRUE)*sqrt(periodsPerYear)
 
}

#3. Option Portfolio Greeks
#	Given an OptionDF, calculate all greeks and other values

GreekDF <- function(optionDF, priceVec, RFR, COC, stockPrice = NULL) {
	
	#Break apart all the option codes and store in DF
	codeInfo <- SplitOptionCode(rownames(optionDF))
	
	#If stock price wasn't provided, retrieve in real time
	# ifelse(stockPrice == NULL, 
		   # stockPrice = getQuote(as.character(codeInfo$ticker[1]))$Last)

	#Initialize the data frame with existing data
	GreekDF <- optionDF
		
	#Calculate Realized Volatility
	#Use the timeToExpiry*365 as your number of periods
	#This will be the vol number used for the Greek calculations
	# tempPriceVecs <- lapply(priceVec, tail, n=
	# GreekDF$vol <- mapply(HistVol, tail(priceVec, nrows(GreekDF)+1),
						  # periods = GreekDF$yearsToExpiry*365)
	
	GreekDF$vol <- rep(NA, nrow(GreekDF))
	for (i in 1:nrow(GreekDF)) {
		GreekDF$vol[i] <- round(HistVol(priceVec, n = GreekDF$yearsToExpiry[i]*365),5)
	}
	
	#Calculate the Implied Volatility
	GreekDF$bid.IVol <- round(mapply(CalcImpVol, GreekDF$bid, stockPrice, GreekDF$strike,
					    GreekDF$yearsToExpiry, RFR, COC, GreekDF$type),4)
	GreekDF$ask.IVol <- round(mapply(CalcImpVol, GreekDF$ask, stockPrice, GreekDF$strike,
						GreekDF$yearsToExpiry, RFR, COC, GreekDF$type),4)
	
	#Calculate Theoretical Price
	GreekDF$bsPrice <- round(mapply(CalcOptionPrice, stockPrice, GreekDF$strike, 
					   GreekDF$yearsToExpiry, RFR, COC, GreekDF$vol, GreekDF$type),4)
					   
	#Calculate market price premium
	GreekDF$bidPxPremia <- GreekDF$bid - GreekDF$bsPrice
	GreekDF$askPxPremia <- GreekDF$ask - GreekDF$bsPrice
  
	#Calculate market vol premium
	GreekDF$bidVolPremia <- round(GreekDF$bid.IVol - GreekDF$vol,4)
	GreekDF$askVolPremia <- round(GreekDF$ask.IVol - GreekDF$vol,4)
	
	#Calculate Greeks
	GreekDF$delta <-  round(mapply(CalcDelta, stockPrice, GreekDF$strike,
					  GreekDF$yearsToExpiry, RFR, COC, GreekDF$vol, GreekDF$type),4)
	GreekDF$theta <-  round(mapply(CalcTheta, stockPrice, GreekDF$strike,
					  GreekDF$yearsToExpiry, RFR, COC, GreekDF$vol, GreekDF$type),5)
	GreekDF$rho <-    round(mapply(CalcRho, stockPrice, GreekDF$strike,
				      GreekDF$yearsToExpiry, RFR, COC, GreekDF$vol, GreekDF$type),4)
	GreekDF$vega <-   round(mapply(CalcVega, stockPrice, GreekDF$strike,
					  GreekDF$yearsToExpiry, RFR, COC, GreekDF$vol, GreekDF$type),4)
	GreekDF$Gamma <-  round(mapply(CalcGamma, stockPrice, GreekDF$strike,
					  GreekDF$yearsToExpiry, RFR, COC, GreekDF$vol, GreekDF$type),4)
	GreekDF$vanna <-  round(mapply(CalcVanna, stockPrice, GreekDF$strike,
					  GreekDF$yearsToExpiry, RFR, COC, GreekDF$vol, GreekDF$type),4)
	GreekDF$vomma <-  round(mapply(CalcVomma, stockPrice, GreekDF$strike,
					  GreekDF$yearsToExpiry, RFR, COC, GreekDF$vol, GreekDF$type),4)
	GreekDF$veta <-   round(mapply(CalcVeta, stockPrice, GreekDF$strike,
					  GreekDF$yearsToExpiry, RFR, COC, GreekDF$vol, GreekDF$type),5)
	GreekDF$speed <-  round(mapply(CalcSpeed, stockPrice, GreekDF$strike,
					  GreekDF$yearsToExpiry, RFR, COC, GreekDF$vol, GreekDF$type),4)
	GreekDF$zomma <-  round(mapply(CalcZomma, stockPrice, GreekDF$strike,
					  GreekDF$yearsToExpiry, RFR, COC, GreekDF$vol, GreekDF$type),4)
	GreekDF$colour <- round(mapply(CalcColour, stockPrice, GreekDF$strike,
					  GreekDF$yearsToExpiry, RFR, COC, GreekDF$vol, GreekDF$type),4)
	GreekDF$ultima <- round(mapply(CalcUltima, stockPrice, GreekDF$strike,
					  GreekDF$yearsToExpiry, RFR, COC, GreekDF$vol, GreekDF$type),4)
							 
	GreekDF
}

#4. Payoff function and plotting tool
#	Given a portfolio of options, plot out the payoff function
#	Maybe have a choice of against different metrics (against underlying price, underlying vol, etc)
#	Maybe next step would be to automatically plot against all of these metrics as a series of multiple graphs (probably best)



	
	
	
# Quick test of the impvols
testvol <- CalcImpVol(atest$Ask, atest$stockPrice, atest$Strike, atest$YearsToExpiry, atest$RFR, atest$COC)

	
#IVol Testing SCript
optionPrice <- 17.65
stockPrice <- 187.6
strikePrice <- 170 
yearsToExpiration <- 0.08219178
riskFreeRate <- 0.002
costOfCarry <- 0.002
callOrPut = 'call'

	tolerance <- 0.0001
	lowerBoundVol <- 0.001
	upperBoundVol <- 10
	impliedVol <- 0.25 #starting estimate
	
	i=0
	while (abs(optionPrice - CalcOptionPrice(stockPrice, strikePrice, yearsToExpiration, 
		riskFreeRate, costOfCarry, impliedVol, callOrPut)) > tolerance) {
		
		i = i+1 
		if (i > 30) return(NA)
		
		if (CalcOptionPrice(stockPrice, strikePrice, yearsToExpiration, 
		riskFreeRate, costOfCarry, impliedVol, callOrPut) < optionPrice) {
			lowerBoundVol <- impliedVol
		} else {
			upperBoundVol <- impliedVol
		}
		impliedVol <- mean(c(lowerBoundVol, upperBoundVol))
		
		cat("i: ",i, "\n")
		cat("impliedVol: ",impliedVol, "\n")
		cat("UVol: ",upperBoundVol, "\n")
		cat("LVol: ",lowerBoundVol, "\n")
		cat("UPrice: ",upperBoundPrice, "\n")
		cat("LPrice: ",lowerBoundPrice, "\n")
	}	
	
