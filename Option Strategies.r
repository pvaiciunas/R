
# These functions should create all combinations of options tickers
# That constitute the strategy. Another function should follow to 
# take these lists of strategies and calcualte all other necessary info.
# (i.e. payoffs, costs, exposures to greeks, etc)
# Input is an option DF
# Output is a 2 column data frame with each record containing a list of options to buy
# and a list of options to sell

# These spread strategies are too specific at this point, they only really
# allow for one ITM and one OTM option to be bought and sold. Will need to
# generalize this a lot more so that more complex strategies can be modeled

# Bull Call Spread
BullCallSpread <- function(OptionDF) {
#
# Buy 1 ITM Call
# Sell 1 OTM Call
#
# Input is a full OptionDF object
# Output is a data.frame columns denote the options to buy and sell
# Each item in the dataframe is a list of option tickers

# Clean up data and split the data frame into separate months
	stockPrice <- OptionDF$stockPrice[1]
	allCallOptions <- OptionDF[OptionDF$type == 'call',]
	splitDF <- split(allCallOptions, allCallOptions$expiry)
  	output <- data.frame()
  
  #for each ITM call, create all potential spreads inter-month
	for (i in 1:length(splitDF)) {
  		ITMOptions <- splitDF[[i]][splitDF[[i]]$strike <= stockPrice,]$optionCode
		OTMOptions <- splitDF[[i]][splitDF[[i]]$strike > stockPrice,]$optionCode
  		possibleCombos <- expand.grid(ITMOptions, OTMOptions)
		possibleCombosLists <- data.frame(I(split(possibleCombos[,1], rownames(possibleCombos))),
								I(split(possibleCombos[,2], rownames(possibleCombos))))
    
		output <- rbind(output, possibleCombosLists)
    }
	
	#Output
	output[,3] <- "bull.call.spread"
	names(output) <- c("buy","sell", "strategy")
	rownames(output) <- 1:nrow(output)
	output
}


# Bull Put Spread
BullPutSpread <- function(OptionDF) {
#
# Buy 1 OTM Put
# Sell 1 ITM Put
#
# Input is a full OptionDF object
# Output is a data.frame columns denote the options to buy and sell
# Each item in the dataframe is a list of option tickers

# Clean up data and split the data frame into separate months
	stockPrice <- OptionDF$stockPrice[1]
	allCallOptions <- OptionDF[OptionDF$type == 'put',]
	splitDF <- split(allCallOptions, allCallOptions$expiry)
  	output <- data.frame()
  
  #for each ITM call, create all potential spreads inter-month
	for (i in 1:length(splitDF)) {
  		ITMOptions <- splitDF[[i]][splitDF[[i]]$strike <= stockPrice,]$optionCode
		OTMOptions <- splitDF[[i]][splitDF[[i]]$strike > stockPrice,]$optionCode
  		possibleCombos <- expand.grid(OTMOptions, ITMOptions)
		possibleCombosLists <- data.frame(I(split(possibleCombos[,1], rownames(possibleCombos))),
								I(split(possibleCombos[,2], rownames(possibleCombos))))
    
		output <- rbind(output, possibleCombosLists)
    }
	
	#Output
	output[,3] <- "bull.put.spread"
	names(output) <- c("buy","sell", "strategy")
	rownames(output) <- 1:nrow(output)
	output
}


# Bull Put Spread
BearCallSpread <- function(OptionDF) {
#
# Buy 1 OTM Call
# Sell 1 ITM Call
#
# Input is a full OptionDF object
# Output is a data.frame columns denote the options to buy and sell
# Each item in the dataframe is a list of option tickers

# Clean up data and split the data frame into separate months
	stockPrice <- OptionDF$stockPrice[1]
	allCallOptions <- OptionDF[OptionDF$type == 'call',]
	splitDF <- split(allCallOptions, allCallOptions$expiry)
  	output <- data.frame()
  
  #for each ITM call, create all potential spreads inter-month
	for (i in 1:length(splitDF)) {
  		ITMOptions <- splitDF[[i]][splitDF[[i]]$strike <= stockPrice,]$optionCode
		OTMOptions <- splitDF[[i]][splitDF[[i]]$strike > stockPrice,]$optionCode
  		possibleCombos <- expand.grid(OTMOptions, ITMOptions)
		possibleCombosLists <- data.frame(I(split(possibleCombos[,1], rownames(possibleCombos))),
								I(split(possibleCombos[,2], rownames(possibleCombos))))
    
		output <- rbind(output, possibleCombosLists)
    }
	
	#Output
	output[,3] <- "bear.call.spread"
	names(output) <- c("buy","sell", "strategy")
	rownames(output) <- 1:nrow(output)
	output
}


# Bear Put Spread
BearPutSpread <- function(OptionDF) {
#
# Buy 1 ITM Put
# Sell 1 OTM Put
#
# Input is a full OptionDF object
# Output is a data.frame columns denote the options to buy and sell
# Each item in the dataframe is a list of option tickers

# Clean up data and split the data frame into separate months
	stockPrice <- OptionDF$stockPrice[1]
	allCallOptions <- OptionDF[OptionDF$type == 'put',]
	splitDF <- split(allCallOptions, allCallOptions$expiry)
  	output <- data.frame()
  
  #for each ITM call, create all potential spreads inter-month
	for (i in 1:length(splitDF)) {
  		ITMOptions <- splitDF[[i]][splitDF[[i]]$strike <= stockPrice,]$optionCode
		OTMOptions <- splitDF[[i]][splitDF[[i]]$strike > stockPrice,]$optionCode
  		possibleCombos <- expand.grid(ITMOptions, OTMOptions)
		possibleCombosLists <- data.frame(I(split(possibleCombos[,1], rownames(possibleCombos))),
								I(split(possibleCombos[,2], rownames(possibleCombos))))
    
		output <- rbind(output, possibleCombosLists)
    }
	
	#Output
	output[,3] <- "bear.put.spread"
	names(output) <- c("buy","sell", "strategy")
	rownames(output) <- 1:nrow(output)
	output
}



# Generalized Payoff Function
#	i.e. given a set of option, generate a payoff graph, as well as 
#	relevant statistics on:
#	cost, greek exposure, 

OptionPayoff <- function(stockPrice, strikePrice, optionType) {

	if(!(optionType == 'call' || optionType == 'put')) {
		return(NA)
	}
	
	payoff <- ifelse(optionType == 'call', 
				max((stockPrice - strikePrice), 0), 
				max((strikePrice - stockPrice), 0))
	
	payoff
}

OptionPayoffPlot <- function(optionsCode, riskFreeRate) {

	optionDetails <- SplitOptionCode(optionCodes)
	maxStrike <- max(optionDetails$Strike)
	minStike <- min(optionDetails$Strike)
	medianStrike <- median(optionDetails$Strike)
	
	payoffs <- 
	
}

#TODO
ImpliedDividend <- function(){}

#TODO
# Part of the Arb strategies section
PutCallParity <- function() {


}

