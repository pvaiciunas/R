
# These functions should create all combinations of options tickers
# That constitute the strategy. Another function should follow to 
# take these lists of strategies and calcualte all other necessary info.
# (i.e. payoffs, costs, exposures to greeks, etc)
# Input is an option DF
# Output is a 2 column data frame containing a list of options to buy
# and a list of options to sell


BullCallSpread <- function(OptionDF) {
#
# Buy 1 ITM Call
# Sell 1 OTM Call
#
# Input is a full OptionDF object
# Output is a data.frame containing a list of tickers to buy and sell

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
  
  names(output) <- c("buy","sell")
  rownames(output) <- 1:nrow(output)
  output
  
}




# Bear Call Spread

# Bear Put Spread

# Bull Put Spread

# Box Spread

# Long 


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

ImpliedDividend <- function(){}

PutCallParity <- function() {


}

