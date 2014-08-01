
# Bull Call Spread
#
# Buy 1 ITM Call
# Sell 1 OTM Call
#
# Try to maximize spread between upside to downside, as well as where relative
# to the underlying price these domains exist
#
# Input is an OptionDF object

Extract the unique expiration months or timeToExpiries

for each month, for each ITM call, create all potential spreads (Should be # of ITM options ^2)

Of these, rank them based on a few metrics you need to decide on
Also include data on total price and payoff stats
bullCSpreadDF <- 





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
