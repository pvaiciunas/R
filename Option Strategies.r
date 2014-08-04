

BullCSpread <- function(OptionDF) {
  #
  # Buy 1 ITM Call
  # Sell 1 OTM Call
  #
  # Try to maximize spread between upside to downside, as well as where relative
  # to the underlying price these domains exist
  #
  # Input is a full OptionDF object
  
  # Clean up data and split the data frame into separate months
  stockPrice <- OptionDF$stockPrice[1]
  callOptions <- OptionDF[OptionDF$type == 'call',]
  splitDF <- split(callOptions, callOptions$expiry)
  
  # create a pre-allocated list and an empty DF that will eventually be output
  outputList <- vector("list",length(splitDF))
  names(outputList) <- names(splitDF)
  
  # Loop through the option tenors
  for (i in 1:length(splitDF)) {
    
    # create a separate data frame for ITM and OTM options
    tempDF <- splitDF[[i]]
    ITM <- tempDF[tempDF$strike <= tempDF$stockPrice,]
    OTM <- tempDF[tempDF$strike > tempDF$stockPrice,]  
    
    # create empty data frame  
    outputDF <- data.frame()
    
    # Loop through the ITM options, and compare to all OTM options
    # Append to the empty outputDF data frame post-loop
    for (j in 1:nrow(ITM)) {
      loopDF <- data.frame(maxProfit = (OTM$strike - ITM$strike[j] - ITM$ask[j]),
                           maxLoss = OTM$bid - ITM$ask[j],
                           ITMstrike = ITM$strike[j],
                           OTMstrike = OTM$strike,
                           stockPrice = stockPrice,
                           buyPx = ITM$ask[j], 
                           sellPx = OTM$bid,
                           ITMoptionCode = rep(ITM$optionCode[j], nrow(OTM)),
                           OTMoptionCode = OTM$optionCode)
      
      outputDF <- rbind(outputDF, loopDF)
    }  				
    
    #cleanup the resulting data frame, and add to the outputList
    outputDF <- outputDF[order(outputDF$maxProfit, outputDF$maxLoss, decreasing = TRUE),]
    outputDF <- na.omit(outputDF)
    outputList[[i]] <- outputDF
  }
  
  outputList
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

