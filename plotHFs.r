#This script isn't generalized. It currently works well only for a set of 18 returns streams
#(densities) with one reference stream to compare against (histogram). 
#If a different amount of return streams are being analyzed, the 'cl' variable, 
#the if/else statment in the for loop, potentially the limits of the histogram, 
#the lty paramter of the legend, and maybe the limits of the densities (currently 
#the -0.1 and 0.1 in the 'plot' function).


#After creating a dataframe of returns of managers called 'x':
for (i in 1:ncol(x)) {
	
	parmList[[i]] <- JohnsonFit(x[,i], moment = "quant")

}

#Then after choosing some type of baseline of a histogram to compare against. Not necessary though
#In this case, It's the Hedge Fund composite
hist(HFs[,1],
freq = FALSE,
ylim = c(0, 40),
xlim = c(-0.1, 0.1),
xlab = "Monthly Returns (Histogram = HF Composite)",
main = "Hedge Fund Distributions"
)

cl <- rep(rainbow(6),3)
	
for (i in 1:length(parmList)) {
  
	if(i > 12) {
		type <- 5
	} else if (i > 6) {
		type <- 4 
	} else {
		type <- 1
	}  
  
	plot(function(x)dJohnson(x,parmList[[i]]),
	-0.1, 
	0.1,
	add = TRUE,
	col = cl[i],
	lty = type,
	lwd = 2
	)
}

legend("topleft",
		legend = names(parmList),
		text.col = cl,
		lty = c(rep(1,6), rep(3,6), rep(5, 6)),
		lwd = 2,
		col = cl,
		cex = 0.8,
		x.intersp = 0.5,
		y.intersp = 0.6,
		bty = "n"
		)
		
