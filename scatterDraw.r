
scatterDraw <- function(x, pause = 0.15, color = "cornflowerblue") {

#Given an object that's favourable to scatterplotting, this function will
#draw out each individual point from start to finish. It is a time series
#visualization of a scatterplot relationship.

#Currently very specific to the project I was working on - the %% function
#and other labelling will have to updated if the charachteristics of the data
#change.


	#set up the plotting area with the correct scale
	plot(x, type = "n")
	abline(h = 0, lty = 3)
	points(x[1,], lwd = 1, col = "black", pch = 19)
	
	Sys.sleep(pause*2)
	
	for (i in 2:nrow(x)) {
		
		#add the latest point and connect it to the previous one
		points(x[i,], lwd = 1, col = color, pch = 20)
		segments(x[i-1,1],x[i-1,2],x[i,1],x[i,2], col = color)
		
		#in this case, if the data point is for the January month, label it
		if (substr(rownames(x)[i],1,3) == "Jan") {
			textxy(x[i,1], x[i,2], rownames(x)[i], cex = 0.9)
		}
		
		#If the data point is the last one, label it, and mark the point big and red
		if (i == nrow(x)) {
			points(x[i,], lwd = 6, col = "red", pch = 19)
			textxy(x[i,1], x[i,2], rownames(x)[i], cex = 1)
		}
		
		#pause the system between loops so that you can visualize the progression
		Sys.sleep(pause)
		
	}
}
