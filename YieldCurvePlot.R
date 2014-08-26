library(quantmod)
library(rgl)

#Specify tickers and download data
tickers <- c("DGS1MO","DGS3MO","DGS6MO",
             "DGS1","DGS2","DGS3","DGS5",
             "DGS7","DGS10","DGS20", "DGS30")

ticker.names <- c("1mo","3mo","6mo",
                  "1y","2y","3y","5y",
                  "7y","10y","20y","30y")

getSymbols(tickers, src = "FRED")


#combine all the data
yields.df <- NULL
for (i in tickers) {
  yields.df <- cbind(yields.df, get(i))
}

#clean up data to remove the days where everythings NA
yields.df <- yields.df[rowSums(is.na(yields.df))<3,]
colnames(yields.df) <- ticker.names

#get a couple of descriptors of data for later plotting use
time.index <- index(yields.df)
year.interval <- floor(as.numeric(max(time.index) - min(time.index))/365)
date.axis <- seq(min(time.index), max(time.index), length.out=8)

#specify graph dimensions. Currently, x-axis is 2x y-axis
z <- as.matrix(yields.df)
y <- 1:ncol(z)
x <- seq(from = 1, to = ncol(z)*2, length.out = nrow(z))

#specify colours
zlim <- range(z)
zlen <- zlim[2] - zlim[1] + 1
jet.colors <- 
  colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
colorzjet <- jet.colors(100)


#plot the data
open3d()
surface3d(x, y, z, 
          color = colorzjet[findInterval(z, seq(0.01, 8, length=100))]
          )

#add axis labeling and other formatting
axis3d(nticks = length(ticker.names)-1, edge="y+-",labels = ticker.names, cex=0.6)
axis3d(edge = "x", at = seq(1,length(y)*2, length.out=length(date.axis)), labels = date.axis, cex=0.6)
axis3d(edge = "z++",cex=0.6)
mtext3d("Yield %", edge='z++', line=1.75, cex=0.75)
mtext3d("Date", edge='x--', line=2.00, cex=0.75)
mtext3d("Maturity", edge='y+-', line=1.75, cex=0.75)
grid3d(c("y+", "x-", "z-"))