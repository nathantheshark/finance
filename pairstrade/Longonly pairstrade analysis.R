# Load libraries
setInternet2(TRUE)
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)
load.packages('quantmod')
library(ggplot2)

# Read in data
data <- read.csv("all_data.csv", header=TRUE)

# Sort by cagr
sorted.data <- data[with(data, order(-cagr)),]

# Get data for top x values
values.to.plot = 30
max.per.page = 3
num.this.page = 0
# Initialize pdf file to write to
pdf("Longonly pairstrade spread analysis.pdf")
# Set up 'grids' for plotting
par(mfrow=c(max.per.page, 2))

for (i in 1:values.to.plot) {
	if (num.this.page >= 3) {
		par(mfrow=c(max.per.page, 2))
		num.this.page = 1
	} else {
		num.this.page = num.this.page + 1
	}

	symbols = c(toString(sorted.data$equity.1[i]), toString(sorted.data$equity.2[i]))
	sd.trigger = sorted.data$sd.trigger[i]
	stock.data <- new.env()
	getSymbols(symbols, src = 'yahoo', from = '1900-01-01', env = stock.data, auto.assign = T)

	data1 = stock.data[[ls(stock.data)[1]]]
	data2 = stock.data[[ls(stock.data)[2]]]		
	
	hist.prices = merge(data1, data2)
	hist.prices.close = Cl(hist.prices)
	colnames(hist.prices.close) = c(symbols[1], symbols[2])
	# remove any missing data
	hist.prices.close = na.omit(hist.prices.close)
	# convert to zoo object
	zoo.hist.prices.close = zoo(hist.prices.close)
	
	# plot both historical prices in red and blue
	plot(zoo.hist.prices.close, screens=1, col=c('red', 'blue'), ylab='', xlab='')
	
	# plot the spread, mean, and +/- sd.triggers
	spread = zoo.hist.prices.close[, 1] - zoo.hist.prices.close[, 2]
	spread.mean = mean(spread)
	spread.sd = sd(spread)
	spread.plus.sd = spread.mean + (spread.sd * sd.trigger)
	spread.minus.sd = spread.mean - (spread.sd * sd.trigger)	
	spread.data = cbind(spread, spread.mean, spread.plus.sd, spread.minus.sd)
	plot(spread.data, screens=1, ylab='', xlab='')
}

dev.off()
