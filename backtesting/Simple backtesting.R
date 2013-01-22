bt.simple <- function(data, signal) 
{
	# lag singal
	signal = Lag(signal, 1)
	
	# back fill
	signal = na.locf(signal, na.rm = FALSE)
	signal[is.na(signal)] = 0
	
	# calculate Close-to-Close returns
	ret = ROC(Cl(data), type='discrete')
	ret[1] = 0
	
	# compute stats	
	bt = list()
	bt$ret = ret * signal
	bt$equity = cumprod(1 + bt$ret)    	    	
	return(bt)
}

# Test for bt.simple functions
load.packages('quantmod')

# load historical prices from Yahoo Finance
data = getSymbols('SPY', src = 'yahoo', from = '1980-01-01', auto.assign = F)

# Buy & Hold
signal = rep(1, nrow(data))
buy.hold = bt.simple(data, signal)

# MA Cross
sma = SMA(Cl(data),200)
signal = ifelse(Cl(data) > sma, 1, 0)
sma.cross = bt.simple(data, signal)

# Create a chart showing the strategies perfromance in 2000:2009
dates = '2000::2009'
buy.hold.equity = buy.hold$equity[dates] / as.double(buy.hold$equity[dates][1])
sma.cross.equity = sma.cross$equity[dates] / as.double(sma.cross$equity[dates][1])

chartSeries(buy.hold.equity, TA = c(addTA(sma.cross.equity, on=1, col='red')),	
		theme ='white', yrange = range(buy.hold.equity, sma.cross.equity) )	
