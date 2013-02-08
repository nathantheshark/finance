library('tseries')
# function for cointegration
cointegration<-function(x,y)
{
	vals<-data.frame(x,y)
	beta<-coef(lm(vals[,2]~vals[,1]+0,data=vals))[1]
	(adf.test(vals[,2]-beta*vals[,1], alternative="stationary", k=0))$p.value
}

# function to backtest long position only pairs trade
bt.pairstrade.longonly <- function(hist.prices.close, hist.prices.open, sd.trigger)
{
	# compute simple returns
	hist.returns.close = ROC(hist.prices.close, type = 'discrete')
	hist.returns.close[1,] = 0
	hist.returns.open = ROC(hist.prices.open, type = 'discrete')
	hist.returns.open[1,] = 0
	
	# calculate spread
	spread = hist.returns.close[,1] - hist.returns.close[,2]
	
	# calculate statistics of spread
	spread.mean = colMeans(spread)
	spread.sd = sd(spread)
	
	# create signal to trade if spread is greater/less than specified amount of standard deviations (sd.trigger)
	signal = matrix(rep(0, nrow(spread)*2), ncol=2)
	colnames(signal) = colnames(hist.returns.close)	
	for (i in 1:nrow(spread)) {
		if (spread[i] > spread.mean + (sd.trigger * spread.sd)) {
			signal[i,2] = 1
		} else if (spread[i] < spread.mean - (sd.trigger * spread.sd)) {
			signal[i,1] = 1		
		}
	}
	
	# lag the signal by 2 -- position not taken until next day's open, and return not realized until following days open
	signal[,1] = Lag(signal[,1], k = 2)
	signal[,2] = Lag(signal[,2], k = 2)
	signal = na.locf(signal, na.rm = FALSE)
	signal[is.na(signal)] = 0
	
	# perform backtest and compute stats
	bt = list()
	bt$ret = hist.returns.open * signal
	bt$equity = cumprod(1 + bt$ret)
	bt$best = c( max(bt$ret[,1]), max(bt$ret[,2]) )
	bt$worst = c( min(bt$ret[,1]), min(bt$ret[,2]) )
	bt$cagr = compute.cagr(bt$equity)
	bt$sd.trigger = sd.trigger
	return(bt)
	
}

###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
# set global paramaters and call libraries
setInternet2(TRUE)
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

load.packages('quantmod')

# load historical prices from Yahoo Finance
#symbols = spl('AAMRQ,DAL,UAL,LCC,ALK,LUV,JBLU')
all.symbols = c('AAMRQ','DAL','UAL','LCC','ALK','LUV','JBLU')
symbols = spl('DAL,AAMRQ')
#symbol.names = spl('American,Delta,United,US Air, Alaska, Southwest, JetBlue')
symbol.names = spl('Delta,American')

#getSymbols(symbols, auto.assign = TRUE)
i=1
j=2
data1 = getSymbols(all.symbols[i], auto.assign=FALSE)
data2 = getSymbols(all.symbols[j], auto.assign=FALSE)


#hist.prices = merge(AAMRQ,DAL,UAL,LCC,ALK,LUV,JBLU)
hist.prices = merge(data1, data2)
hist.prices.close = Cl(hist.prices)
#colnames(hist.prices.close) = symbols
colnames(hist.prices.close) = c(all.symbols[i], all.symbols[j])
hist.prices.open = Op(hist.prices)
#colnames(hist.prices.open) = symbols
colnames(hist.prices.close) = c(all.symbols[i], all.symbols[j])


# remove any missing data
hist.prices.close = na.omit(hist.prices.close)
hist.prices.open = na.omit(hist.prices.open)

# backtest the long only pairs trade
cagr = c()
best = c()
worst = c()
min.equity = c()
max.equity = c()
sequence = seq(0.1, 4, 0.1)
for ( sd.trigger in sequence ) {
	#sd.trigger = 1
	pairs.trade = bt.pairstrade.longonly(hist.prices.close, hist.prices.open, sd.trigger)
	cagr = c(cagr, pairs.trade$cagr, sum(pairs.trade$cagr))
	best = c(best, pairs.trade$best, max(pairs.trade$best))
	worst = c(worst, pairs.trade$worst, min(pairs.trade$worst))
	min.equity = c(min.equity, min(pairs.trade$equity[, 1]), min(pairs.trade$equity[, 2]), min(pairs.trade$equity))
	max.equity = c(max.equity, max(pairs.trade$equity[, 1]), max(pairs.trade$equity[, 2]), max(pairs.trade$equity))
	
}
cagr = matrix(cagr, ncol=3, byrow=TRUE)
best = matrix(best, ncol=3, byrow=TRUE)
worst = matrix(worst, ncol=3, byrow=TRUE)
min.equity = matrix(min.equity, ncol=3, byrow=TRUE)
max.equity = matrix(max.equity, ncol=3, byrow=TRUE)
rownames(cagr) = sequence
rownames(best) = sequence
rownames(worst) = sequence
rownames(min.equity) = sequence
rownames(max.equity) = sequence
colnames(cagr) = c( colnames(hist.prices.close), "overall")
colnames(best) = c( colnames(hist.prices.close), "overall")
colnames(worst) = c( colnames(hist.prices.close), "overall")
colnames(min.equity) = c( colnames(hist.prices.close), "overall")
colnames(max.equity) = c( colnames(hist.prices.close), "overall")


# plot results
xrange = as.numeric(rownames(cagr))
plot(min.equity[, 3], cagr[, 3])
lines(min.equity[, 3], cagr[, 3])




