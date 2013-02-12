library('tseries')
# function for cointegration
cointegration<-function(x,y)
{
	vals<-data.frame(x,y)
	beta<-coef(lm(vals[,2]~vals[,1]+0,data=vals))[1]
	(adf.test(vals[,2]-beta*vals[,1], alternative="stationary", k=0))$p.value
}

# function to backtest long position only pairs trade
bt.pairstrade.longonly <- function(sd.trigger, hist.prices.close, hist.prices.open)
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

# function to find optimal sd.trigger that maximize CAGR
bt.pairstrade.longonly.optim <- function(sd.trigger, hist.prices.close, hist.prices.open)
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
	return(sum(bt$cagr))
	
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

# load S&P Components csv file
SP500 = read.csv('S&P 500 Components.csv', header=TRUE, stringsAsFactors=FALSE)
all.symbols = SP500$Ticker.symbol[1:100]

## list of symbols to be used, along with names
#all.symbols = c('DAL','AAMRQ','UAL','LCC','ALK','LUV','JBLU')
#symbol.names = spl('Delta,American,United,US Air, Alaska, Southwest, JetBlue')

# get all historical stock price data and set to environement all.data
all.data <- new.env()
getSymbols(all.symbols, src = 'yahoo', from = '1900-01-01', env = all.data, auto.assign = T)

# get number of rows N that will be in dataframe of outputs
N = 0
for (a in seq(length(all.symbols)-1:1)) {
	N = N + a
}

# create empty data frame with rownames
df = data.frame(
		equity.1=rep(NA, N), equity.2=rep(NA, N), sd.trigger=rep(NA, N), 
		cagr=rep(NA, N), best=rep(NA, N), worst=rep(NA, N), min.equity=rep(NA, N), 
		max.equity=rep(NA, N), open.correl=rep(NA, N), open.coint.p=rep(NA, N), 
		close.correl=rep(NA, N), close.coint.p=rep(NA, N)
)

###########################################################
## STOP HERE AND GO TO PARALLEL PROCESSING ##
##   2/11/2013 NJB - CHECK VALUES AND CONFIRM OK BEFORE DELETING THE BELOW SECTION  ##

# loop values through all.symbols using i, j
r = 1		# row to paste into

for (i in 1:(length(all.symbols)-1)) {
	for (j in (i+1):length(all.symbols)) {
		
		print(i)
		print(j)
		
		# load historical prices from Yahoo Finance
		data1 = all.data[[ls(all.data)[i]]]
		data2 = all.data[[ls(all.data)[j]]]		
		
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
		
		# find optimal value (maximize cagr)
		optimal.cagr = optimize(f=bt.pairstrade.longonly.optim, interval=0:4, maximum=TRUE, hist.prices.close, hist.prices.open)
		sd.trigger = optimal.cagr$maximum
		
		# get all statistics from backtest
		pairs.trade = bt.pairstrade.longonly(sd.trigger, hist.prices.close, hist.prices.open)
		
		# add row to data.frame df containg output statistics of optimal sd.trigger values
		df[r,] = c(
				all.symbols[i], all.symbols[j],	pairs.trade$sd.trigger, sum(pairs.trade$cagr), 
				max(pairs.trade$best), min(pairs.trade$worst), min(pairs.trade$equity), max(pairs.trade$equity), 
				cor(hist.prices.open[,1], hist.prices.open[,2]), cointegration(hist.prices.open[,1], hist.prices.open[,2]), 
				cor(hist.prices.close[,1], hist.prices.close[,2]), cointegration(hist.prices.close[,1], hist.prices.close[,2])
				)
		r = r + 1
	}
}

##############################################
############# parallel processing ############
library(foreach)
library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)

# create list objects of all pairs historical prices for open and close
all.hist.prices.close = list()
all.hist.prices.open = list()
l = 1
for (i in 1:(length(all.symbols)-1)) {
	for (j in (i+1):length(all.symbols)) {
		print(i)
		print(j)
		
		# load historical prices from Yahoo Finance
		data1 = all.data[[ls(all.data)[i]]]
		data2 = all.data[[ls(all.data)[j]]]		
		
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
		
		all.hist.prices.close[[l]] = hist.prices.close
		all.hist.prices.open[[l]] = hist.prices.open
		l = l + 1		
		
	}
}

# use parallel computing to find optimal sd.trigger for each possible pair
tic <- Sys.time()
optimal.sd.triggers <- foreach(l=1:(length(all.hist.prices.open)), .combine='c', .packages='quantmod') %dopar% {
		hist.prices.close = all.hist.prices.close[[l]]
	hist.prices.open = all.hist.prices.open[[l]]
	
	# find optimal value (maximize cagr)
	optimal.cagr = optimize(f=bt.pairstrade.longonly.optim, interval=0:4, maximum=TRUE, hist.prices.close, hist.prices.open)
	sd.trigger = optimal.cagr$maximum
}
Sys.time() - tic

# use backtesting to get stats for all optimal sd.trigger for all pairs and populate to result data.frame
# loop values through all.symbols using i, j
l = 1		# row to paste into

for (i in 1:(length(ls(all.data))-1)) {
	for (j in (i+1):length(ls(all.data))) {		
		print(i)
		print(j)
		
		sd.trigger = optimal.sd.triggers[l]
		hist.prices.close = all.hist.prices.close[[l]]
		hist.prices.open = all.hist.prices.open[[l]]
		symb1 = ls(all.data)[i]
		symb2 = ls(all.data)[j]
		
		# backtest at optimal sd.trigger to get statistics
		pairs.trade = bt.pairstrade.longonly(sd.trigger, hist.prices.close, hist.prices.open)
		
		# add row to data.frame df containg output statistics of optimal sd.trigger values
		df[l,] = c(
				ls(all.data)[i], ls(all.data)[j],	pairs.trade$sd.trigger, sum(pairs.trade$cagr), 
				max(pairs.trade$best), min(pairs.trade$worst), min(pairs.trade$equity), max(pairs.trade$equity), 
				cor(hist.prices.open[,1], hist.prices.open[,2]), cointegration(hist.prices.open[,1], hist.prices.open[,2]), 
				cor(hist.prices.close[,1], hist.prices.close[,2]), cointegration(hist.prices.close[,1], hist.prices.close[,2])
		)
		l = l + 1
	}
}


write.csv(df, "results.csv")












######################################################################################

# OLD BEFORE OPTIMIZATION

# backtest the long only pairs trade
cagr = c()
best = c()
worst = c()
min.equity = c()
max.equity = c()
sequence = seq(0.1, 4, 0.1)
for ( sd.trigger in sequence ) {
	#sd.trigger = 1
	pairs.trade = bt.pairstrade.longonly(sd.trigger, hist.prices.close, hist.prices.open)
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




