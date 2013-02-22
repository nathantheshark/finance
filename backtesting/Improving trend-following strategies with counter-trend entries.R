# Load Systematic Investor Toolbox (SIT)
setInternet2(TRUE)
con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
source(con)
close(con)

#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod')
tickers = spl('SPY')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
bt.prep(data, align='keep.all', dates='1970::2011')

#*****************************************************************
# Code Strategies
#****************************************************************** 
prices = data$prices    

# Buy & Hold	
data$weight[] = 1
buy.hold = bt.run(data)	

# Trend-Following strategy: Long[Close > SMA(10) ]
sma = bt.apply(data, function(x) { SMA(Cl(x), 10) } )	
data$weight[] = NA
data$weight[] = iif(prices >= sma, 1, 0)
trend.following = bt.run(data, trade.summary=T)			

# Trend-Following With Counter-Trend strategy: Long[Close > SMA(10), DVB(1) CounterTrend ]
dv = bt.apply(data, function(x) { DV(HLC(x), 1, TRUE) } )	
data$weight[] = NA
data$weight[] = iif(prices > sma & dv < 0.25, 1, data$weight)
data$weight[] = iif(prices < sma & dv > 0.75, 0, data$weight)
trend.following.dv1 = bt.run(data, trade.summary=T)			

#*****************************************************************
# Create Report
#****************************************************************** 
plotbt.custom.report(trend.following.dv1, trend.following, buy.hold)	
