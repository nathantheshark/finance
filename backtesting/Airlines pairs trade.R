###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
setInternet2(TRUE)
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

load.packages('quantmod')

# load historical prices from Yahoo Finance
#symbols = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')
symbols = spl('AAMRQ,DAL,UAL,LCC,ALK,LUV,JBLU')
#symbol.names = spl('S&P 500,Nasdaq 100,Emerging Markets,Russell 2000,EAFE,20 Year
#Treasury,U.S. Real Estate,Gold')
symbol.names = spl('American,Delta,United,US Air, Alaska, Southwest, JetBlue')

getSymbols(symbols, from = '1980-01-01', auto.assign = TRUE)

hist.prices = merge(AAMRQ,DAL,UAL,LCC,ALK,LUV,JBLU)
hist.prices.close = Cl(hist.prices)
colnames(hist.prices.close) = symbols
hist.prices.open = Op(hist.prices)
colnames(hist.prices.open) = symbols

# remove any missing data
hist.prices.close = na.omit(hist.prices.close)
hist.prices.open = na.omit(hist.prices.open)

# compute simple returns
hist.returns.close = ROC(hist.prices.close, type = 'discrete')
hist.returns.close[1,] = 0
hist.returns.open = ROC(hist.prices.open, type = 'discrete')
hist.returns.open[1,] = 0

# create matrix of spreads
spread = c()
spread.colnames = c()
df.hist.prices.close = data.frame(hist.prices.close)
for (i in 1:(length(ls(df.hist.prices.close))-1)) {
	for (j in (i+1):length(ls(df.hist.prices.close))) {
		spread = c(spread, df.hist.prices.close[[ls(df.hist.prices.close[i])]] - df.hist.prices.close[[ls(df.hist.prices.close[j])]])
		concat.colnames = paste(ls(df.hist.prices.close[i]), "-", ls(df.hist.prices.close[i]))
		spread.colnames = c(spread.colnames, concat.colnames)
	}
}
spread = matrix(spread, nrow=nrow(df.hist.prices.close), ncol = (6+5+4+3+2+1))		# convert to matrix
colnames(spread) = spread.colnames