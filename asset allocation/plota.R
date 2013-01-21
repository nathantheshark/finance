###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
setInternet2(TRUE)
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

load.packages('quantmod')

# download sample data from Yahoo
data.spy = getSymbols('SPY', from = '1980-01-01', auto.assign = FALSE)
data.ibm = getSymbols('IBM', from = '1980-01-01', auto.assign = FALSE)

y = data.spy['2011:01:01::2011:02:01']
highlight = which(Cl(y) < 127)

png(filename = 'plot1.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')

layout(c(1,1,2))
plota(y, type = 'candle', main = 'SPY', plotX = F, x.highlight = highlight)
y = plota.scale.volume(y)
plota(y, type = 'volume', x.highlight = highlight)

dev.off()

############# Simple chart of SPY with RSI and Legend ###########################

y = data.spy['2010:01:01::2011:02:01']

layout(c(1,1,2,3))
plota(y, type = 'candle', plotX = F)
plota.legend('SPY', 'blue', y)

y = plota.scale.volume(y)
plota(y, type = 'volume', plotX = F)
plota.legend('Volume', 'blue', Vo(y))

rsi = RSI(Cl(y),2)
plota(rsi, type = 'l', y.highlight = c(c(Inf,80),c(20,-Inf)))
abline(h = 20, col = 'red')
abline(h = 80, col = 'red')
plota.legend('RSI(2)', 'black', rsi)

########### Second y-axis #######################################################

y = data.spy['2010:01:01::2011:02:01']

# to plot second Y axis, free some space on left side
# e.g. set LeftMargin=3
plota(y, type = 'ohlc', LeftMargin=3)

y0 = y;
y = data.ibm['2010:10:15::2011:02:01']
plota2Y(y, ylim = range(OHLC(y)),las=1, col='red', col.axis = 'red')
plota.ohlc(y, col = 'red')
plota.legend('SPY(rhs),IBM(lhs)', 'blue,red', list(y0,y))

######### Daily and Monthly on the same plot ###################################

y = data.spy['2010:01:01::2011:02:01']

plota(y, type = 'candle')
y1 = to.monthly(y)
index(y1) = as.Date(index(y1))
plota.ohlc(y1, col = 'pink')
plota.candle(y)
plota.legend('Daily,Monthly', 'red,pink')

######### Daily, Weekly, and Monthly on the same plot ##########################

y = data.spy['2010:01:01::2011']

layout(c(1,2,3))
plota(y, type = 'candle', plotX = F)
plota.legend('Daily', 'blue', y)

plota(y, ylim = range(OHLC(y)), plotX = F)
y1 = to.weekly(y)
index(y1) = as.Date(index(y1))
plota.candle(y1)
plota.legend('Weekly', 'blue', y1)

plota(y, ylim = range(OHLC(y)))
y1 = to.monthly(y)
index(y1) = as.Date(index(y1))
plota.candle(y1)
plota.legend('Monthly', 'blue', y1)



