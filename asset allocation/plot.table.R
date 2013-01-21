###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
setInternet2(TRUE)
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

############# Create a basic plot.table ########################################

# define row and column titles
mrownames = spl('row one,row two,row 3')
mcolnames = spl('col 1,col 2,col 3,col 4')

# create temp matrix with data you want to plot
temp = matrix(NA, len(mrownames), len(mcolnames))
rownames(temp) = mrownames
colnames(temp) = mcolnames
temp[,] = matrix(1:12,3,4)

# plot temp, display current date in (top, left) cell
plot.table(temp, format(as.Date(Sys.time()), '%d %b %Y'))

############# Create a plot.table with colorbar ################################

# generate 1,000 random numbers from Normal(0,1) distribution
data =  matrix(rnorm(1000), nc=10)
colnames(data) = paste('data', 1:10, sep='')

# compute Pearson correlation of data and format it nicely
temp = compute.cor(data, 'pearson')
temp[] = plota.format(100 * temp, 0, '', '%')

# plot temp with colorbar, display Correlation in (top, left) cell
plot.table(temp, smain='Correlation', highlight = TRUE, colorbar = TRUE)

######## Create report showing IBM Key Statistics from Yahoo! Finance #########

# Load quantmod package to download price history for IBM
load.packages('quantmod')

Symbol = 'IBM'

# download IBM price history from Yahoo
data = getSymbols(Symbol, from = '1980-01-01', auto.assign = FALSE)

# download Key Statistics from yahoo
url = paste('http://finance.yahoo.com/q/ks?s=', Symbol, sep = '')
txt = join(readLines(url))

# extract Valuation Measures table from this page
temp = extract.table.from.webpage(txt, 'Market Cap', hasHeader = F)
temp = rbind(c('', Symbol), temp)	# add header row

# prepare IBM data for 2010:2011 and compute 50 days moving average
y = data['2010::2011']
sma50 = SMA(Cl(y), 50)

# plote candles and volume and table
layout(c(1,1,2,3,3))

plota(y, type = 'candle', main = Symbol, plotX = F)
plota.lines(sma50, col='blue')
plota.legend(c(Symbol,'SMA 50'), 'green,blue', list(y,sma50))

y = plota.scale.volume(y)
plota(y, type = 'volume')

plot.table(temp)