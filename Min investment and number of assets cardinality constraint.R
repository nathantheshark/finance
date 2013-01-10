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
symbols = spl('SPY,VGTSX,VISGX,SDY,VEIEX,ACM,CAT,F,GEX')
#symbol.names = spl('S&P 500,Nasdaq 100,Emerging Markets,Russell 2000,EAFE,20 Year
#Treasury,U.S. Real Estate,Gold')
symbol.names = spl('S&P 500,Total Intl Stock Index,Small Cap Growth Index,S&P Dividend,Emerging Mkts Stock Idx,AECOM Technology,Caterpillar,Ford,Glb Alternatve Energy ETF')

getSymbols(symbols, from = '1980-01-01', auto.assign = TRUE)

# change F to FRD (F is systeam variable for FALSE and using it causes errors)
FRD = F
rm(F)

# align dates for all symbols & convert to monthly
hist.prices = merge(SPY,VGTSX,VISGX,SDY,VEIEX,ACM,CAT,FRD,GEX)
month.ends = endpoints(hist.prices, 'months')
hist.prices = Cl(hist.prices)[month.ends, ]
colnames(hist.prices) = symbols

# remove any missing data
hist.prices = na.omit(hist.prices['1995::2010'])

# compute simple returns
hist.returns = na.omit( ROC(hist.prices, type = 'discrete') )

###############################################################################

# compute historical returns, risk, and correlation
ia = list()
ia$expected.return = apply(hist.returns, 2, mean, na.rm = T)
ia$risk = apply(hist.returns, 2, sd, na.rm = T)
ia$correlation = cor(hist.returns, use = 'complete.obs', method = 'pearson')

ia$symbols = symbols
ia$symbol.names = symbol.names
ia$n = len(symbols)
ia$hist.returns = hist.returns

# convert to annual, year = 12 months
annual.factor = 12
ia$expected.return = annual.factor * ia$expected.return
ia$risk = sqrt(annual.factor) * ia$risk

# compute covariance matrix
ia$risk = iif(ia$risk == 0, 0.000001, ia$risk)
ia$cov = ia$cor * (ia$risk %*% t(ia$risk))

###############################################################################

#--------------------------------------------------------------------------
# Create Efficient Frontier
#--------------------------------------------------------------------------
ia = aa.test.create.ia()
n = ia$n		

# 0 <= x.i <= 0.8 
constraints = new.constraints(n, lb = 0, ub = 0.8)

# SUM x.i = 1
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		

# create efficient frontier(s)
ef.risk = portopt(ia, constraints, 50, 'Risk')
ef.mad = portopt(ia, constraints, 50, 'MAD', min.mad.portfolio)

# Plot range of portfolio weights and number of assets in each portfolio on efficient frontier
layout(1:2)
par(mar = c(4,4,2,1), cex = 0.8)
y = iif(ef.risk$weight > 0.000001, ef.risk$weight, NA) 
plot(as.vector(sort(100 * y)), pch=20, xaxt='n', ylim = c(0, 80),
		xlab='', ylab='Weight', main='Portfolio Weights')
abline(h=0, col = 'red')
abline(h=10, col = 'red')

plot(100* ef.risk$risk, rowSums(!is.na(y), na.rm = T), pch=20, type='b', 
		xlab='Risk', ylab='# Assets', main='Number of Assets')

# Plot multiple Efficient Frontiers & Transition Maps
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)	
plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)	

plot.transition.map(ef.risk)
plot.transition.map(ef.mad)