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


# Alpha for used for CVaR and CDaR
# http://www.investopedia.com/articles/04/092904.asp
ia$parameters.alpha = 0.95

# create efficient frontier(s)
ef.risk = 		portopt(ia, constraints, 50, 'Risk')
ef.cvar = 		portopt(ia, constraints, 50, 'CVaR', 	min.cvar.portfolio)
ef.cdar = 		portopt(ia, constraints, 50, 'CDaR', 	min.cdar.portfolio)

# Plot multiple Efficient Frontiers
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk, ef.cvar, ef.cdar), portfolio.risk, F)	
plot.ef(ia, list(ef.risk, ef.cvar, ef.cdar), portfolio.cvar, F)	
plot.ef(ia, list(ef.risk, ef.cvar, ef.cdar), portfolio.cdar, F)	

# Plot multiple Transition Maps
layout( matrix(1:4, nrow = 2) )
plot.transition.map(ef.risk)
plot.transition.map(ef.cvar)
plot.transition.map(ef.cdar)







min.cvar.portfolio <- function
		(
		ia,		# input assumptions
		constraints	# constraints
)
{
	n = ia$n
	nt = nrow(ia$hist.returns)
	
	alpha = ia$parameters.alpha
	
	# objective : Conditional Value at Risk (CVaR)
	#  E + 1/(1-a) * 1/T * [ SUM  w.j ]
	f.obj = c( rep(0, n), (1/(1-alpha))* (1/nt) * rep(1, nt), 1 )
	
	# adjust constraints, add w.j, E
	constraints = add.variables(nt + 1, constraints, lb = c(rep(0,nt),-Inf))
	
	#  -E - [ SUM <over i> r.ij * x.i ] < w.j, for each j = 1,...,T 
	a = rbind( matrix(0, n, nt), diag(nt), 1)
	a[1 : n, ] = t(ia$hist.returns)
	constraints = add.constraints(a, rep(0, nt), '>=', constraints)			
	
	# setup linear programming	
	f.con = constraints$A
	f.dir = c(rep('=', constraints$meq), rep('>=', len(constraints$b) - constraints$meq))
	f.rhs = constraints$b
	
	# find optimal solution
	x = NA
	sol = try(solve.LP.bounds('min', f.obj, t(f.con), f.dir, f.rhs, 
					lb = constraints$lb, ub = constraints$ub), TRUE)
	
	if(!inherits(sol, 'try-error')) {
		x = sol$solution[1:n]
		
	}
	
	return( x )
}

min.cdar.portfolio <- function
		(
		ia,		# input assumptions
		constraints	# constraints
)
{
	n = ia$n
	nt = nrow(ia$hist.returns)
	
	alpha = ia$parameters.alpha
	
	# objective : Conditional Drawdown at Risk (CDaR)
	#  E + 1/(1-a) * 1/T * [ SUM  w.j ]
	f.obj = c( rep(0, n), (1/(1-alpha))* (1/nt) * rep(1, nt), 1, rep(0, nt) )
	
	# adjust constraints, add w.j, E, u.j
	constraints = add.variables(2*nt + 1, constraints, lb = c(rep(0,nt), rep(-Inf,nt+1)))
	
	#  u.j - [ SUM <over i> [ SUM <over j> r.ij ] * x.i ] - E < w.j, for each j = 1,...,T 
	a = rbind( matrix(0, n, nt), diag(nt), 1, -diag(nt))
	a[1 : n, ] = t(apply( t(ia$hist.returns), 1, cumsum))	
	constraints = add.constraints(a, rep(0, nt), '>=', constraints)					
	
	#  [ SUM <over i> [ SUM <over j> r.ij ] * x.i ] < u.j, for each j = 1,...,T 
	a = rbind( matrix(0, n, nt), 0*diag(nt), 0, diag(nt))
	a[1 : n, ] = -t(apply( t(ia$hist.returns), 1, cumsum))
	constraints = add.constraints(a, rep(0, nt), '>=', constraints)
	
	#  u.j-1 < u.j, for each j = 1,...,T - portfolio high water mark is increasing		
	temp = diag(nt);
	temp[-nt,-1]=-diag((nt-1))
	diag(temp) = 1			
	
	a = rbind( matrix(0, n, nt), 0*diag(nt), 0, temp)
	a = a[,-1]		
	constraints = add.constraints(a, rep(0, (nt-1)), '>=', constraints)
	
	# setup linear programming	
	f.con = constraints$A
	f.dir = c(rep('=', constraints$meq), rep('>=', len(constraints$b) - constraints$meq))
	f.rhs = constraints$b
	
	# find optimal solution
	x = NA
	sol = try(solve.LP.bounds('min', f.obj, t(f.con), f.dir, f.rhs, 
					lb = constraints$lb, ub = constraints$ub), TRUE)
	
	if(!inherits(sol, 'try-error')) {
		x = sol$solution[1:n]
		
	}
	
	return( x )
}
