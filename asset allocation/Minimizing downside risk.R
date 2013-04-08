# load Systematic Investor Toolbox
setInternet2(TRUE)
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)
load.packages('quantmod')

#--------------------------------------------------------------------------
# Create Efficient Frontier
#--------------------------------------------------------------------------
ia = aa.test.create.ia()
n = ia$n		

# 0 <= x.i <= 0.8
constraints = new.constraints(n, lb = 0, ub = 0.8)

# SUM x.i = 1
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		

# Set target return (or Minimum Acceptable Returns (MAR))
# and consider only returns that are less than the target 
ia$parameters.mar = 0/100 
# convert annual to monthly
ia$parameters.mar = ia$parameters.mar / 12


# create efficient frontier(s)
ef.mad = portopt(ia, constraints, 50, 'MAD', min.mad.portfolio)
ef.mad.downside = portopt(ia, constraints, 50, 'S-MAD', min.mad.downside.portfolio)

ef.risk = portopt(ia, constraints, 50, 'Risk')
ef.risk.downside = portopt(ia, constraints, 50, 'S-Risk', min.risk.downside.portfolio)


# Plot multiple Efficient Frontiers and Transition Maps
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.mad.downside, ef.mad), portfolio.mad, F)			
plot.ef(ia, list(ef.mad.downside, ef.mad), portfolio.mad.downside, F)			

plot.transition.map(ef.mad)
plot.transition.map(ef.mad.downside)

# Plot multiple Efficient Frontiers and Transition Maps
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk.downside, ef.risk), portfolio.risk, F)			
plot.ef(ia, list(ef.risk.downside, ef.risk), portfolio.risk.downside, F)			

plot.transition.map(ef.risk)
plot.transition.map(ef.risk.downside)
