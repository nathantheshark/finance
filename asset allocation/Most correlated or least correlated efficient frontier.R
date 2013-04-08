# Load Systematic Investor Toolbox (SIT)
setInternet2(TRUE)
con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
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


# create efficient frontier(s)
ef.risk = portopt(ia, constraints, 50, 'Risk')
ef.cor.insteadof.cov = portopt(ia, constraints, 50, 'Cor instead of Cov', min.cor.insteadof.cov.portfolio)
ef.avgcor = portopt(ia, constraints, 50, 'AvgCor', min.avgcor.portfolio)

# Plot multiple Efficient Frontiers	
layout(1:2)
plot.ef(ia, list(ef.risk, ef.avgcor, ef.cor.insteadof.cov), portfolio.risk, F)	
plot.ef(ia, list(ef.risk, ef.avgcor, ef.cor.insteadof.cov), portfolio.avgcor, F)	

# Plot multiple Transition Maps	
layout( matrix(1:4, nrow = 2) )
plot.transition.map(ef.risk)
plot.transition.map(ef.avgcor)
plot.transition.map(ef.cor.insteadof.cov)

# visualize input assumptions
plot.ia(ia)
