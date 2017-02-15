ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
    submodel = NULL, external.regressors = NULL, variance.targeting = FALSE),
    mean.model = list(armaOrder = c(1, 1), include.mean = TRUE, archm = FALSE,
    archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE),
    distribution.model = "norm", start.pars = list(), fixed.pars = list(), ...)
    
distribution.model
The conditional density to use for the innovations. Valid choices are “norm” for the normal distibution, “snorm” for the skew-normal distribution, “std” for the student-t, “sstd” for the skew-student, “ged” for the generalized error distri- bution, “sged” for the skew-generalized error distribution, “nig” for the normal inverse gaussian distribution, “ghyp” for the Generalized Hyperbolic, and “jsu” for Johnson’s SU distribution. Note that some of the distributions are taken from the fBasics package and implenented locally here for convenience. The “jsu” distribution is the reparametrized version from the “gamlss” package.

norm 		#Normal
snorm		#skew-normal
std		#student-t
sstd		#skew-student
ged		#generalized error distribution
sged		#skew-generalized error distribuzion
nig		#normal inverse gaussian
ghyp		#generalized hyperbolic



# GARCH
# rolling AR(1)-GARCH(1,1) - Normal with VaR violations
garch11.spec=ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1),submodel = "GARCH"), 
                     mean.model=list(armaOrder=c(1,0), include.mean=TRUE), 
                     distribution.model="norm")
#verificare la differenza di stima senza submodel="GARCH"

STOCK.garch11.roll = ugarchroll(garch11.spec, STOCK.ret, n.ahead=1,
                                forecast.length = w.t,
                                refit.every=1, refit.window="moving",
                                window.size=w.e,
                                calculate.VaR=TRUE, VaR.alpha=(1-alpha))




# IGARCH
# rolling AR(1)-IGARCH(1,1) - Normal with VaR violations
igarch11.spec=ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)), 
                     	 mean.model=list(armaOrder=c(1,0), include.mean=TRUE),
                     	 distribution.model="norm")

STOCK.igarch11.roll = ugarchroll(igarch11.spec, STOCK.ret, n.ahead=1,
                                 forecast.length = w.t,
                                 refit.every=1, refit.window="moving",
                                 window.size=w.e,
                                 calculate.VaR=TRUE, VaR.alpha=(1-alpha))
                               
                         
                         
                              

# APARCH
# rolling AR(1)-APARCH(1,1) - Normal with VaR violations
aparch11.spec=ugarchspec(variance.model=list(model="apARCH", garchOrder=c(1,1)), 
                     	 mean.model=list(armaOrder=c(1,0), include.mean=TRUE),
                     	 distribution.model="norm")

STOCK.aparch11.roll = ugarchroll(aparch11.spec, STOCK.ret, n.ahead=1,
                                 forecast.length = w.t,
                                 refit.every=1, refit.window="moving",
                                 window.size=w.e,
                                 calculate.VaR=TRUE, VaR.alpha=(1-alpha))


               
               
               
# FGARCH
# rolling AR(1)-FGARCH(1,1) - Normal with VaR violations
fgarch11.spec=ugarchspec(variance.model=list(model="fGARCH", garchOrder=c(1,1), submodel = "ALLGARCH"), 
                     	 mean.model=list(armaOrder=c(1,0), include.mean=TRUE),
                     	 distribution.model="norm")

STOCK.fgarch11.roll = ugarchroll(fgarch11.spec, STOCK.ret, n.ahead=1,
                                 forecast.length = w.t,
                                 refit.every=1, refit.window="moving",
                                 window.size=w.e,
                                 calculate.VaR=TRUE, VaR.alpha=(1-alpha))


















                              
                          
                               