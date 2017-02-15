########################################################################
# Legnaro Giacomo
# Universita' degli Studi di Padova
# Statistica Economia e Finanza
# - Indirizzo: Finanza
# - Percorso: Metodologico
# AA 2014-2015
########################################################################
# Value-at-Risk Prediction
# Matricola : 1052299
########################################################################

#install.packages("Rsolnp", repos="http://R-Forge.R-project.org") 
#require(devtools) 
#install_bitbucket("rugarch","alexiosg") 
#library(Rsolnp)
#library(rugarch)
# VaR models
########################################################################

# GARCH
# rolling AR(1)-GARCH(1,1) - Normal with VaR violations
VaR.argarch =  function(R,p,d="norm")
{
  #R=as.zoo(STOCK.ret)
  #x=as.zoo(STOCK.ret)
  #d="sstd"
  #p=alpha
  alpha = .setalphaprob(p)
  
  r = as.vector(na.omit(R))
  if (!is.numeric(r)) stop("The selected column is not numeric") 
  garch11.spec=ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                          mean.model=list(armaOrder=c(1,0), include.mean=TRUE), 
                          distribution.model=d)
  #verificare la differenza di stima senza submodel="GARCH"
  STOCK.garch11.roll = ugarchroll(garch11.spec, r, n.ahead=1,
                                  forecast.length = w.t,
                                  refit.every=1, refit.window="moving",
                                  window.size=w.e, 
                                  #solver = "hybrid",
                                  #solver="solnp",
                                  #solver="nlminb",
                                  #solver="gosolnp",
                                  #solver="nloptr",
                                  #solver = 'msoptim', solver.control = list(restarts = 10),
                                  solver.control=list(tol=1e-6, delta=1e-7, n.restarts=3, pkg="multicore", cores=4), 
                                  calculate.VaR=TRUE, VaR.alpha=alpha,
                                  #aggiunto conf.level per la significatività
                                  #se non va basta
                                  conf.level=(1-alpha))

  STOCK.garch11.roll = resume(STOCK.garch11.roll, solver="hybrid", solver.control=list(tol=1e-6, delta=1e-7, n.restarts=3, pkg="multicore", cores=4)) 
  STOCK.garch11.roll = resume(STOCK.garch11.roll, solver="solnp", solver.control=list(tol=1e-6, delta=1e-7, n.restarts=3, pkg="multicore", cores=4)) 
  STOCK.garch11.roll = resume(STOCK.garch11.roll, solver="nlminb", solver.control=list(tol=1e-6, delta=1e-7, n.restarts=3, pkg="multicore", cores=4))
  STOCK.garch11.roll = resume(STOCK.garch11.roll, solver="gosolnp", solver.control=list(tol=1e-6, delta=1e-7, n.restarts=3, pkg="multicore", cores=4))
  STOCK.garch11.roll = resume(STOCK.garch11.roll, solver="nloptr", solver.control=list(tol=1e-6, delta=1e-7, n.restarts=3, pkg="multicore", cores=4))
  STOCK.garch11.roll = resume(STOCK.garch11.roll, solver="msoptim", solver.control=list(tol=1e-6, delta=1e-7, n.restarts=3, pkg="multicore", cores=4)) 
                                
  STOCK.garch11.roll = resume(STOCK.garch11.roll, solver="hybrid", solver.control=list(tol=1e-5, delta=1e-6, n.restarts=3, pkg="multicore", cores=4)) 
  STOCK.garch11.roll = resume(STOCK.garch11.roll, solver="solnp", solver.control=list(tol=1e-5, delta=1e-6, n.restarts=3, pkg="multicore", cores=4)) 
  STOCK.garch11.roll = resume(STOCK.garch11.roll, solver="nlminb", solver.control=list(tol=1e-5, delta=1e-6, n.restarts=3, pkg="multicore", cores=4))
  STOCK.garch11.roll = resume(STOCK.garch11.roll, solver="gosolnp", solver.control=list(tol=1e-5, delta=1e-6, n.restarts=3, pkg="multicore", cores=4))
  STOCK.garch11.roll = resume(STOCK.garch11.roll, solver="nloptr", solver.control=list(tol=1e-5, delta=1e-6, n.restarts=3, pkg="multicore", cores=4))
  STOCK.garch11.roll = resume(STOCK.garch11.roll, solver="msoptim", solver.control=list(tol=1e-5, delta=1e-6, n.restarts=3, pkg="multicore", cores=4)) 
  
  STOCK.garch11.roll = resume(STOCK.garch11.roll, solver="hybrid", solver.control=list(tol=1e-4, delta=1e-5, n.restarts=3, pkg="multicore", cores=4)) 
  STOCK.garch11.roll = resume(STOCK.garch11.roll, solver="solnp", solver.control=list(tol=1e-4, delta=1e-5, n.restarts=3, pkg="multicore", cores=4)) 
  STOCK.garch11.roll = resume(STOCK.garch11.roll, solver="nlminb", solver.control=list(tol=1e-4, delta=1e-5, n.restarts=3, pkg="multicore", cores=4))
  STOCK.garch11.roll = resume(STOCK.garch11.roll, solver="gosolnp", solver.control=list(tol=1e-4, delta=1e-5, n.restarts=3, pkg="multicore", cores=4))
  STOCK.garch11.roll = resume(STOCK.garch11.roll, solver="nloptr", solver.control=list(tol=1e-4, delta=1e-5, n.restarts=3, pkg="multicore", cores=4))
  STOCK.garch11.roll = resume(STOCK.garch11.roll, solver="msoptim", solver.control=list(tol=1e-4, delta=1e-5, n.restarts=3, pkg="multicore", cores=4)) 
  

  if (alpha == "0.01") {
    rVaR=as.data.frame(STOCK.garch11.roll@forecast$VaR$"alpha(1%)")
  }
  if (alpha == "0.025") {
    rVaR=as.data.frame(STOCK.garch11.roll@forecast$VaR$"alpha(3%)")
  }
  if (alpha == "0.05") {
    rVaR=as.data.frame(STOCK.garch11.roll@forecast$VaR$"alpha(5%)")
  }
  
  VaR=array(rVaR)
  result=data.frame(VaR=VaR)
  #str(result)
  #result = as.matrix(rVaR)
  #forecast_mu=STOCK.garch11.roll@forecast$density$Mu
  colnames(result)<-colnames(R)
  result=zoo(result, as.Date(index(R))[(w.e+1):n.obs])
  return(result)
}




# GARCH
# rolling AR(1)-IGARCH(1,1) - Normal with VaR violations
VaR.arigarch =  function(R,p,d="norm")
{
  #R=as.zoo(STOCK.ret)
  #x=as.zoo(STOCK.ret)
  #d="sstd"
  #p=alpha
  alpha = .setalphaprob(p)
  
  r = as.vector(na.omit(R))
  if (!is.numeric(r)) stop("The selected column is not numeric") 
  igarch11.spec=ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)), 
                          mean.model=list(armaOrder=c(1,0), include.mean=TRUE), 
                          distribution.model=d)
  #verificare la differenza di stima senza submodel="GARCH"
  STOCK.igarch11.roll = ugarchroll(igarch11.spec, r, n.ahead=1,
                                  forecast.length = w.t,
                                  refit.every=1, refit.window="moving",
                                  window.size=w.e, 
                                  #solver = "hybrid",
                                  #solver="solnp",
                                  #solver="nlminb",
                                  #solver="gosolnp",
                                  #solver="nloptr",
                                  #solver = 'msoptim', solver.control = list(restarts = 10),
                                  solver.control=list(tol=1e-6, delta=1e-7, n.restarts=3, pkg="multicore", cores=4), 
                                  calculate.VaR=TRUE, VaR.alpha=alpha,
                                  #aggiunto conf.level per la significatività
                                  #se non va basta
                                  conf.level=(1-alpha))
                                  
  STOCK.igarch11.roll = resume(STOCK.igarch11.roll, solver="hybrid", solver.control=list(tol=1e-6, delta=1e-7, n.restarts=3, pkg="multicore", cores=4)) 
  STOCK.igarch11.roll = resume(STOCK.igarch11.roll, solver="solnp", solver.control=list(tol=1e-6, delta=1e-7, n.restarts=3, pkg="multicore", cores=4)) 
  STOCK.igarch11.roll = resume(STOCK.igarch11.roll, solver="nlminb", solver.control=list(tol=1e-6, delta=1e-7, n.restarts=3, pkg="multicore", cores=4))
  STOCK.igarch11.roll = resume(STOCK.igarch11.roll, solver="gosolnp", solver.control=list(tol=1e-6, delta=1e-7, n.restarts=3, pkg="multicore", cores=4))
  STOCK.igarch11.roll = resume(STOCK.igarch11.roll, solver="nloptr", solver.control=list(tol=1e-6, delta=1e-7, n.restarts=3, pkg="multicore", cores=4))
  STOCK.igarch11.roll = resume(STOCK.igarch11.roll, solver="msoptim", solver.control=list(tol=1e-6, delta=1e-7, n.restarts=3, pkg="multicore", cores=4)) 
                                  
  STOCK.igarch11.roll = resume(STOCK.igarch11.roll, solver="hybrid", solver.control=list(tol=1e-5, delta=1e-6, n.restarts=3, pkg="multicore", cores=4)) 
  STOCK.igarch11.roll = resume(STOCK.igarch11.roll, solver="solnp", solver.control=list(tol=1e-5, delta=1e-6, n.restarts=3, pkg="multicore", cores=4)) 
  STOCK.igarch11.roll = resume(STOCK.igarch11.roll, solver="nlminb", solver.control=list(tol=1e-5, delta=1e-6, n.restarts=3, pkg="multicore", cores=4))
  STOCK.igarch11.roll = resume(STOCK.igarch11.roll, solver="gosolnp", solver.control=list(tol=1e-5, delta=1e-6, n.restarts=3, pkg="multicore", cores=4))
  STOCK.igarch11.roll = resume(STOCK.igarch11.roll, solver="nloptr", solver.control=list(tol=1e-5, delta=1e-6, n.restarts=3, pkg="multicore", cores=4))
  STOCK.igarch11.roll = resume(STOCK.igarch11.roll, solver="msoptim", solver.control=list(tol=1e-5, delta=1e-6, n.restarts=3, pkg="multicore", cores=4)) 
  
  STOCK.igarch11.roll = resume(STOCK.igarch11.roll, solver="hybrid", solver.control=list(tol=1e-4, delta=1e-5, n.restarts=3, pkg="multicore", cores=4)) 
  STOCK.igarch11.roll = resume(STOCK.igarch11.roll, solver="solnp", solver.control=list(tol=1e-4, delta=1e-5, n.restarts=3, pkg="multicore", cores=4)) 
  STOCK.igarch11.roll = resume(STOCK.igarch11.roll, solver="nlminb", solver.control=list(tol=1e-4, delta=1e-5, n.restarts=3, pkg="multicore", cores=4))
  STOCK.igarch11.roll = resume(STOCK.igarch11.roll, solver="gosolnp", solver.control=list(tol=1e-4, delta=1e-5, n.restarts=3, pkg="multicore", cores=4))
  STOCK.igarch11.roll = resume(STOCK.igarch11.roll, solver="nloptr", solver.control=list(tol=1e-4, delta=1e-5, n.restarts=3, pkg="multicore", cores=4))
  STOCK.igarch11.roll = resume(STOCK.igarch11.roll, solver="msoptim", solver.control=list(tol=1e-4, delta=1e-5, n.restarts=3, pkg="multicore", cores=4)) 
  
  if (alpha == "0.01") {
    rVaR=as.data.frame(STOCK.igarch11.roll@forecast$VaR$"alpha(1%)")
  }
  if (alpha == "0.025") {
    rVaR=as.data.frame(STOCK.igarch11.roll@forecast$VaR$"alpha(3%)")
  }
  if (alpha == "0.05") {
    rVaR=as.data.frame(STOCK.igarch11.roll@forecast$VaR$"alpha(5%)")
  }
  
  VaR=array(rVaR)
  result=data.frame(VaR=VaR)
  #str(result)
  #result = as.matrix(rVaR)
  #forecast_mu=STOCK.igarch11.roll@forecast$density$Mu
  colnames(result)<-colnames(R)
  result=zoo(result, as.Date(index(R))[(w.e+1):n.obs])
  return(result)
}




# GARCH
# rolling AR(1)-APARCH(1,1) - Normal with VaR violations
VaR.araparch =  function(R,p,d="norm")
{
  #R=as.zoo(STOCK.ret)
  #x=as.zoo(STOCK.ret)
  #d="sstd"
  #p=alpha
  alpha = .setalphaprob(p)
  
  r = as.vector(na.omit(R))
  if (!is.numeric(r)) stop("The selected column is not numeric") 
  aparch11.spec=ugarchspec(variance.model=list(model="apARCH", garchOrder=c(1,1)), 
                           mean.model=list(armaOrder=c(1,0), include.mean=TRUE), 
                           distribution.model=d)
  #verificare la differenza di stima senza submodel="GARCH"
  STOCK.aparch11.roll = ugarchroll(aparch11.spec, r, n.ahead=1,
                                   forecast.length = w.t,
                                   refit.every=1, refit.window="moving",
                                   window.size=w.e, 
                                   #solver = "hybrid",
                                   #solver="solnp",
                                   #solver="nlminb",
                                   #solver="gosolnp",
                                   #solver="nloptr",
                                   #solver = 'msoptim', solver.control = list(restarts = 10),
                                   solver.control=list(tol=1e-6, delta=1e-7, n.restarts=3, pkg="multicore", cores=4), 
                                   calculate.VaR=TRUE, VaR.alpha=alpha,
                                   #aggiunto conf.level per la significatività
                                   #se non va basta
                                   conf.level=(1-alpha))
  
  STOCK.aparch11.roll = resume(STOCK.aparch11.roll, solver="hybrid", solver.control=list(tol=1e-6, delta=1e-7, n.restarts=3, pkg="multicore", cores=4)) 
  STOCK.aparch11.roll = resume(STOCK.aparch11.roll, solver="solnp", solver.control=list(tol=1e-6, delta=1e-7, n.restarts=3, pkg="multicore", cores=4)) 
  STOCK.aparch11.roll = resume(STOCK.aparch11.roll, solver="nlminb", solver.control=list(tol=1e-6, delta=1e-7, n.restarts=3, pkg="multicore", cores=4))
  STOCK.aparch11.roll = resume(STOCK.aparch11.roll, solver="gosolnp", solver.control=list(tol=1e-6, delta=1e-7, n.restarts=3, pkg="multicore", cores=4))
  STOCK.aparch11.roll = resume(STOCK.aparch11.roll, solver="nloptr", solver.control=list(tol=1e-6, delta=1e-7, n.restarts=3, pkg="multicore", cores=4))
  STOCK.aparch11.roll = resume(STOCK.aparch11.roll, solver="msoptim", solver.control=list(tol=1e-6, delta=1e-7, n.restarts=3, pkg="multicore", cores=4)) 
  
  STOCK.aparch11.roll = resume(STOCK.aparch11.roll, solver="hybrid", solver.control=list(tol=1e-5, delta=1e-6, n.restarts=3, pkg="multicore", cores=4)) 
  STOCK.aparch11.roll = resume(STOCK.aparch11.roll, solver="solnp", solver.control=list(tol=1e-5, delta=1e-6, n.restarts=3, pkg="multicore", cores=4)) 
  STOCK.aparch11.roll = resume(STOCK.aparch11.roll, solver="nlminb", solver.control=list(tol=1e-5, delta=1e-6, n.restarts=3, pkg="multicore", cores=4))
  STOCK.aparch11.roll = resume(STOCK.aparch11.roll, solver="gosolnp", solver.control=list(tol=1e-5, delta=1e-6, n.restarts=3, pkg="multicore", cores=4))
  STOCK.aparch11.roll = resume(STOCK.aparch11.roll, solver="nloptr", solver.control=list(tol=1e-5, delta=1e-6, n.restarts=3, pkg="multicore", cores=4))
  STOCK.aparch11.roll = resume(STOCK.aparch11.roll, solver="msoptim", solver.control=list(tol=1e-5, delta=1e-6, n.restarts=3, pkg="multicore", cores=4)) 
  
  STOCK.aparch11.roll = resume(STOCK.aparch11.roll, solver="hybrid", solver.control=list(tol=1e-4, delta=1e-5, n.restarts=3, pkg="multicore", cores=4)) 
  STOCK.aparch11.roll = resume(STOCK.aparch11.roll, solver="solnp", solver.control=list(tol=1e-4, delta=1e-5, n.restarts=3, pkg="multicore", cores=4)) 
  STOCK.aparch11.roll = resume(STOCK.aparch11.roll, solver="nlminb", solver.control=list(tol=1e-4, delta=1e-5, n.restarts=3, pkg="multicore", cores=4))
  STOCK.aparch11.roll = resume(STOCK.aparch11.roll, solver="gosolnp", solver.control=list(tol=1e-4, delta=1e-5, n.restarts=3, pkg="multicore", cores=4))
  STOCK.aparch11.roll = resume(STOCK.aparch11.roll, solver="nloptr", solver.control=list(tol=1e-4, delta=1e-5, n.restarts=3, pkg="multicore", cores=4))
  STOCK.aparch11.roll = resume(STOCK.aparch11.roll, solver="msoptim", solver.control=list(tol=1e-4, delta=1e-5, n.restarts=3, pkg="multicore", cores=4)) 
  
  if (alpha == "0.01") {
    rVaR=as.data.frame(STOCK.aparch11.roll@forecast$VaR$"alpha(1%)")
  }
  if (alpha == "0.025") {
    rVaR=as.data.frame(STOCK.aparch11.roll@forecast$VaR$"alpha(3%)")
  }
  if (alpha == "0.05") {
    rVaR=as.data.frame(STOCK.aparch11.roll@forecast$VaR$"alpha(5%)")
  }
  
  VaR=array(rVaR)
  result=data.frame(VaR=VaR)
  #str(result)
  #result = as.matrix(rVaR)
  #forecast_mu=STOCK.igarch11.roll@forecast$density$Mu
  colnames(result)<-colnames(R)
  result=zoo(result, as.Date(index(R))[(w.e+1):n.obs])
  return(result)
}





# GARCH
# rolling AR(1)-APARCH(1,1) - Normal with VaR violations
VaR.arfgarch =  function(R,p,d="norm")
{
  #R=as.zoo(STOCK.ret)
  #x=as.zoo(STOCK.ret)
  #d="sstd"
  #p=alpha
  alpha = .setalphaprob(p)
  
  r = as.vector(na.omit(R))
  if (!is.numeric(r)) stop("The selected column is not numeric") 
  fgarch11.spec=ugarchspec(variance.model=list(model="fGARCH", garchOrder=c(1,1), submodel = "ALLGARCH"),  
                           mean.model=list(armaOrder=c(1,0), include.mean=TRUE), 
                           distribution.model=d)
  #verificare la differenza di stima senza submodel="GARCH"
  STOCK.fgarch11.roll = ugarchroll(fgarch11.spec, r, n.ahead=1,
                                   forecast.length = w.t,
                                   refit.every=1, refit.window="moving",
                                   window.size=w.e, 
                                   #solver = "hybrid",
                                   #solver="solnp",
                                   #solver="nlminb",
                                   #solver="gosolnp",
                                   #solver="nloptr",
                                   #solver = 'msoptim', solver.control = list(restarts = 10),
                                   solver.control=list(tol=1e-6, delta=1e-7, n.restarts=3, pkg="multicore", cores=4), 
                                   calculate.VaR=TRUE, VaR.alpha=alpha,
                                   #aggiunto conf.level per la significatività
                                   #se non va basta
                                   conf.level=(1-alpha))
  
  STOCK.fgarch11.roll = resume(STOCK.fgarch11.roll, solver="hybrid", solver.control=list(tol=1e-6, delta=1e-7, n.restarts=3, pkg="multicore", cores=4)) 
  STOCK.fgarch11.roll = resume(STOCK.fgarch11.roll, solver="solnp", solver.control=list(tol=1e-6, delta=1e-7, n.restarts=3, pkg="multicore", cores=4)) 
  STOCK.fgarch11.roll = resume(STOCK.fgarch11.roll, solver="nlminb", solver.control=list(tol=1e-6, delta=1e-7, n.restarts=3, pkg="multicore", cores=4))
  STOCK.fgarch11.roll = resume(STOCK.fgarch11.roll, solver="gosolnp", solver.control=list(tol=1e-6, delta=1e-7, n.restarts=3, pkg="multicore", cores=4))
  STOCK.fgarch11.roll = resume(STOCK.fgarch11.roll, solver="nloptr", solver.control=list(tol=1e-6, delta=1e-7, n.restarts=3, pkg="multicore", cores=4))
  STOCK.fgarch11.roll = resume(STOCK.fgarch11.roll, solver="msoptim", solver.control=list(tol=1e-6, delta=1e-7, n.restarts=3, pkg="multicore", cores=4)) 
  
  STOCK.fgarch11.roll = resume(STOCK.fgarch11.roll, solver="hybrid", solver.control=list(tol=1e-5, delta=1e-6, n.restarts=3, pkg="multicore", cores=4)) 
  STOCK.fgarch11.roll = resume(STOCK.fgarch11.roll, solver="solnp", solver.control=list(tol=1e-5, delta=1e-6, n.restarts=3, pkg="multicore", cores=4)) 
  STOCK.fgarch11.roll = resume(STOCK.fgarch11.roll, solver="nlminb", solver.control=list(tol=1e-5, delta=1e-6, n.restarts=3, pkg="multicore", cores=4))
  STOCK.fgarch11.roll = resume(STOCK.fgarch11.roll, solver="gosolnp", solver.control=list(tol=1e-5, delta=1e-6, n.restarts=3, pkg="multicore", cores=4))
  STOCK.fgarch11.roll = resume(STOCK.fgarch11.roll, solver="nloptr", solver.control=list(tol=1e-5, delta=1e-6, n.restarts=3, pkg="multicore", cores=4))
  STOCK.fgarch11.roll = resume(STOCK.fgarch11.roll, solver="msoptim", solver.control=list(tol=1e-5, delta=1e-6, n.restarts=3, pkg="multicore", cores=4)) 
  
  STOCK.fgarch11.roll = resume(STOCK.fgarch11.roll, solver="hybrid", solver.control=list(tol=1e-4, delta=1e-5, n.restarts=3, pkg="multicore", cores=4)) 
  STOCK.fgarch11.roll = resume(STOCK.fgarch11.roll, solver="solnp", solver.control=list(tol=1e-4, delta=1e-5, n.restarts=3, pkg="multicore", cores=4)) 
  STOCK.fgarch11.roll = resume(STOCK.fgarch11.roll, solver="nlminb", solver.control=list(tol=1e-4, delta=1e-5, n.restarts=3, pkg="multicore", cores=4))
  STOCK.fgarch11.roll = resume(STOCK.fgarch11.roll, solver="gosolnp", solver.control=list(tol=1e-4, delta=1e-5, n.restarts=3, pkg="multicore", cores=4))
  STOCK.fgarch11.roll = resume(STOCK.fgarch11.roll, solver="nloptr", solver.control=list(tol=1e-4, delta=1e-5, n.restarts=3, pkg="multicore", cores=4))
  STOCK.fgarch11.roll = resume(STOCK.fgarch11.roll, solver="msoptim", solver.control=list(tol=1e-4, delta=1e-5, n.restarts=3, pkg="multicore", cores=4)) 
  
  if (alpha == "0.01") {
    rVaR=as.data.frame(STOCK.fgarch11.roll@forecast$VaR$"alpha(1%)")
  }
  if (alpha == "0.025") {
    rVaR=as.data.frame(STOCK.fgarch11.roll@forecast$VaR$"alpha(3%)")
  }
  if (alpha == "0.05") {
    rVaR=as.data.frame(STOCK.fgarch11.roll@forecast$VaR$"alpha(5%)")
  }
  
  VaR=array(rVaR)
  result=data.frame(VaR=VaR)
  #str(result)
  #result = as.matrix(rVaR)
  #forecast_mu=STOCK.igarch11.roll@forecast$density$Mu
  colnames(result)<-colnames(R)
  result=zoo(result, as.Date(index(R))[(w.e+1):n.obs])
  return(result)
}