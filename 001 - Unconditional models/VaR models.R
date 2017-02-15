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



# VaR models
########################################################################

#Var.historical e' definita direttamente alla riga 76
VaR.historical = function(R,p)
{ 
  alpha = .setalphaprob(p)
  for(column in 1:ncol(R)) {
    r = na.omit(as.vector(R[,column]))
    rq = -quantile(r, type=3, probs=alpha)
    if (column==1) {
      result=data.frame(rq=rq)
    } else {
      rq=data.frame(rq=rq)
      result=cbind(result,rq)
    }
  }
  colnames(result)<-colnames(R)
  return(result)
}    


VaR.Gaussian =  function(R,p)
{
  alpha = .setalphaprob(p)
  
  columns = ncol(R)
  for(column in 1:columns) {
    r = as.vector(na.omit(R[,column]))
    if (!is.numeric(r)) stop("The selected column is not numeric") 
    # location = apply(R,2,mean);
    m2 = centeredmoment(r,2)
    VaR = - (mean(r) + qnorm(alpha)*sqrt(m2))
    VaR=array(VaR)
    if (column==1) {
      #create data.frame
      result=data.frame(VaR=VaR)
    } else {
      VaR=data.frame(VaR=VaR)
      result=cbind(result,VaR)
    }
  }
  colnames(result)<-colnames(R)
  return(result)
}


VaR.tstud =  function(R,p)
{
  alpha = .setalphaprob(p)
  
  columns = ncol(R)
  for(column in 1:columns) {
    r = as.vector(na.omit(R[,column]))
    if (!is.numeric(r)) stop("The selected column is not numeric") 
    fit.std = stdFit(r)    # t di Student 
    mean = fit.std$par[[1]]
    sd   = fit.std$par[[2]]
    nu   = fit.std$par[[3]]
    VaR = -qstd(alpha, mean, sd, nu)
    VaR=array(VaR)
    if (column==1) {
      #create data.frame
      result=data.frame(VaR=VaR)
    } else {
      VaR=data.frame(VaR=VaR)
      result=cbind(result,VaR)
    }
  }
  colnames(result)<-colnames(R)
  return(result)
}


VaR.skewt =  function(R,p)
{
  alpha = .setalphaprob(p)
  
  columns = ncol(R)
  for(column in 1:columns) {
    r = as.vector(na.omit(R[,column]))
    if (!is.numeric(r)) stop("The selected column is not numeric") 
    fit.sstd = sstdFit(r)   # t di Student asimmetrica
    mean  = fit.sstd$estimate[[1]]
    sd    = fit.sstd$estimate[[2]]
    nu    = fit.sstd$estimate[[3]]
    xi    = fit.sstd$estimate[[4]]
    VaR = -qsstd(alpha, mean, sd, nu, xi)
    VaR=array(VaR)
    if (column==1) {
      #create data.frame
      result=data.frame(VaR=VaR)
    } else {
      VaR=data.frame(VaR=VaR)
      result=cbind(result,VaR)
    }
  }
  colnames(result)<-colnames(R)
  return(result)
}