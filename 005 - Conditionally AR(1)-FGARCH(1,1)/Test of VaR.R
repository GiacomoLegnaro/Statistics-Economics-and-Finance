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



# Test of VaR
########################################################################
VaRTest = function(alpha = 0.05, actual, VaR, conf.level = 0.95){
  N = length(actual)
  VaRn = floor(N * alpha)
  if(N != length(VaR)) stop("\nlength of realized not equal to length of VaR!")
  tmp = LR.cc.test(p = alpha, actual = actual, VaR = VaR, conf.level = conf.level)
  ans = list()
  ans$expected.exceed = floor(alpha*tmp$TN)
  ans$actual.exceed = tmp$N
  ans$uc.H0 = "Correct Exceedances"
  ans$uc.LRstat = tmp$stat.uc
  ans$uc.critical = tmp$crit.val.uc
  ans$uc.LRp = tmp$p.value.uc
  #ans$uc.Decision = ifelse(ans$uc.LRp<(1-conf.level), "Reject H0", "Fail to Reject H0")
  ans$uc.Decision = ifelse(ans$uc.LRp<(1-conf.level), 1, 0)
  ans$ind.H0 = "Independent"
  ans$ind.LRstat = tmp$stat.ind
  ans$ind.critical = tmp$crit.val.ind
  ans$ind.LRp = tmp$p.value.ind
  ans$ind.Decision = ifelse(ans$ind.LRp<(1-conf.level), 1, 0)
  ans$cc.H0 = "Correct Exceedances & Independent"
  ans$cc.LRstat = tmp$stat.cc
  ans$cc.critical = tmp$crit.val.cc
  ans$cc.LRp = tmp$p.value.cc
  ans$cc.Decision = ifelse(ans$cc.LRp<(1-conf.level), 1, 0)
  return(ans)
}



# Test of Unconditional Coverage
# Test of Independence
# Test of Conditional Coverage
########################################################################
LR.cc.test = function (p, actual, VaR, conf.level = 0.95) 
{
  result = .LR.cc(p = p, actual = actual, VaR = VaR)
  crit.val.uc = qchisq(conf.level, df = 1)
  crit.val.ind = qchisq(conf.level, df = 1)
  crit.val.cc = qchisq(conf.level, df = 2)
  p.value.uc = 1 - pchisq(result$stat.uc, df = 1)
  p.value.ind = 1 - pchisq(result$stat.ind, df = 1)
  p.value.cc = 1 - pchisq(result$stat.cc, df = 2)
  reject.uc = ifelse(p.value.uc < 1 - conf.level, TRUE, FALSE)
  reject.ind = ifelse(p.value.ind < 1 - conf.level, TRUE, FALSE)
  reject.cc = ifelse(p.value.cc < 1 - conf.level, TRUE, FALSE)
  return(list(stat.uc = result$stat.uc, stat.cc = result$stat.cc, stat.ind = result$stat.ind,
              p.value.uc = p.value.uc, p.value.ind = p.value.ind, p.value.cc = p.value.cc,
              conf.level = conf.level,
              reject.uc = reject.uc, reject.ind = reject.ind, reject.cc = reject.cc,
              N = result$N, TN = result$TN, 
              crit.val.uc = crit.val.uc, crit.val.ind = crit.val.ind, crit.val.cc = crit.val.cc))
}
.LR.cc = function (p, actual, VaR) 
{
  VaR.ind = ifelse(actual < VaR, 1, 0)
  N = sum(VaR.ind, na.rm=TRUE)
  TN = length(VaR.ind)
  T00 = sum(c(0, ifelse(VaR.ind[2:TN] == 0 & VaR.ind[1:(TN - 1)] == 0, 1, 0)), na.rm=TRUE)
  T11 = sum(c(0, ifelse(VaR.ind[2:TN] == 1 & VaR.ind[1:(TN - 1)] == 1, 1, 0)), na.rm=TRUE)
  T01 = sum(c(0, ifelse(VaR.ind[2:TN] == 1 & VaR.ind[1:(TN - 1)] == 0, 1, 0)), na.rm=TRUE)
  T10 = sum(c(0, ifelse(VaR.ind[2:TN] == 0 & VaR.ind[1:(TN - 1)] == 1, 1, 0)), na.rm=TRUE)
  T0 = T00 + T01
  T1 = T10 + T11
  pi0 = T01/T0
  pi1 = T11/T1
  pe = (T01 + T11)/(T0 + T1)
  # stat.ind = -2 * log((1 - pe)^(T00 + T10) * pe^(T01 + T11)) + 2 * log((1 - pi0)^T00 * pi0^T01 * (1 - pi1)^T10 * pi1^T11)
  stat.ind = -2 *( (T00 + T10)*log(1 - pe) + (T01 + T11)*log(pe)) + 2 * (T00*log(1 - pi0)+T01*log(pi0)+T10*log(1 - pi1)+T11*log(pi1))
  stat.uc = .LR.uc(p = p, TN = TN, N = N)
  stat.cc = stat.uc + stat.ind
  return(list(stat.cc = stat.cc, stat.uc = stat.uc, stat.ind = stat.ind, N = N, 
              TN = TN))
}
.LR.uc = function (p, TN, N) 
{
  stat.uc = -2 *( (TN - N)*log(1 - p)+ N*log(p) ) + 2 * ( (TN - N)*log(1 - N/TN)+N*log(N/TN) )
  return(stat.uc)
}
.Log = function(x){
  ans = log(x)
  #if(!is.finite(ans)) ans = sign(ans) * 1e10
  ans
}