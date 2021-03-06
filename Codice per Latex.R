########################################################################
# Legnaro Giacomo
# Universita' degli Studi di Padova
# Statistica Economia e Finanza - Indirizzo: Finanza - Percorso: Metodologico
# AA 2014-2015
########################################################################
# Value-at-Risk Prediction
# Matricola : 1052299
########################################################################


########################################################################
# Working Directory
########################################################################


#mainDir="/Users/giacomolegnaro/Documenti - Mega/Statistica, Economia e Finanza/Tesi/Codice/001 - Unconditional models"
mainDir="/Users/giacomolegnaro/Documenti - Mega/Statistica, Economia e Finanza/Tesi/Codice"
#mainDir="C:/Giacomo/001 - Unconditional models"
setwd(mainDir)
subDir="Output"
dir.create(subDir, showWarnings = FALSE)
setwd(file.path(mainDir, subDir))
getwd()

########################################################################
# Load Libraries
########################################################################

library(PerformanceAnalytics)
library(quantmod)
library(rugarch)
library(car)
library(FinTS)
library(fGarch)

require(PerformanceAnalytics)

#options(digits=4)

#ordinamento file esportazione
########################################################################
tabella=1
grafico=1


########################################################################
# Data
########################################################################


# NASDAQ Composite (^IXIC)
########################################################################
# download data
symbol.vec = "^IXIC"
getSymbols(symbol.vec, from ="1971-02-08", to = "2001-06-22")
colnames(IXIC)
start(IXIC)
end(IXIC)

# extract closing prices
IXIC = IXIC[, "IXIC.Close", drop=F]

# plot prices
# Apertura file postscript
postscript(paste("Grafico IXIC - Price.eps", sep=""), width=10, height=7.5, pointsize=12)
grafico = grafico + 1
PerformanceAnalytics::chart.TimeSeries(IXIC, date.format="%Y-%m", main="NASDAQ Composite (^IXIC) - Price")
#plot(IXIC, main="NASDAQ Composite (^IXIC) - Price")
#seriesPlot(timeSeries(IXIC), col=1,rug=F, labels=F, title=F, main="NASDAQ Composite (^IXIC) - Price")
dev.off() #Chiusura file postscript

# calculate log-returns for analysis
IXIC.ret = CalculateReturns(IXIC, method="log")

# remove first NA observation
IXIC.ret = IXIC.ret[-1,]
colnames(IXIC.ret) ="IXIC"
qqnorm(IXIC.ret)
qqline(IXIC.ret)
?qqplot

# plot returns
# Apertura file postscript
postscript(paste("Grafico IXIC - Return.eps", sep=""), width=10, height=7.5, pointsize=12)
grafico = grafico + 1
PerformanceAnalytics::chart.TimeSeries(IXIC.ret, date.format="%Y-%m", main="NASDAQ Composite (^IXIC) - Return")
#plot(IXIC.ret, main="NASDAQ Composite (^IXIC) - Return")
dev.off() #Chiusura file postscript

# ready to be processed
#STOCK.ret = IXIC.ret



# Dow Jones Industrial Average (^DJI)
########################################################################
# download data
symbol.vec = "^DJI"
getSymbols(symbol.vec, from ="1971-02-08", to = "2015-08-25")
colnames(DJI)
start(DJI)
end(DJI)

# extract closing prices
DJI = DJI[, "DJI.Close", drop=F]

# plot prices
# Apertura file postscript
postscript(paste("Grafico DJI - Price.eps", sep=""), width=10, height=7.5, pointsize=12)
grafico = grafico + 1
#plot(DJI, main="Dow Jones Industrial Average (^DJI) - Price")
PerformanceAnalytics::chart.TimeSeries(DJI, date.format="%Y-%m", main="Dow Jones Industrial Average (^DJI) - Price")
dev.off() #Chiusura file postscript

# calculate log-returns for analysis
DJI.ret = CalculateReturns(DJI, method="log")

# remove first NA observation
DJI.ret = DJI.ret[-1,]
colnames(DJI.ret) ="DJI"

# plot returns
# Apertura file postscript
postscript(paste("Grafico DJI - Return.eps", sep=""), width=10, height=7.5, pointsize=12)
grafico = grafico + 1
#plot(DJI.ret, main="Dow Jones Industrial Average (^DJI) - Return")
PerformanceAnalytics::chart.TimeSeries(DJI.ret, date.format="%Y-%m", main="Dow Jones Industrial Average (^DJI) - Return")
dev.off() #Chiusura file postscript

# ready to be processed
#STOCK.ret = DJI.ret



# S&P 500 (^GSPC)
########################################################################
# download data
symbol.vec = "^GSPC"
getSymbols(symbol.vec, from ="1971-02-08", to = "2001-06-22")
colnames(GSPC)
start(GSPC)
end(GSPC)

# extract closing prices
GSPC = GSPC[, "GSPC.Close", drop=F]

# plot prices
# Apertura file postscript
postscript(paste("Grafico GSPC - Price.eps", sep=""), width=10, height=7.5, pointsize=12)
grafico = grafico + 1
#plot(GSPC, main="S&P 500 (^GSPC) - Price")
PerformanceAnalytics::chart.TimeSeries(GSPC, date.format="%Y-%m", main="S&P 500 (^GSPC) - Price")
dev.off() #Chiusura file postscript

# calculate log-returns for analysis
GSPC.ret = CalculateReturns(GSPC, method="log")

# remove first NA observation
GSPC.ret = GSPC.ret[-1,]
colnames(GSPC.ret) ="GSPC"

# plot returns
# Apertura file postscript
postscript(paste("Grafico GSPC - Return.eps", sep=""), width=10, height=7.5, pointsize=12)
grafico = grafico + 1
PerformanceAnalytics::chart.TimeSeries(GSPC.ret, date.format="%Y-%m", main="S&P 500 (^GSPC) - Return")
#plot(GSPC.ret, main="S&P 500 (^GSPC) - Return")
dev.off() #Chiusura file postscript

# ready to be processed
#STOCK.ret = GSPC.ret




# DAX (^GDAXI) - Deutscher Aktienindex (German stock index)
########################################################################
# download data
symbol.vec = "^GDAXI"
getSymbols(symbol.vec, from ="1971-02-08", to = "2015-08-25")
colnames(GDAXI)
start(GDAXI)
end(GDAXI)

# extract closing prices
GDAXI = GDAXI[, "GDAXI.Close", drop=F]

# plot prices
# Apertura file postscript
postscript(paste("Grafico GDAXI - Price.eps", sep=""), width=10, height=7.5, pointsize=12)
grafico = grafico + 1
PerformanceAnalytics::chart.TimeSeries(GDAXI, date.format="%Y-%m", main="DAX (^GDAXI) - Price")
#plot(GDAXI, main="DAX (^GDAXI) - Price")
dev.off() #Chiusura file postscript

# calculate log-returns for analysis
GDAXI.ret = CalculateReturns(GDAXI, method="log")

# remove first NA observation
GDAXI.ret = GDAXI.ret[-1,]
colnames(GDAXI.ret) ="GDAXI"

# plot returns
# Apertura file postscript
postscript(paste("Grafico GDAXI - Return.eps", sep=""), width=10, height=7.5, pointsize=12)
grafico = grafico + 1
PerformanceAnalytics::chart.TimeSeries(GDAXI.ret, date.format="%Y-%m", main="DAX (^GDAXI) - Return")
#plot(GDAXI.ret, main="DAX (^GDAXI) - Return")
dev.off() #Chiusura file postscript

# ready to be processed
#STOCK.ret = GDAXI.ret




# CAC 40 (^FCHI)
########################################################################
#CAC 40 (French: CAC quarante [kak kaʁɑ̃t]) (Cotation Assistée en Continu)
#is a benchmark French stock market index. 

# download data
symbol.vec = "^FCHI"
getSymbols(symbol.vec, from ="1971-02-08", to = "2015-08-25")
colnames(FCHI)
start(FCHI)
end(FCHI)

# extract closing prices
FCHI = FCHI[, "FCHI.Close", drop=F]

# plot prices
# Apertura file postscript
postscript(paste("Grafico FCHI - Price.eps", sep=""), width=10, height=7.5, pointsize=12)
grafico = grafico + 1
PerformanceAnalytics::chart.TimeSeries(FCHI, date.format="%Y-%m", main="CAC 40 (^FCHI) - Price")
#plot(FCHI, main="CAC 40 (^FCHI) - Price")
dev.off() #Chiusura file postscript

# calculate log-returns for analysis
FCHI.ret = CalculateReturns(FCHI, method="log")

# remove first NA observation
FCHI.ret = FCHI.ret[-1,]
colnames(FCHI.ret) ="FCHI"

# plot returns
# Apertura file postscript
postscript(paste("Grafico FCHI - Return.eps", sep=""), width=10, height=7.5, pointsize=12)
grafico = grafico + 1
PerformanceAnalytics::chart.TimeSeries(FCHI.ret, date.format="%Y-%m", main="CAC 40 (^FCHI) - Return")
#plot(FCHI.ret, main="CAC 40 (^FCHI) - Return")
dev.off() #Chiusura file postscript

# ready to be processed
#STOCK.ret = FCHI.ret



# FTSE 100 (^FTSE)
########################################################################
# FTSE 100 è un indice azionario delle 100 società più capitalizzate 
# quotate al London Stock Exchange.
# download data
symbol.vec = "^FTSE"
getSymbols(symbol.vec, from ="1971-02-08", to = "2001-06-22")
colnames(FTSE)
start(FTSE)
end(FTSE)

# extract closing prices
FTSE = FTSE[, "FTSE.Close", drop=F]

# plot prices
# Apertura file postscript
postscript(paste("Grafico FTSE - Price.eps", sep=""), width=10, height=7.5, pointsize=12)
grafico = grafico + 1
PerformanceAnalytics::chart.TimeSeries(FTSE, date.format="%Y-%m", main="FTSE 100 (^FTSE) - Price")
#plot(FTSE, main="FTSE 100 (^FTSE) - Price")
dev.off() #Chiusura file postscript

# calculate log-returns for analysis
FTSE.ret = CalculateReturns(FTSE, method="log")

# remove first NA observation
FTSE.ret = FTSE.ret[-1,]
colnames(FTSE.ret) ="FTSE"

# plot returns
# Apertura file postscript
postscript(paste("Grafico FTSE - Return.eps", sep=""), width=10, height=7.5, pointsize=12)
grafico = grafico + 1
PerformanceAnalytics::chart.TimeSeries(FTSE.ret, date.format="%Y-%m", main="FTSE 100 (^FTSE) - Return")
#plot(FTSE.ret, main="FTSE 100 (^FTSE) - Return")
dev.off() #Chiusura file postscript

# ready to be processed
#STOCK.ret = FTSE.ret






# FTSE MIB (FTSEMIB.MI)
########################################################################
# download data
symbol.vec = "FTSEMIB.MI"
getSymbols(symbol.vec, from ="1971-02-08", to = "2015-08-25")
#start(FTSEMIB.MI) "1997-12-31"
#end(FTSEMIB.MI) "2015-08-25"
colnames(FTSEMIB.MI)
start(FTSEMIB.MI)
end(FTSEMIB.MI)

# extract closing prices
FTSEMIB.MI = FTSEMIB.MI[, "FTSEMIB.MI.Close", drop=F]

# plot prices
# Apertura file postscript
postscript(paste("Grafico FTSEMIB.MI - Price.eps", sep=""), width=10, height=7.5, pointsize=12)
grafico = grafico + 1
PerformanceAnalytics::chart.TimeSeries(FTSEMIB.MI, date.format="%Y-%m", main="FTSE MIB (FTSEMIB.MI) - Price")
#plot(FTSEMIB.MI, main="FTSE MIB (FTSEMIB.MI) - Price") 
dev.off() #Chiusura file postscript

# calculate log-returns for analysis
FTSEMIB.MI.ret = CalculateReturns(FTSEMIB.MI, method="log")

# remove first NA observation
FTSEMIB.MI.ret = FTSEMIB.MI.ret[-1,]
colnames(FTSEMIB.MI.ret) ="FTSEMIB.MI"

# plot returns
# Apertura file postscript
postscript(paste("Grafico FTSEMIB.MI - Return.eps", sep=""), width=10, height=7.5, pointsize=12)
grafico = grafico + 1
PerformanceAnalytics::chart.TimeSeries(FTSEMIB.MI.ret, date.format="%Y-%m", main="FTSE MIB (FTSEMIB.MI) - Return")
#plot(FTSEMIB.MI.ret, main="FTSE MIB (FTSEMIB.MI) - Return")
dev.off() #Chiusura file postscript

# ready to be processed
#STOCK.ret = FTSEMIB.MI.ret



#STOCK=data.frame(IXIC.ret, DJI.ret, GSPC.ret, XIN4.L.ret, GDAXI.ret, FCHI.ret, FTSEMIB.MI.ret)
#STOCK=data.frame(IXIC.ret, DJI.ret, GSPC.ret, GDAXI.ret, FCHI.ret, FTSEMIB.MI.ret)
#STOCK=c("IXIC.ret", "DJI.ret", "GSPC.ret", "GDAXI.ret", "FCHI.ret", "FTSEMIB.MI.ret")
STOCK=c("IXIC.ret", "DJI.ret", "GSPC.ret", "GDAXI.ret", "FCHI.ret", "FTSE.ret", "FTSEMIB.MI.ret")

########################################################################
# Elaborazione
########################################################################


for (y in STOCK){
  # for titolo
  ########################################################################  
  STOCK.ret = eval(parse(text=STOCK[index(STOCK)[which(y==STOCK)]],))
  
  postscript(paste("Grafico ", colnames(STOCK.ret)," - QQPlot.eps", sep=""), width=10, height=7.5, pointsize=12)
  grafico = grafico + 1
  qqnormPlot(STOCK.ret, title=F, main=paste("Normal Probability Plot - ", colnames(STOCK.ret), sep="")) 
  dev.off() #Chiusura file postscript
}  


for (y in STOCK){
  STOCK.ret = eval(parse(text=STOCK[index(STOCK)[which(y==STOCK)]],))
  stat = matrix(c(basicStats(STOCK.ret)[c(1,7,14,15,16,3,4),],
                      as.numeric(jarqueberaTest(as.zoo(STOCK.ret))@test$statistic),
                      as.numeric(jarqueberaTest(as.zoo(STOCK.ret))@test$p.value)),
                nrow=1, ncol=9, 
                dimnames=list(c(colnames(STOCK.ret)),
                              c(labels(basicStats(STOCK.ret))[[1]][c(1,7,14,15,16,3,4)],"JB-test","JB-pvalue")))
  print(xtable(stat, digits=c(0,0,6,6,6,6,6,6,3,3), align=c("c","c","c","c","c","c","c","c","c","c")))
}


for (y in STOCK){
  STOCK.ret = eval(parse(text=STOCK[index(STOCK)[which(y==STOCK)]],))

  stat = data.frame(as.matrix(c(basicStats(STOCK.ret)[1,],
                                format(as.Date(start(STOCK.ret)), "%d/%m/%Y"),
                                format(as.Date(end(STOCK.ret)), "%d/%m/%Y"),
                                basicStats(STOCK.ret)[c(7,14),])))
  rownames(stat)=c(labels(basicStats(STOCK.ret))[[1]][c(1)],
                   "Start","End",
                   labels(basicStats(STOCK.ret))[[1]][c(7,14)])
  colnames(stat)=c(colnames(STOCK.ret))
  print(xtable(data.frame(t(stat)), digits=c(0,0,0,6,6,6),
               align=c("c","c","c","c","c","c"), label=NULL))                    
  
  
  stat = data.frame(as.matrix(c(basicStats(STOCK.ret)[c(15,16,3,4),],
                                as.numeric(jarqueberaTest(as.zoo(STOCK.ret))@test$statistic),
                                as.numeric(jarqueberaTest(as.zoo(STOCK.ret))@test$p.value))))
  rownames(stat)=c(labels(basicStats(STOCK.ret))[[1]][c(15,16,3,4)],
                   "JB-test", "JB-pvalue")
  colnames(stat)=c(colnames(STOCK.ret))
  print(xtable(data.frame(t(stat)), digits=c(0,6,6,6,6,3,3),
               align=c("c","c","c","c","c","c","c"), label=colnames(STOCK.ret)))                    
  
}



# Gestione directory
########################################################################
mainDir="/Users/giacomolegnaro/Documenti - Mega/Statistica, Economia e Finanza/Tesi/LaTeX/Output/"
setwd(mainDir)

y="IXIC.ret"
y="DJI.ret"
y="GSPC.ret"
y="GDAXI.ret"
y="FCHI.ret"
y="FTSE.ret"
y="FTSEMIB.MI.ret"
i="001"
i="002"
i="003"
i="004"
i="005"
#for (i in c("001","002","003")){
  subDir=paste(y,i,"", sep="/")
  dir.create(subDir, showWarnings = FALSE)
  setwd(file.path(mainDir, subDir))
  VaR = read.csv2("VaR.csv")
  print(xtable(VaR, digits=c(0,0,1,0,0,2,2,3,0,2,3,0,2,3,0,4),
               align=c("c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c"),
               label=paste("Unconditional Model - ", y, sep="")))       
  
 
    

#}






  getwd()
  
  # Gestione directory
  ########################################################################
  setwd(mainDir)
  dir.create(y, showWarnings = FALSE)
  subDir=paste(y,"Workspace","", sep="/")
  dir.create(subDir, showWarnings = FALSE)
  setwd(file.path(mainDir, subDir))
  
  
  # for titolo
  ########################################################################  
  STOCK.ret = eval(parse(text=STOCK[index(STOCK)[which(y==STOCK)]],))
  
  
  ########################################################################
  # Statistiche Descrittive
  ########################################################################
  
  stat = matrix(basicStats(STOCK.ret)[c(1,7,14,15,16,3,4),], nrow=1, ncol=7, 
                dimnames=list(c(), labels(basicStats(STOCK.ret))[[1]][c(1,7,14,15,16,3,4)]))
  stat
  
  #Esportazione risultati
  write.csv2(stat, file=paste(1000+tabella, " - Statistiche descrittive ",
                              colnames(STOCK.ret), ".csv", sep=""))
  tabella = tabella + 1 
  
  
  
  
  ########################################################################
  # backtesting unconditional VaR models  -  Parameters 
  # HS, Normal, t, Skewed t
  ########################################################################
  
  # set up estimation window and testing window
  n.obs = nrow(STOCK.ret)
  #Rolling Windows = 1000
  w.e = 1000
  w.t = n.obs - w.e
  #livelli di significativita'
  lambda=c(1, 2.5, 5)
  
  
  
  
  ########################################################################
  # backTestVar
  ########################################################################
  # loop over testing sample, compute VaR and record hit rates
  backTestVaR <- function(x, p = 0.95) {
    #0.95 e' il valore standard, ma lo si modifica se gli si passa un valore diverso al parametro
    historical.VaR = as.numeric(VaR(x, p=p, method="historical", invert=TRUE))
    normal.VaR = as.numeric(VaR(x, p=p, method="gaussian", invert=TRUE))
    tstudent.VaR = as.numeric(VaR(x, p=p, method="tstud", invert=TRUE))
    skewt.VaR = as.numeric(VaR(x, p=p, method="skewt", invert=TRUE))
    #Se si vuol avere il valore del percentile originario come segno invert=FALSE
    ans = c(historical.VaR, normal.VaR, tstudent.VaR, skewt.VaR)
    names(ans) = c("HS", "Normal", "t-stud", "skew-t")
    return(ans)
  }
  
  
  ########################################################################
  VaR.performance = matrix(0, nrow=12, ncol=14,
                           dimnames=list(c("HS","","","Normal","","",
                                           "t-stud","","","skew-t","",""),
                                         c("100 lambda",
                                           "Expected Exceeed","Actual Exceed",
                                           "%viol",
                                           "LR.uc.stat","LR.uc.p","LR.uc.decision",
                                           "LR.ind.stat","LR.ind.p","LR.ind.decision",
                                           "LR.cc.stat","LR.cc.p","LR.cc.decision",
                                           "avg(VaR)")))
  
  for (z in 1:3){
    alpha = 1-(lambda[z]/100)
    VaR.performance[c(z,z+3,z+6,z+9),"100 lambda"]=lambda[z]
    # rolling 1-step ahead estimates of VaR
    VaR.results = rollapply(as.zoo(STOCK.ret), width=w.e, 
                            FUN = backTestVaR, p=alpha, by.column = FALSE,
                            align = "right")
    #p=0.99 
    #livello di confidenza alpha=0.01
    #head(VaR.results)
    #head(sort(as.numeric(STOCK.ret[1:1000])),20)
    #str(STOCK.ret)
    VaR.results = lag(VaR.results, k=-1)
    #str(VaR.results)
    #head(VaR.results)
    #riaggiusto la serie per il confronto diretto Value at Risk vs rendimento osservato
    #per il calcolo delle violazioni
    # Apertura file postscript
    postscript(paste(grafico, " Grafico" ,".eps", sep=""), width=10, height=7.5, pointsize=12)
    grafico = grafico + 1
    chart.TimeSeries(merge(STOCK.ret, VaR.results), legend.loc="topright")
    dev.off() #Chiusura file postscript
    
    
    
    # violation's table
    ########################################################################
    
    violations.mat = matrix(0, 4, 5)
    rownames(violations.mat) = c("HS", "Normal","t-stud", "skew-t")
    colnames(violations.mat) = c("En1", "n1", "1-alpha", "Percent", "VR")
    violations.mat[, "En1"] = (1-alpha)*w.t
    #En1 numero di violazioni per alpha fissato pari a 0.01
    #1-alpha livello di significativita'
    violations.mat[, "1-alpha"] = 1 - alpha
    
    for(i in colnames(VaR.results)) {
      VaR.violations = as.zoo(STOCK.ret[index(VaR.results), ]) < VaR.results[, i]
      violations.mat[i, "n1"] = sum(VaR.violations, na.rm=TRUE)
      violations.mat[i, "Percent"] = sum(VaR.violations, na.rm=TRUE)/w.t
      violations.mat[i, "VR"] = violations.mat[i, "n1"]/violations.mat[i, "En1"]
      VaR.performance[(z-3)+3*index(colnames(VaR.results))[which(i==colnames(VaR.results))], "%viol"] = 
        violations.mat[i, "Percent"]*100
      # Apertura file postscript
      postscript(paste(grafico, " Grafico", i, "VaR.eps", sep=""), width=10, height=7.5, pointsize=12)
      grafico = grafico + 1
      chart.TimeSeries(-VaR.results[,i])
      dev.off() #Chiusura file postscript
    }
    violations.mat
    
    
    # Show HS VaR violations
    ########################################################################
    
    HSVaR.violations = as.zoo(STOCK.ret[index(VaR.results), ]) < VaR.results[, "HS"]
    violation.dates = index(HSVaR.violations[which(HSVaR.violations)])
    
    # plot violations
    # Apertura file postscript
    postscript(paste(grafico, " Grafico" ,".eps", sep=""), width=10, height=7.5, pointsize=12)
    grafico = grafico + 1
    plot(as.zoo(STOCK.ret[index(VaR.results),]), col="blue", ylab="Return",
         main=paste("HS VaR violations - lambda = ", alpha, sep=""))
    abline(h=0)
    lines(VaR.results[, "HS"], col="black", lwd=2)
    lines(as.zoo(STOCK.ret[violation.dates,]), type="p", pch=16, col="red", lwd=2)
    dev.off() #Chiusura file postscript  
    
    
    # HS VaR Test uc - ind - cc - avg
    ########################################################################
    
    VaR.test = VaRTest(alpha=(1-alpha), 
                       actual=coredata(STOCK.ret[index(VaR.results),]), 
                       VaR=coredata(VaR.results[,"HS"]),
                       conf.level=alpha)
    
    #coredata prende solo i valori e tralascia intestazioni colonne ecc ecc.
    names(VaR.test)
    # numbers of exceedances
    VaR.test[1:2]
    VaR.performance[z, "Expected Exceeed"] = as.numeric(VaR.test[1])
    VaR.performance[z, "Actual Exceed"] = as.numeric(VaR.test[2])
    # LR tests for unconditional coverage of exceedances
    VaR.test[3:7]
    VaR.performance[z, "LR.uc.stat"] = as.numeric(VaR.test[4])
    VaR.performance[z, "LR.uc.p"] = as.numeric(VaR.test[6])
    VaR.performance[z, "LR.uc.decision"] = as.numeric(VaR.test[7])
    # LR tests for independence of exceedances
    VaR.test[8:12]
    VaR.performance[z, "LR.ind.stat"] = as.numeric(VaR.test[9])
    VaR.performance[z, "LR.ind.p"] = as.numeric(VaR.test[11])
    VaR.performance[z, "LR.ind.decision"] = as.numeric(VaR.test[12])
    # LR tests for conditional coverage of exceedances
    VaR.test[13:17]
    VaR.performance[z, "LR.cc.stat"] = as.numeric(VaR.test[14])
    VaR.performance[z, "LR.cc.p"] = as.numeric(VaR.test[16])
    VaR.performance[z, "LR.cc.decision"] = as.numeric(VaR.test[17])
    # Average value of the VaR estimates
    avg.VaR = -mean(VaR.results[, "HS"], na.rm=TRUE)
    VaR.performance[z, "avg(VaR)"] = avg.VaR
    
    #Esportazione risultati
    write.csv2(violations.mat, file=paste(1000+tabella, " - HS violations - lambda ", (1-alpha)*100, ".csv", sep=""))
    tabella = tabella + 1 
    write.csv2(VaR.test, file=paste(1000+tabella, " - HS VaR test - lambda ", (1-alpha)*100, ".csv", sep=""))
    tabella = tabella + 1
    write.csv2(avg.VaR, file=paste(1000+tabella, " - HS avg.VaR - lambda ", (1-alpha)*100, ".csv", sep=""))
    tabella = tabella + 1
    
    
    
    
    # Show Normal VaR violations
    ########################################################################
    
    normalVaR.violations = as.zoo(STOCK.ret[index(VaR.results), ]) < VaR.results[, "Normal"]
    violation.dates = index(normalVaR.violations[which(normalVaR.violations)])
    
    # plot violations
    # Apertura file postscript
    postscript(paste(grafico, " Grafico" ,".eps", sep=""), width=10, height=7.5, pointsize=12)
    grafico = grafico + 1
    plot(as.zoo(STOCK.ret[index(VaR.results),]), col="blue", ylab="Return",
         main=paste("Normal VaR violations - lambda = ", alpha, sep=""))
    abline(h=0)
    lines(VaR.results[, "Normal"], col="black", lwd=2)
    lines(as.zoo(STOCK.ret[violation.dates,]), type="p", pch=16, col="red", lwd=2)
    dev.off() #Chiusura file postscript
    
    
    # Normal VaR Test uc - ind - cc - avg
    ########################################################################
    
    VaR.test = VaRTest(alpha=(1-alpha), 
                       actual=coredata(STOCK.ret[index(VaR.results),]), 
                       VaR=coredata(VaR.results[,"Normal"]),
                       conf.level=alpha)
    
    #coredata prende solo i valori e tralascia intestazioni colonne ecc ecc.
    names(VaR.test)
    # numbers of exceedances
    VaR.test[1:2]
    VaR.performance[z+3, "Expected Exceeed"] = as.numeric(VaR.test[1])
    VaR.performance[z+3, "Actual Exceed"] = as.numeric(VaR.test[2])
    # LR tests for unconditional coverage of exceedances
    VaR.test[3:7]
    VaR.performance[z+3, "LR.uc.stat"] = as.numeric(VaR.test[4])
    VaR.performance[z+3, "LR.uc.p"] = as.numeric(VaR.test[6])
    VaR.performance[z+3, "LR.uc.decision"] = as.numeric(VaR.test[7])
    # LR tests for independence of exceedances
    VaR.test[8:12]
    VaR.performance[z+3, "LR.ind.stat"] = as.numeric(VaR.test[9])
    VaR.performance[z+3, "LR.ind.p"] = as.numeric(VaR.test[11])
    VaR.performance[z+3, "LR.ind.decision"] = as.numeric(VaR.test[12])
    # LR tests for conditional coverage of exceedances
    VaR.test[13:17]
    VaR.performance[z+3, "LR.cc.stat"] = as.numeric(VaR.test[14])
    VaR.performance[z+3, "LR.cc.p"] = as.numeric(VaR.test[16])
    VaR.performance[z+3, "LR.cc.decision"] = as.numeric(VaR.test[17])
    # Average value of the VaR estimates
    avg.VaR = -mean(VaR.results[, "Normal"], na.rm=TRUE)
    VaR.performance[z+3, "avg(VaR)"] = avg.VaR
    
    #Esportazione risultati
    write.csv2(violations.mat, file=paste(1000+tabella, " - Normal violations - lambda ", (1-alpha)*100, ".csv", sep=""))
    tabella = tabella + 1 
    write.csv2(VaR.test, file=paste(1000+tabella, " - Normal VaR test - lambda ", (1-alpha)*100, ".csv", sep=""))
    tabella = tabella + 1
    write.csv2(avg.VaR, file=paste(1000+tabella, " - Normal avg.VaR - lambda ", (1-alpha)*100, ".csv", sep=""))
    tabella = tabella + 1
    
    
    
    
    
    # Show t di Student VaR violations
    ########################################################################
    
    tstudVaR.violations = as.zoo(STOCK.ret[index(VaR.results), ]) < VaR.results[, "t-stud"]
    violation.dates = index(normalVaR.violations[which(normalVaR.violations)])
    
    # plot violations
    # Apertura file postscript
    postscript(paste(grafico, " Grafico" ,".eps", sep=""), width=10, height=7.5, pointsize=12)
    grafico = grafico + 1
    plot(as.zoo(STOCK.ret[index(VaR.results),]), col="blue", ylab="Return",
         main=paste("Student t VaR violations - lambda = ", alpha, sep=""))
    abline(h=0)
    lines(VaR.results[, "t-stud"], col="black", lwd=2)
    lines(as.zoo(STOCK.ret[violation.dates,]), type="p", pch=16, col="red", lwd=2)
    dev.off() #Chiusura file postscript
    
    
    # t di Student VaR Test uc - ind - cc - avg
    ########################################################################
    
    VaR.test = VaRTest(alpha=(1-alpha), 
                       actual=coredata(STOCK.ret[index(VaR.results),]), 
                       VaR=coredata(VaR.results[,"t-stud"]),
                       conf.level=alpha)
    
    #coredata prende solo i valori e tralascia intestazioni colonne ecc ecc.
    names(VaR.test)
    # numbers of exceedances
    VaR.test[1:2]
    VaR.performance[z+6, "Expected Exceeed"] = as.numeric(VaR.test[1])
    VaR.performance[z+6, "Actual Exceed"] = as.numeric(VaR.test[2])
    # LR tests for unconditional coverage of exceedances
    VaR.test[3:7]
    VaR.performance[z+6, "LR.uc.stat"] = as.numeric(VaR.test[4])
    VaR.performance[z+6, "LR.uc.p"] = as.numeric(VaR.test[6])
    VaR.performance[z+6, "LR.uc.decision"] = as.numeric(VaR.test[7])
    # LR tests for independence of exceedances
    VaR.test[8:12]
    VaR.performance[z+6, "LR.ind.stat"] = as.numeric(VaR.test[9])
    VaR.performance[z+6, "LR.ind.p"] = as.numeric(VaR.test[11])
    VaR.performance[z+6, "LR.ind.decision"] = as.numeric(VaR.test[12])
    # LR tests for conditional coverage of exceedances
    VaR.test[13:17]
    VaR.performance[z+6, "LR.cc.stat"] = as.numeric(VaR.test[14])
    VaR.performance[z+6, "LR.cc.p"] = as.numeric(VaR.test[16])
    VaR.performance[z+6, "LR.cc.decision"] = as.numeric(VaR.test[17])
    # Average value of the VaR estimates
    avg.VaR = -mean(VaR.results[, "t-stud"], na.rm=TRUE)
    VaR.performance[z+6, "avg(VaR)"] = avg.VaR
    
    #Esportazione risultati
    write.csv2(violations.mat, file=paste(1000+tabella, " - t-stud violations - lambda ", (1-alpha)*100, ".csv", sep=""))
    tabella = tabella + 1 
    write.csv2(VaR.test, file=paste(1000+tabella, " - t-stud VaR test - lambda ", (1-alpha)*100, ".csv", sep=""))
    tabella = tabella + 1
    write.csv2(avg.VaR, file=paste(1000+tabella, " - t-stud avg.VaR - lambda ", (1-alpha)*100, ".csv", sep=""))
    tabella = tabella + 1
    
    
    
    
    # Show Skew-t VaR violations
    ########################################################################
    
    normalVaR.violations = as.zoo(STOCK.ret[index(VaR.results), ]) < VaR.results[, "skew-t"]
    violation.dates = index(normalVaR.violations[which(normalVaR.violations)])
    
    # plot violations
    # Apertura file postscript
    postscript(paste(grafico, " Grafico" ,".eps", sep=""), width=10, height=7.5, pointsize=12)
    grafico = grafico + 1
    plot(as.zoo(STOCK.ret[index(VaR.results),]), col="blue", ylab="Return",
         main=paste("Skew-t VaR violations - lambda = ", alpha, sep=""))
    abline(h=0)
    lines(VaR.results[, "skew-t"], col="black", lwd=2)
    lines(as.zoo(STOCK.ret[violation.dates,]), type="p", pch=16, col="red", lwd=2)
    dev.off() #Chiusura file postscript
    
    
    # Skew-t VaR Test uc - ind - cc - avg
    ########################################################################
    
    VaR.test = VaRTest(alpha=1-alpha, 
                       actual=coredata(STOCK.ret[index(VaR.results),]), 
                       VaR=coredata(VaR.results[,"skew-t"]),
                       conf.level=alpha)
    
    #coredata prende solo i valori e tralascia intestazioni colonne ecc ecc.
    names(VaR.test)
    # numbers of exceedances
    VaR.test[1:2]
    VaR.performance[z+9, "Expected Exceeed"] = as.numeric(VaR.test[1])
    VaR.performance[z+9, "Actual Exceed"] = as.numeric(VaR.test[2])
    # LR tests for unconditional coverage of exceedances
    VaR.test[3:7]
    VaR.performance[z+9, "LR.uc.stat"] = as.numeric(VaR.test[4])
    VaR.performance[z+9, "LR.uc.p"] = as.numeric(VaR.test[6])
    VaR.performance[z+9, "LR.uc.decision"] = as.numeric(VaR.test[7])
    # LR tests for independence of exceedances
    VaR.test[8:12]
    VaR.performance[z+9, "LR.ind.stat"] = as.numeric(VaR.test[9])
    VaR.performance[z+9, "LR.ind.p"] = as.numeric(VaR.test[11])
    VaR.performance[z+9, "LR.ind.decision"] = as.numeric(VaR.test[12])
    # LR tests for conditional coverage of exceedances
    VaR.test[13:17]
    VaR.performance[z+9, "LR.cc.stat"] = as.numeric(VaR.test[14])
    VaR.performance[z+9, "LR.cc.p"] = as.numeric(VaR.test[16])
    VaR.performance[z+9, "LR.cc.decision"] = as.numeric(VaR.test[17])
    # Average value of the VaR estimates
    avg.VaR = -mean(VaR.results[, "skew-t"], na.rm=TRUE)
    VaR.performance[z+9, "avg(VaR)"] = avg.VaR
    
    #Esportazione risultati
    write.csv2(violations.mat, file=paste(1000+tabella, " - Skew-t violations - lambda ", (1-alpha)*100, ".csv", sep=""))
    tabella = tabella + 1 
    write.csv2(VaR.test, file=paste(1000+tabella, " - Skew-t VaR test - lambda ", (1-alpha)*100, ".csv", sep=""))
    tabella = tabella + 1
    write.csv2(avg.VaR, file=paste(1000+tabella, " - Skew-t avg.VaR - lambda ", (1-alpha)*100, ".csv", sep=""))
    tabella = tabella + 1  
  }
  
  write.csv2(VaR.performance, file="VaR Prediction Performance - unconditional models.csv")
  
}