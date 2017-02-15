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


########################################################################
# Working Directory
########################################################################


#mainDir="/Users/giacomolegnaro/Documenti - Mega/Statistica, Economia e Finanza/Tesi/Codice/003 - Conditionally AR(1)-IGARCH(1,1)"
mainDir="C:/Giacomo/003 - Conditionally AR(1)-IGARCH(1,1)"
setwd(mainDir)
subDir="Output"
dir.create(subDir, showWarnings = FALSE)
setwd(file.path(mainDir, subDir))
getwd()


########################################################################
# Load Libraries
########################################################################

install.packages("PerformanceAnalytics")
install.packages("quantmod")
install.packages("rugarch")
install.packages("car")
install.packages("FinTS")
install.packages("fGarch")

library(PerformanceAnalytics)
library(quantmod)
library(rugarch)
library(car)
library(FinTS)
library(fGarch)

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
postscript(paste(grafico, " Grafico IXIC - Price.eps", sep=""), width=10, height=7.5, pointsize=12)
grafico = grafico + 1
plot(IXIC, main="NASDAQ Composite (^IXIC) - Price")
dev.off() #Chiusura file postscript

# calculate log-returns for analysis
IXIC.ret = CalculateReturns(IXIC, method="log")

# remove first NA observation
IXIC.ret = IXIC.ret[-1,]
colnames(IXIC.ret) ="IXIC"

# plot returns
# Apertura file postscript
postscript(paste(grafico, " Grafico IXIC - Return.eps", sep=""), width=10, height=7.5, pointsize=12)
grafico = grafico + 1
plot(IXIC.ret, main="NASDAQ Composite (^IXIC) - Return")
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
postscript(paste(grafico, " Grafico DJI - Price.eps", sep=""), width=10, height=7.5, pointsize=12)
grafico = grafico + 1
plot(DJI, main="Dow Jones Industrial Average (^DJI) - Price")
dev.off() #Chiusura file postscript

# calculate log-returns for analysis
DJI.ret = CalculateReturns(DJI, method="log")

# remove first NA observation
DJI.ret = DJI.ret[-1,]
colnames(DJI.ret) ="DJI"

# plot returns
# Apertura file postscript
postscript(paste(grafico, " Grafico DJI - Return.eps", sep=""), width=10, height=7.5, pointsize=12)
grafico = grafico + 1
plot(DJI.ret, main="Dow Jones Industrial Average (^DJI) - Return")
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
postscript(paste(grafico, " Grafico GSPC - Price.eps", sep=""), width=10, height=7.5, pointsize=12)
grafico = grafico + 1
plot(GSPC, main="S&P 500 (^GSPC) - Price")
dev.off() #Chiusura file postscript

# calculate log-returns for analysis
GSPC.ret = CalculateReturns(GSPC, method="log")

# remove first NA observation
GSPC.ret = GSPC.ret[-1,]
colnames(GSPC.ret) ="GSPC"

# plot returns
# Apertura file postscript
postscript(paste(grafico, " Grafico GSPC - Return.eps", sep=""), width=10, height=7.5, pointsize=12)
grafico = grafico + 1
plot(GSPC.ret, main="S&P 500 (^GSPC) - Return")
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
postscript(paste(grafico, " Grafico GDAXI - Price.eps", sep=""), width=10, height=7.5, pointsize=12)
grafico = grafico + 1
plot(GDAXI, main="DAX (^GDAXI) - Price")
dev.off() #Chiusura file postscript

# calculate log-returns for analysis
GDAXI.ret = CalculateReturns(GDAXI, method="log")

# remove first NA observation
GDAXI.ret = GDAXI.ret[-1,]
colnames(GDAXI.ret) ="GDAXI"

# plot returns
# Apertura file postscript
postscript(paste(grafico, " Grafico GDAXI - Return.eps", sep=""), width=10, height=7.5, pointsize=12)
grafico = grafico + 1
plot(GDAXI.ret, main="DAX (^GDAXI) - Return")
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
postscript(paste(grafico, " Grafico FCHI - Price.eps", sep=""), width=10, height=7.5, pointsize=12)
grafico = grafico + 1
plot(FCHI, main="CAC 40 (^FCHI) - Price")
dev.off() #Chiusura file postscript

# calculate log-returns for analysis
FCHI.ret = CalculateReturns(FCHI, method="log")

# remove first NA observation
FCHI.ret = FCHI.ret[-1,]
colnames(FCHI.ret) ="FCHI"

# plot returns
# Apertura file postscript
postscript(paste(grafico, " Grafico FCHI - Return.eps", sep=""), width=10, height=7.5, pointsize=12)
grafico = grafico + 1
plot(FCHI.ret, main="CAC 40 (^FCHI) - Return")
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
postscript(paste(grafico, " Grafico FTSE - Price.eps", sep=""), width=10, height=7.5, pointsize=12)
grafico = grafico + 1
plot(FTSE, main="FTSE 100 (^FTSE) - Price")
dev.off() #Chiusura file postscript

# calculate log-returns for analysis
FTSE.ret = CalculateReturns(FTSE, method="log")

# remove first NA observation
FTSE.ret = FTSE.ret[-1,]
colnames(FTSE.ret) ="FTSE"

# plot returns
# Apertura file postscript
postscript(paste(grafico, " Grafico FTSE - Return.eps", sep=""), width=10, height=7.5, pointsize=12)
grafico = grafico + 1
plot(FTSE.ret, main="FTSE 100 (^FTSE) - Return")
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
postscript(paste(grafico, " Grafico FTSEMIB.MI - Price.eps", sep=""), width=10, height=7.5, pointsize=12)
grafico = grafico + 1
plot(FTSEMIB.MI, main="FTSE MIB (FTSEMIB.MI) - Price")
dev.off() #Chiusura file postscript

# calculate log-returns for analysis
FTSEMIB.MI.ret = CalculateReturns(FTSEMIB.MI, method="log")

# remove first NA observation
FTSEMIB.MI.ret = FTSEMIB.MI.ret[-1,]
colnames(FTSEMIB.MI.ret) ="FTSEMIB.MI"

# plot returns
# Apertura file postscript
postscript(paste(grafico, " Grafico FTSEMIB.MI - Return.eps", sep=""), width=10, height=7.5, pointsize=12)
grafico = grafico + 1
plot(FTSEMIB.MI.ret, main="FTSE MIB (FTSEMIB.MI) - Return")
dev.off() #Chiusura file postscript

# ready to be processed
#STOCK.ret = FTSEMIB.MI.ret



#STOCK=data.frame(IXIC.ret, DJI.ret, GSPC.ret, XIN4.L.ret, GDAXI.ret, FCHI.ret, FTSEMIB.MI.ret)
#STOCK=data.frame(IXIC.ret, DJI.ret, GSPC.ret, GDAXI.ret, FCHI.ret, FTSEMIB.MI.ret)
#STOCK=c("IXIC.ret", "DJI.ret", "GSPC.ret", "GDAXI.ret", "FCHI.ret", "FTSEMIB.MI.ret")
STOCK=c("IXIC.ret", "DJI.ret", "GSPC.ret", "GDAXI.ret", "FCHI.ret", "FTSE.ret", "FTSEMIB.MI.ret")



# Gestione directory
########################################################################
mainDir=getwd()


########################################################################
# Elaborazione
########################################################################

for (i in STOCK){
  
  # Gestione directory
  ########################################################################
    setwd(mainDir)
    dir.create(i, showWarnings = FALSE)
    subDir=paste(i,"Workspace","", sep="/")
    dir.create(subDir, showWarnings = FALSE)
    setwd(file.path(mainDir, subDir))
    
    
  # for titolo
  ########################################################################  
    STOCK.ret = eval(parse(text=STOCK[index(STOCK)[which(i==STOCK)]],))
  
    
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
  backTestVaR.cond <- function(x, p = 0.95) {
    #0.95 e' il valore standard, ma lo si modifica se gli si passa un valore diverso al parametro
    normal.VaR = as.numeric(VaR.arigarch(x, p=p, d="norm"))
    tstudent.VaR = as.numeric(VaR.arigarch(x, p=p, d="std"))
    skewt.VaR = as.numeric(VaR.arigarch(x, p=p, d="sstd"))
    sged.VaR = as.numeric(VaR.arigarch(x, p=p, d="sged"))
    ans = cbind(normal.VaR, tstudent.VaR, skewt.VaR, sged.VaR)
    ans=zoo(ans, as.Date(index(x))[(w.e+1):n.obs])
    names(ans) = c("Normal", "t-stud", "skew-t", "sged")
    return(ans)
  }
  
  
  ########################################################################
  VaR.performance = matrix(0, nrow=12, ncol=14,
                           dimnames=list(c("Normal","","","t-stud","","",
                                           "skew-t","","","sged","",""),
                                         c("100 lambda",
                                           "Expected Exceeed","Actual Exceed",
                                           "%viol",
                                           "LR.uc.stat","LR.uc.p","LR.uc.decision",
                                           "LR.ind.stat","LR.ind.p","LR.ind.decision",
                                           "LR.cc.stat","LR.cc.p","LR.cc.decision",
                                           "avg(VaR)")))
  
  for (z in 1:3){
    alpha = 1-(lambda[z]/100)
    VaR.performance[c(z,z+3,z+6,z+9),"100 lambda"]=as.numeric(lambda[z])
    # rolling 1-step ahead estimates of VaR
    VaR.results = backTestVaR.cond(as.zoo(STOCK.ret), p=alpha)
    #p=0.99 
    #livello di confidenza alpha=0.01
    #head(VaR.results)
    #head(sort(as.numeric(STOCK.ret[1:1000])),20)
    #str(STOCK.ret)
    #prova=VaR.results
    #prova2=VaR.results
    #prova3=result
    #prova4=result
    #prova5=result
    #VaR.results=result
    #colnames(VaR.results) = "Normal"
    #VaR.results = lag(result_2, k=-1) 
    #VaR.results = lag(VaR.results, k=-1)
    #str(VaR.results)
    #head(VaR.results)
    #riaggiusto la serie per il confronto diretto Value at Risk vs rendimento osservato
    #per il calcolo delle violazioni 
    # Apertura file postscript
    postscript(paste(grafico, " Grafico", ".eps", sep=""), width=10, height=7.5, pointsize=12)
    grafico = grafico + 1
    chart.TimeSeries(merge(STOCK.ret, VaR.results), legend.loc="topright",
                     main=paste("VaR livello di significativita'? ", 1-alpha, sep=""))
    dev.off() #Chiusura file postscript
    
    
    # violation's table
    ########################################################################
    
    violations.mat = matrix(0, 4, 5)
    rownames(violations.mat) = c("Normal","t-stud", "skew-t", "sged")
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
        as.numeric(violations.mat[i, "Percent"]*100)
      # Apertura file postscript
      postscript(paste(grafico, " Grafico", ".eps", sep=""), width=10, height=7.5, pointsize=12)
      grafico = grafico + 1
      chart.TimeSeries(-VaR.results[,i], 
                       #legend.loc="topright",
                       main=paste("Unconditional AR(1)-IGARCH(1,1) - ", i,
                                  " - livello di significativita' ", 1-alpha, sep=""))
      dev.off() #Chiusura file postscript
    }
    violations.mat
    
    
   
    
    # Show Normal VaR violations
    ########################################################################
    
    normalVaR.violations = as.zoo(STOCK.ret[index(VaR.results), ]) < VaR.results[, "Normal"]
    violation.dates = index(normalVaR.violations[which(normalVaR.violations)])
    
    # plot violations
    # Apertura file postscript
    postscript(paste(grafico, " Grafico", ".eps", sep=""), width=10, height=7.5, pointsize=12)
    grafico = grafico + 1
    plot(as.zoo(STOCK.ret[index(VaR.results),]), col="blue", ylab="Return",
         main=paste("Violations Normal model: alpha=", 1-alpha, sep=""))
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
    avg.VaR = -mean(VaR.results[, "Normal"], na.rm=TRUE)
    VaR.performance[z, "avg(VaR)"] = as.numeric(avg.VaR)
    
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
    postscript(paste(grafico, " Grafico", ".eps", sep=""), width=10, height=7.5, pointsize=12)
    grafico = grafico + 1
    plot(as.zoo(STOCK.ret[index(VaR.results),]), col="blue", ylab="Return",
         main=paste("Violations Student-t model: alpha=", 1-alpha, sep=""))
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
    avg.VaR = -mean(VaR.results[, "t-stud"], na.rm=TRUE)
    VaR.performance[z+3, "avg(VaR)"] = as.numeric(avg.VaR)
    
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
    postscript(paste(grafico, " Grafico", ".eps", sep=""), width=10, height=7.5, pointsize=12)
    grafico = grafico + 1
    plot(as.zoo(STOCK.ret[index(VaR.results),]), col="blue", ylab="Return",
         main=paste("Violations Skew-t model: alpha=", 1-alpha, sep=""))
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
    levels(VaR.test[12])
    # LR tests for conditional coverage of exceedances
    VaR.test[13:17]
    VaR.performance[z+6, "LR.cc.stat"] = as.numeric(VaR.test[14])
    VaR.performance[z+6, "LR.cc.p"] = as.numeric(VaR.test[16])
    VaR.performance[z+6, "LR.cc.decision"] = as.numeric(VaR.test[17])
    # Average value of the VaR estimates
    avg.VaR = -mean(VaR.results[, "skew-t"], na.rm=TRUE)
    VaR.performance[z+6, "avg(VaR)"] = as.numeric(avg.VaR)
    
    #Esportazione risultati
    write.csv2(violations.mat, file=paste(1000+tabella, " - Skew-t violations - lambda ", (1-alpha)*100, ".csv", sep=""))
    tabella = tabella + 1 
    write.csv2(VaR.test, file=paste(1000+tabella, " - Skew-t VaR test - lambda ", (1-alpha)*100, ".csv", sep=""))
    tabella = tabella + 1
    write.csv2(avg.VaR, file=paste(1000+tabella, " - Skew-t avg.VaR - lambda ", (1-alpha)*100, ".csv", sep=""))
    tabella = tabella + 1  
  
    
    
  
    # Show sged VaR violations
    ########################################################################
    
    sgedVaR.violations = as.zoo(STOCK.ret[index(VaR.results), ]) < VaR.results[, "sged"]
    violation.dates = index(sgedVaR.violations[which(sgedVaR.violations)])
    
    # plot violations
    # Apertura file postscript
    postscript(paste(grafico, " Grafico", ".eps", sep=""), width=10, height=7.5, pointsize=12)
    grafico = grafico + 1
    plot(as.zoo(STOCK.ret[index(VaR.results),]), col="blue", ylab="Return",
         main=paste("Violations sged model: alpha=", 1-alpha, sep=""))
    abline(h=0)
    lines(VaR.results[, "sged"], col="black", lwd=2)
    lines(as.zoo(STOCK.ret[violation.dates,]), type="p", pch=16, col="red", lwd=2)
    dev.off() #Chiusura file postscript  
    
    
    # sged VaR Test uc - ind - cc - avg
    ########################################################################
    
    VaR.test = VaRTest(alpha=(1-alpha), 
                       actual=coredata(STOCK.ret[index(VaR.results),]), 
                       VaR=coredata(VaR.results[,"sged"]),
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
    avg.VaR = -mean(VaR.results[, "sged"], na.rm=TRUE)
    VaR.performance[z+9, "avg(VaR)"] = as.numeric(avg.VaR)
    
    #Esportazione risultati
    write.csv2(violations.mat, file=paste(1000+tabella, " - sged violations - lambda ", (1-alpha)*100, ".csv", sep=""))
    tabella = tabella + 1 
    write.csv2(VaR.test, file=paste(1000+tabella, " - sged VaR test - lambda ", (1-alpha)*100, ".csv", sep=""))
    tabella = tabella + 1
    write.csv2(avg.VaR, file=paste(1000+tabella, " - sged avg.VaR - lambda ", (1-alpha)*100, ".csv", sep=""))
    tabella = tabella + 1
  }
  
  write.csv2(VaR.performance, file="VaR Prediction Performance - GARCH models - AR(1)-IGARCH(1,1).csv")

}
