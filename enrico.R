######
#GROUP PROJECT FIRST FORECAST:
######

# setwd("C:/Users/enric/Documents/Documentazione utile alla valutazione/2026 R CODES")

rm(list=ls())

options("scipen"=100, "digits"=4) 

# Pacchetti usati nello script
required_packages <- c("fBasics", "fUnitRoots", "forecast", "fDMA", "fGarch", "timeSeries")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE)
}

# Carica il file scaricato (cambia il nome se diverso)
brent_data <- read.csv("DCOILBRENTEU.csv")

# Il CSV FRED usa "observation_date" come nome della colonna data
if ("observation_date" %in% names(brent_data)) {
  names(brent_data)[names(brent_data) == "observation_date"] <- "DATE"
}

# Trasforma la colonna DATE in formato data vero
brent_data$DATE <- as.Date(brent_data$DATE)

# Converte i prezzi in numeri (FRED a volte mette "." per i giorni festivi)
brent_data$DCOILBRENTEU <- as.numeric(as.character(brent_data$DCOILBRENTEU))

# Rimuove i giorni in cui il prezzo non è disponibile
brent_clean <- na.omit(brent_data)

prezzi_petrolio=brent_clean[,2]                                                

basicStats(prezzi_petrolio)
plot(prezzi_petrolio, type = "l")                                    


normalTest(prezzi_petrolio,method='jb')

# they are NOT normally distributed

jarqueberaTest(prezzi_petrolio)

Box.test(prezzi_petrolio,lag=15,type='Ljung')

par(mfcol=c(2,1))                    
Acf(prezzi_petrolio,lag=15)           #given the slow decay in ACF and the fact that the first lag in the PACF                                  
Pacf(prezzi_petrolio,lag=15)          #is close to 1, we conclude that there is a unit root
par(mfcol=c(1,1))                    

adfTest(prezzi_petrolio,lags=6,type="c") 
adfTest(prezzi_petrolio,lags=6,type="nc")
adfTest(prezzi_petrolio,lags=6,type="ct") #in fact, we fail to reject the null in all three cases: the series is non-stationary

ritorni=diff(prezzi_petrolio)
ritorni1 = diff(log(prezzi_petrolio))

t.test(ritorni) 
t.test(ritorni1) 
#true mean is zero in both cases, we do not need a constant

par(mfcol=c(2,1))
Acf(ritorni,lag=15)                                              #!!!!! visualizzazione grafica per capire quale processo stocastico è verosimile
Pacf(ritorni,lag=15) 
par(mfcol=c(1,1))



par(mfcol=c(2,1))
Acf(ritorni1,lag=15)                                              #!!!!! visualizzazione grafica per capire quale processo stocastico è verosimile
Pacf(ritorni1,lag=15) 
par(mfcol=c(1,1))

archtest(ritorni, lag = 15) #there is heteroskedasticity in returns
archtest(ritorni1, lag = 15)#there is heteroskedasticity

jarqueberaTest(ritorni)
jarqueberaTest(ritorni1) #both returns are not normally distributed

Box.test(ritorni,lag=15,type='Ljung')
Box.test(ritorni1,lag=15,type='Ljung') #both returns show autocorrelation


m1=ar(ritorni, aic = TRUE, order.max = 15, method = "mle")
m1$order #suggests an AR(6)
m2=ar(ritorni1, aic = TRUE, order.max = 15, method = "mle")
m2$order #suggests an AR(6)

auto.arima(ritorni, max.p = 6, max.q = 6, ic="aic") #selects an AR(6) with zero mean
auto.arima(ritorni1, max.p = 6, max.q = 6, ic="aic") #selects an ARMA(6,1) with zero mean

modritorni= arima(ritorni, order=c(6,0,0), include.mean = FALSE)
modritorni1= arima(ritorni1, order=c(6,0,1), include.mean = FALSE)

summary(modritorni)    
summary(modritorni1)   

confint(modritorni) #only AR(6) is statistically significant
confint(modritorni1) #only AR(6) is statistically significant

res1 =modritorni$residuals
res2 = modritorni1$residuals

plot(res1, type="l")
plot(res2, type="l")

jarqueberaTest(res1)
jarqueberaTest(res2) #both residuals are not normally distributed

Box.test(res1, lag = 15) #no autocorrelated residuals
Box.test(res2, lag = 15) #no autocorrelated residuals

Pacf(ritorni, lag=15)
Pacf(ritorni1, lag=15)

adfTest(ritorni,lags=6,type="nc") #the series is stationary
adfTest(ritorni1,lags=6,type="nc") #the series is stationary

archtest(res1, lag = 15)
archtest(as.vector(res1), lag = 15) #ARCH effects up to lag 15 are present
archtest(as.vector(res2), lag = 15) #ARCH effects up to lag 15 are presnt

# Then, all stochastic assumptions are not satisfied
# hence we cannot do inference according to the 
# std.errors in the estimation output.
# However, to answer the question, we use IC


Box.test(abs(ritorni),lag=15,type='Ljung')    #we reject the null in both cases and conclude that there is serial correlation
Box.test(abs(ritorni1),lag=15,type='Ljung')   #up to 15 lags in absolute log returns: a clear sign of heteroskedasticity

par(mfrow=c(2,1))
Acf(ritorni^2, lag.max = 15)
Pacf(ritorni^2, lag.max = 15) #ARCH effects up to lag 6, 13

par(mfrow=c(2,1))
Acf(ritorni1^2, lag.max = 15)
Pacf(ritorni1^2, lag.max = 15) #ARCH effects up to lag 6

Box.test(ritorni^2, lag=15,type='Ljung')
Box.test(ritorni1^2, lag=15,type='Ljung') #in both cases autocorrelation up to lag 15

fDMA::archtest(ritorni, lag = 15) 
fDMA::archtest(ritorni1, lag = 15) #in both cases reject the null of homoskedasticity, there are ARCH effects up to 15
#adjusted returns are heteroskedastic up to lag 15

ar(ritorni^2, method = "mle")$order #AR(9)process for conditional variance
ar(ritorni1^2, method = "mle")$order #AR(8) process for conditional variance
# hence, we should consider AR(6)-ARCH(9) or ARMA(6,1)-ARCH(8) without mean


cbind(AIC(modritorni), AIC(modritorni1))

cbind(BIC(modritorni), BIC(modritorni1)) #it is best to use log differences of oil prices. It suggested an ARMA(6,1)

library(fGarch)
library(timeSeries)

modvolatilityarch = garchFit(ritorni1 ~ arma(6,1)+garch(9,0), data = ritorni1, 
                include.mean = FALSE, trace = FALSE)

modvolatilitygarch = garchFit(ritorni1 ~ arma(6,1)+garch(1,1), data = ritorni1, 
                include.mean = FALSE, trace = FALSE)

c(modvolatilityarch@fit$ics[2], modvolatilitygarch@fit$ics[2])
modvolatilityarch@fit$ics*length(ritorni1)
modvolatilitygarch@fit$ics*length(ritorni1) #this model is selected as the best one according to all the ICs

std.res.modvolatilitygarch=(modvolatilitygarch@residuals)/modvolatilitygarch@sigma.t
jarqueberaTest(std.res.modvolatilitygarch) #standardized residuals are not normally distributed
Box.test(std.res.modvolatilitygarch, lag = 15, type = "L", fitdf = 7) 
Box.test(std.res.modvolatilitygarch^2, lag = 15, type = "L") #squared std residuals non sono correlati
archtest(std.res.modvolatilitygarch,lag = 15) #squared std residuals non sono correlati

p1=c(1,-modritorni$coef)
#    IMPORTANTE we are writing the characteristic equation

p1

r1=polyroot(p1)
# we find the zeros of a polynomial

r1
# because the characteristic equation of the fitted 
# AR(6) model contains complex roots, the model implies the existence of business cycles
