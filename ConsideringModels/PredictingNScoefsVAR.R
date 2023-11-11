library(forecast)
library(tseries)
library(vars)
library(openxlsx)
library(urca)
library(ggplot2)
library(BVAR)
library(Metrics)
library(Hmisc)
library(readr)

coefsDrops <- read_csv("GitHubRepos/FinancialEconometrics/DataWork/Data/coefsDrops.csv", 
                       col_types = cols(...1 = col_integer()))



train <- data.frame(b0 = coefsDrops$beta0[1:60], 
                    b1 = coefsDrops$beta1[1:60],
                    b2 = coefsDrops$beta2[1:60],
                    tau = coefsDrops$tau[1:60])

test <- data.frame(b0 = coefsDrops$beta0[60:70], 
                   b1 = coefsDrops$beta1[60:70],
                   b2 = coefsDrops$beta2[60:70],
                   tau = coefsDrops$tau[60:70])

latex(adf.test(train$b0),  file="NSbeta0")
latex(adf.test(train$b1), file="NSbeta1")
latex(adf.test(train$b2),  file="NSbeta2")
latex(adf.test(train$tau), file="NStau")

jotest=ca.jo(data.frame(train$b0, train$b1, train$b2, train$tau), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

latex(adf.test(-0.138356703 * train$b0 + 0.128211478 * train$b1 + 0.400634805 * train$b2 + 0.002350888 * train$tau))

train <- data.frame(b0 = diff(coefsDrops$beta0[1:60]), 
                    b1 = diff(coefsDrops$beta1[1:60]),
                    b2 = diff(coefsDrops$beta2[1:60]),
                    tau = diff(coefsDrops$tau[1:60]))

test <- data.frame(b0 = diff(coefsDrops$beta0[60:70]), 
                   b1 = diff(coefsDrops$beta1[60:70]),
                   b2 = diff(coefsDrops$beta2[60:70]),
                   tau = diff(coefsDrops$tau[60:70]))

latex(adf.test(train$b0),  file="NSbeta0")
latex(adf.test(train$b1), file="NSbeta1")
latex(adf.test(train$b2),  file="NSbeta2")
latex(adf.test(train$tau), file="NStau")

# Random Walk
rwb0 <- arima(train$b0, order=c(0, 1, 0))
rwb1 <- arima(train$b1, order=c(0, 1, 0))
rwb2 <- arima(train$b2, order=c(0, 1, 0))
rwtau <- arima(train$tau, order=c(0, 1, 0))


# Auto Arima
arimab0 <- auto.arima(train$b0)
summary(arimab0) #(0,0,0)
arimab1 <- auto.arima(train$b1)
summary(arimab1) #(0,0,0)
arimab2 <- auto.arima(train$b2)
summary(arimab2) #(1,0,0)
arimatau <- auto.arima(train$tau)
summary(arimatau) #(0,0,0)

# VAR
var1 <- VAR(train, p=1)
var2 <- VAR(train, p=2)

# Comp b0

predvar <- predict(var1, n.ahead=10)
predvar2 <- predict(var2, n.ahead=10)
parimab0 <- predict(arimab0, 10)
prwb0 <- predict(rwb0, 10)

mae(parimab0$pred, test$b0)
mae(predvar$fcst$b0, test$b0)
mae(prwb0$pred, test$b0)

latex(dm.test(parimab0$pred - test$b0, prwb0$pred - test$b0), file="b0autoARIMAvsRW")
latex(dm.test(predvar$fcst$b0 - test$b0, parimab0$pred - test$b0), file="b0VARvsautoARIMA")

# Comp b1

predvar <- predict(var1, n.ahead=10)
parimab1 <- predict(arimab1, 10)
prwb1 <- predict(rwb1, 10)
mae(parimab1$pred, test$b1)
mae(predvar$fcst$b1, test$b1)
mae(prwb1$pred, test$b1)

latex(dm.test(parimab1$pred - test$b1, prwb1$pred - test$b1), file="b1autoARIMAvsRW")
latex(dm.test(predvar$fcst$b1 - test$b1, parimab1$pred - test$b1), file="b1VARvsautoARIMA")

# Comp b2

predvar <- predict(var1, n.ahead=10)
parimab2 <- predict(arimab2, 10)
prwb2 <- predict(rwb2, 10)
mae(parimab2$pred, test$b2)
mae(predvar$fcst$b2, test$b2)
mae(prwb2$pred, test$b2)

latex(dm.test(parimab2$pred - test$b2, prwb2$pred - test$b2), file="b2autoARIMAvsRW")
latex(dm.test(predvar$fcst$b2 - test$b2, parimab2$pred - test$b2), file="b2VARvsautoARIMA")

# Comp tau

predvar <- predict(var1, n.ahead=10)
parimatau <- predict(arimatau, 10)
prwtau <- predict(rwtau, 10)
mae(parimatau$pred, test$tau)
mae(predvar$fcst$tau, test$tau)
mae(prwtau$pred, test$tau)

latex(dm.test(parimatau$pred - test$tau, prwtau$pred - test$tau), file="tauautoARIMAvsRW")
latex(dm.test(predvar$fcst$tau - test$tau, parimatau$pred - test$tau), file="tauVARvsautoARIMA")
