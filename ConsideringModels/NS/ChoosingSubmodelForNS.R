library(forecast)
library(tseries)
library(vars)
library(openxlsx)
library(urca)
library(ggplot2)
library(BVAR)
library(Metrics)
library(xtable)

library(readr)

coefsDrops <- read_csv("GitHubRepos/FinancialEconometrics/DataWork/Data/coefsDrops.csv", 
                       col_types = cols(...1 = col_integer()))

dUseST <- data.frame(b0 = coefsDrops$beta0[1:60], 
                     b1 = coefsDrops$beta1[1:60],
                     b2 = coefsDrops$beta2[1:60],
                     tau = coefsDrops$tau[1:60])

# Test data for statioary

adf.test(dUseST$b0)
adf.test(diff(dUseST$b0))
adf.test(dUseST$b1)
adf.test(diff(dUseST$b1))
adf.test(dUseST$b2)
adf.test(diff(dUseST$b2))
adf.test(dUseST$tau)
adf.test(diff(dUseST$tau))

# ADF shows that we should take the first difference

train <- data.frame(b0 = diff(coefsDrops$beta0[1:60]), 
                     b1 = diff(coefsDrops$beta1[1:60]),
                     b2 = diff(coefsDrops$beta2[1:60]),
                     tau = diff(coefsDrops$tau[1:60]))
                     
test <- data.frame(b0 = diff(coefsDrops$beta0[60:70]), 
                     b1 = diff(coefsDrops$beta1[60:70]),
                     b2 = diff(coefsDrops$beta2[60:70]),
                     tau = diff(coefsDrops$tau[60:70]))


# Beta 0
varST <- VAR(train, p=1)
pVAR <- predict(varST, h.ahead=10)

rw <- arima(train$b0, order=c(1,0,0))
prw <- predict(rw, 10)

aa <- auto.arima(train$b0)
paa <- predict(aa, 10)

mae(test$b0, pVAR$fcst$b0)
mae(test$b0, prw$pred)
mae(test$b0, paa$pred) # winner

summary(aa) # (0,0,0)

prw$pred


# Beta1

varST <- VAR(train, p=1)
pVAR <- predict(varST, h.ahead=10)

rw <- arima(train$b1, order=c(0,1,0))
prw <- predict(rw, 10)

aa <- auto.arima(train$b1)
paa <- predict(aa, 10)

mae(test$b1, pVAR$fcst$b1)
mae(test$b1, prw$pred)
mae(test$b1, paa$pred) # winner

summary(aa) # (0,0,0)

# Beta2

varST <- VAR(train, p=1)
pVAR <- predict(varST, h.ahead=10)

rw <- arima(train$b2, order=c(0,1,0))
prw <- predict(rw, 10)

aa <- auto.arima(train$b2)
paa <- predict(aa, 10)

mae(test$b2, pVAR$fcst$b2)
mae(test$b2, prw$pred)
mae(test$b2, paa$pred) # winner

summary(aa) # (1,0,0)


# tau

varST <- VAR(train, p=1)
pVAR <- predict(varST, h.ahead=10)

rw <- arima(train$tau, order=c(0,1,0))
prw <- predict(rw, 10)

aa <- auto.arima(train$tau)
paa <- predict(aa, 10)

mae(test$tau, pVAR$fcst$tau)
mae(test$tau, prw$pred)
mae(test$tau, paa$pred) # winner

summary(aa) # (0,0,0)


