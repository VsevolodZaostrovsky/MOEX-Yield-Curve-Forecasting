library(forecast)
library(tseries)
library(vars)
library(openxlsx)
library(urca)
library(ggplot2)
library(Metrics)
library(readr)
library(rugarch)
library(rmgarch)
library(quantmod)


df_train <- read_csv("~/GitHubRepos/FinancialEconometrics/DataWork/Data/main_df_train.csv", 
                     col_types = cols(date = col_date(format = "%Y-%m-%d")))

df_test <- read_csv("~/GitHubRepos/FinancialEconometrics/DataWork/Data/main_df_test.csv", 
                    col_types = cols(date = col_date(format = "%Y-%m-%d")))

testdata3m <- (exp(diff(log(df_test$Y025))) - 1)[1:40]

dUseST <- data.frame(month3 = 100 * (exp(diff(log(df_train$Y025[1:200]))) - 1), 
                     month6 = 100 * (exp(diff(log(df_train$Y05[1:200]))) - 1))


# Random Walk
rw3m <- arima(df_train$OPEC_PRICE, order=c(0, 1, 0))

summary(rw3m)

pRW3m <- predict(rw3m, 10)

pRW3m
testdata3m

# AutoARIMA
autoARIMA <- arima(dUseST$month3, order=c(1,1,1))

summary(autoARIMA)

pautoARIMA3m <- predict(autoARIMA, 40)

pautoARIMA3m
testdata3m

# VAR(1)
varST <- VAR(dUseST)

summary(varST)

pVAR <- predict(varST, h.ahead=50)
mae(testdata3m, pVAR$fcst$month3)

plot(pVAR$fcst$month3)

# GARCH

mSpec <- ugarchspec(variance.model = list(garchOrder = c(1, 1)), 
                    mean.model = list(armaOrder = c(3, 3)))

garch3m <- ugarchfit(spec = mSpec, 
               data = dUseST$month3)

pgarch3m <- ugarchforecast(garch3m, n.ahead = 40)

ugarchforecast(garch3m, n.ahead = 40)


# Results

mae(pRW3m$pred, testdata3m)
mae(pautoARIMA3m$pred, testdata3m)
mae(pVAR$fcst$month3, testdata3m)
mae(pgarch3m$ , testdata3m)

pautoARIMA3m$pred

