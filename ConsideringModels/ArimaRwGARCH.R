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
library(FAVAR)


df_train <- read_csv("~/GitHubRepos/FinancialEconometrics/DataWork/Data/main_df_train.csv", 
                     col_types = cols(date = col_date(format = "%Y-%m-%d")))

df_test <- read_csv("~/GitHubRepos/FinancialEconometrics/DataWork/Data/main_df_test.csv", 
                    col_types = cols(date = col_date(format = "%Y-%m-%d")))

testdata3m <- (exp(diff(log(df_test$Y025))) - 1)[1:40]

dUseST <- data.frame(month3 = 100 * (exp(diff(log(df_train$Y025))) - 1), 
                     month6 = 100 * (exp(diff(log(df_train$Y05))) - 1))

Nelson.Siegel( rate, maturity )

plot()

# Random Walk
rw3m <- arima(diff(df_train$OPEC_PRICE), order=c(1, 0, 0))

summary(rw3m)

pRW3m <- predict(rw3m, 40, se.fit=FALSE)

mae(testdata3m, pRW3m)

# VAR(2)
varST <- VAR(dUseST, p=2)

summary(varST)

pVAR <- predict(varST, h.ahead=100)
pVAR
mae(testdata3m, pVAR$fcst$month3)

plot(df_test$date[1:100], pVAR$fcst$month3[1:100], type='l')
plot(df_test$date[1:100], 100 * (exp(diff(log(df_train$Y025[1:101]))) - 1), type='l')

# GARCH

mSpec <- ugarchspec(variance.model = list(garchOrder = c(2, 1)), 
                    mean.model = list(armaOrder = c(1, 1)))

garch3m <- ugarchfit(spec = mSpec, 
               data = diff(dUseST$month3))

pgarch3m <- ugarchforecast(garch3m, n.ahead = 10)

ugarchforecast(garch3m, n.ahead = 10)

plot(ugarchforecast(garch3m, n.ahead))


# Results

mae(pRW3m$pred, testdata3m)
mae(pVAR$fcst$month3, testdata3m)
mae(pgarch3m$ , testdata3m)

pautoARIMA3m$pred



# AutoARIMA - lookss like it doesnt work at all with such data
autoARIMA <- arima(dUseST$month3, order=c(1,1,1))

pautoARIMA3m <- predict(autoARIMA, 10, se.fit=FALSE)

plot(pautoARIMA3m, type='l')
plot(testdata3m, type='l')


fit <- FAVAR(Y = diff(df_train$OPEC_PRICE),
             X = data.frame(y025=diff(df_train$Y025), diff(df_train$Y05), diff(df_train$Y075)), 
             fctmethod = 'BBE',
             factorprior = list(b0 = 0, vb0 = NULL, c0 = 0.01, d0 = 0.01),
             varprior = list(b0 = 0,vb0 = 10, nu0 = 0, s0 = 0),
             nrep = 15000, nburn = 5000, K = 2, plag = 2)

summary(fit)
library(patchwork)
dt_irf <- irf(fit, resvar = c(2,9,10))
