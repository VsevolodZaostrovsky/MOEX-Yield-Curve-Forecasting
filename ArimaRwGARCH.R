# LIBRARY IMPORT
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
# ------------
library(xts)
library(PerformanceAnalytics)


# --------------------
# --------------------
# DATA CONFIG

# max value for this ~ 300
step_for_final_predict <- 11

df_train <- read_csv("FinancialEconometrics/DataWork/Data/main_df_train.csv",
    col_types = cols(date = col_date(format = "%Y-%m-%d"))
)
df_train$date <- as.Date(format(df_train$date, format = "%Y-%m-%d"),
                         format = "%Y-%m-%d")

df_test <- read_csv("FinancialEconometrics/DataWork/Data/main_df_test.csv",
    col_types = cols(date = col_date(format = "%Y-%m-%d"))
)
df_test$date <- as.Date(format(df_test$date, format = "%Y-%m-%d"),
                         format = "%Y-%m-%d")

testdata3m <- (exp(diff(log(df_test$Y025))) - 1)[1:step_for_final_predict]
testdata6m <- (exp(diff(log(df_test$Y05))) - 1)[1:step_for_final_predict]
testdata9m <- (exp(diff(log(df_test$Y075))) - 1)[1:step_for_final_predict]

dUseST <- data.frame(
    month3 = 100 * (exp(diff(log(df_train$Y025))) - 1),
    month6 = 100 * (exp(diff(log(df_train$Y05))) - 1),
    month9 = 100 * (exp(diff(log(df_train$Y075))) - 1)
)

dUseST_with_date <- data.frame(
    # date = df_train$date[-1],
    month3 = df_train$Y025,
    month6 = df_train$Y05,
    month9 = df_train$Y075
    # month3 = 100 * (exp(diff(log(df_train$Y025))) - 1),
    # month6 = 100 * (exp(diff(log(df_train$Y05))) - 1),
    # month9 = 100 * (exp(diff(log(df_train$Y075))) - 1)
)
dUseST_with_date <- xts(dUseST_with_date, order.by = df_train$date)
dUseST_with_date <- CalculateReturns(dUseST_with_date)[-1]



# --------------------
# --------------------
# CHECK MODELS

# Random Walk
rw3m <- arima(diff(df_train$OPEC_PRICE), order = c(1, 0, 0))

summary(rw3m)

pRW3m <- predict(rw3m, step_for_final_predict, se.fit = FALSE)

mae(testdata3m, pRW3m)



# VAR(2)
varST <- VAR(dUseST, p = 2)

summary(varST)

# pVAR <- predict(varST, h.ahead = 100)
pVAR <- predict(varST, h.ahead = step_for_final_predict)
pVAR
mae(testdata3m, pVAR$fcst$month3)

plot(df_test$date[1:step_for_final_predict],
     pVAR$fcst$month3[1:step_for_final_predict],
     type = "l")

plot(df_test$date[1:step_for_final_predict],
     100 * (exp(diff(log(df_train$Y025[1:(step_for_final_predict + 1)]))) - 1),
     type = "l")



# GARCH
curretly_df <- dUseST_with_date$month9
chartSeries(curretly_df)
# глянем настоящую вол
chart.RollingPerformance(R = curretly_df,
                         width = 25,
                         FUN = "sd.annualized",
                         scale = 252,
                         main = 'daily vol')

mSpec <- ugarchspec(
    variance.model = list(garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(1, 2)), distribution.model = "norm"
)

garch <- ugarchfit(
    spec = mSpec,
    data = curretly_df
)

tick_for_end_forecast <- 10
predict_garch <- ugarchforecast(fitORspec = garch, n.ahead = tick_for_end_forecast)
plot(sigma(predict_garch))

predict_final <- mSpec
setfixed(predict_final) <- as.list(coef(garch))

sim <- ugarchpath(spec = predict_final,
                  m.sim = 1,
                  n.sim = tick_for_end_forecast,
                  rseed = 16)

# запустить для общей наглядности, почти похожи 
plot.zoo(sigma(sim))
plot(ts(df_test$Y075[1:tick_for_end_forecast]))
mae(sigma(sim), df_test$Y075[1:tick_for_end_forecast])

# month9
# Garch | ARMA  | Akaike 
# (1,1) | (0,0) | 4.3146
# (1,1) | (1,0) | 4.2662
# (1,1) | (1,1) | 4.2606
# (1,1) | (1,2) | 4.2169
# (1,1) | (2,0) | 4.2612
# (1,1) | (2,1) | 4.2619
# (1,1) | (2,2) | 4.2551
# (1,2) | (1,1) | 4.2598
# (1,2) | (1,2) | 4.2213
# (2,1) | (1,2) | 4.2186
# (2,2) | (1,0) | 4.2671

# month6
# Garch | ARMA  | Akaike 
# (1,1) | (0,0) | 4.4247
# (1,1) | (1,0) | 4.3829
# (1,1) | (1,1) | 4.3909
# (1,1) | (1,2) | 4.3458
# (1,1) | (2,0) | 4.3908
# (1,1) | (2,1) | 4.3922
# (1,1) | (2,2) | 4.3653
# (1,2) | (1,1) | 4.3893
# (1,2) | (1,2) | 4.3460
# (2,1) | (1,2) | 4.3475
# (2,2) | (1,0) | 4.3959

# month3
# Garch | ARMA  | Akaike 
# (1,1) | (0,0) | 4.5829
# (1,1) | (1,0) | 4.5588
# (1,1) | (1,1) | 4.5560
# (1,1) | (1,2) | 4.5530
# (1,1) | (2,0) | 4.5547
# (1,1) | (2,1) | 4.5561
# (1,1) | (2,2) | 4.5550
# (1,2) | (1,1) | 4.5577
# (1,2) | (1,2) | 4.5547
# (2,2) | (1,0) | 4.5621




# --------------------
# --------------------
# RESULTS

mae(pRW3m$pred, testdata3m)
mae(pVAR$fcst$month3, testdata3m)
mae(pgarch3m$pred, testdata3m)

pautoARIMA3m$pred



# AutoARIMA - looks like it doesnt work at all with such data
autoARIMA <- arima(dUseST$month3, order = c(1, 1, 1))

pautoARIMA3m <- predict(autoARIMA, 10, se.fit = FALSE)

plot(pautoARIMA3m, type = "l")
plot(testdata3m, type = "l")


fit <- FAVAR(
    Y = diff(df_train$OPEC_PRICE),
    X = data.frame(y025 = diff(df_train$Y025), diff(df_train$Y05), diff(df_train$Y075)),
    fctmethod = "BBE",
    factorprior = list(b0 = 0, vb0 = NULL, c0 = 0.01, d0 = 0.01),
    varprior = list(b0 = 0, vb0 = 10, nu0 = 0, s0 = 0),
    nrep = 15000, nburn = 5000, K = 2, plag = 2
)

summary(fit)
library(patchwork)
dt_irf <- irf(fit, resvar = c(2, 9, 10))
