library(forecast)
library(tseries)
library(vars)
library(openxlsx)
library(urca)
library(ggplot2)
library(Metrics)
library(readr)


df_train <- read_csv("~/GitHubRepos/FinancialEconometrics/DataWork/Data/main_df_train.csv", 
                     col_types = cols(date = col_date(format = "%Y-%m-%d")))

df_test <- read_csv("~/GitHubRepos/FinancialEconometrics/DataWork/Data/main_df_test.csv", 
                    col_types = cols(date = col_date(format = "%Y-%m-%d")))

m12 <- 1000 * (exp(diff(log(df_train$Y1))) - 1)

auto_arima12m <- auto.arima(m12)

pred_auto_arima12m <- predict(auto_arima12m, np=10)

summary(auto_arima12m)

summary(varST)

dUseST <- data.frame(month3 = 1000 * (exp(diff(log(df_train$Y025))) - 1), 
                   month6 = 1000 * (exp(diff(log(df_train$Y05))) - 1),
                   month9 = 1000 * (exp(diff(log(df_train$Y075))) - 1),
                   month12 = 1000 * (exp(diff(log(df_train$Y1))) - 1))

dUseLT <- data.frame(year5 = 1000 * (exp(diff(log(df_train$Y5))) - 1),
                   year7 = 1000 * (exp(diff(log(df_train$Y7))) - 1),
                   year10 = 1000 * (exp(diff(log(df_train$Y10))) - 1),
                   year15 = 1000 * (exp(diff(log(df_train$Y15))) - 1))


# Random Walk
# rw3m <- arima(dUseST$month3, order=c(0, 1, 0))
# rw6m <- arima(dUseST$month6, order=c(0, 1, 0))
rw9m <- arima(dUseST$month9, order=c(0, 1, 0))
rw12m <- arima(dUseST$month12, order=c(0, 1, 0))

rw5y <- arima(dUseLT$year5, order=c(0, 1, 0))
rw7y <- arima(dUseLT$year7, order=c(0, 1, 0))
rw10y <- arima(dUseLT$year10, order=c(0, 1, 0))
rw15y <- arima(dUseLT$year15, order=c(0, 1, 0))

# AutoARIMA
# auto_arima3m <- auto.arima(dUseST$month3)
# auto_arima6m <- auto.arima(dUseST$month6)
auto_arima9m <- auto.arima(dUseST$month9)
auto_arima12m <- auto.arima(dUseST$month12)

auto_arima5y <- auto.arima(dUseLT$year5)
auto_arima7y <- auto.arima(dUseLT$year7)
auto_arima10y <- auto.arima(dUseLT$year10)
auto_arima15y <- auto.arima(dUseLT$year15)


# VAR(1)
varST <- VAR(dUseST, p = 1)
varLT <- VAR(dUseLT, p = 1)


# VAR(1) vs RW vs AutoARIMA
pred_VAR <- predict(varST, 295)
pred_rw12m <- predict(rw12m, 295)
pred_auto_arima1y <- predict(auto_arima12m, 295)

realD <- 1000 * (exp(diff(log(df_test$Y1))) - 1)

mae(pred_VAR$fcst$month12, realD)
mae(pred_rw12m$pred, realD)
mae(pred_auto_arima1y$pred, realD)

plot(pred_VAR$fcst$month12)
plot(realD)
plot(pred_auto_arima1y$pred)
