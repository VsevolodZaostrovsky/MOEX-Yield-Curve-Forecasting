library(forecast)
library(tseries)
library(vars)
library(openxlsx)
library(urca)
library(ggplot2)
library(Metrics)


df_train <- read_csv("~/GitHubRepos/FinancialEconometrics/DataWork/Data/main_df_train.csv", 
                     col_types = cols(date = col_date(format = "%Y-%m-%d")))

df_test <- read_csv("~/GitHubRepos/FinancialEconometrics/DataWork/Data/main_df_test.csv", 
                    col_types = cols(date = col_date(format = "%Y-%m-%d")))

dUseST <- data.frame(month3 = exp(diff(log(df_train$Y025))) - 1, 
                     month6 = exp(diff(log(df_train$Y05))) - 1,
                     month9 = exp(diff(log(df_train$Y075))) - 1)

dUseMT <- data.frame(month9 = exp(diff(log(df_train$Y075))) - 1,
                     month12 = exp(diff(log(df_train$Y1))) - 1,
                     year2 = exp(diff(log(df_train$Y2))) - 1)

dUseLT <- data.frame(year2 = exp(diff(log(df_train$Y2))) - 1,
                     year3 = exp(diff(log(df_train$Y3))) - 1,
                     year5 = exp(diff(log(df_train$Y5))) - 1)


# Random Walk
rw3m <- arima(dUseST$month3, order=c(1, 0, 0))
rw6m <- arima(dUseST$month6, order=c(1, 0, 0))
rw9m <- arima(dUseST$month9, order=c(1, 0, 0))

# AutoARIMA
auto_arima3m <- auto.arima(dUseST$month3)
auto_arima6m <- auto.arima(dUseST$month6)
auto_arima9m <- auto.arima(dUseST$month9)


# VAR(1)
varST <- VAR(dUseST, p = 1)


# VAR(1) vs RW vs AutoARIMA
pred_VAR <- predict(varST, n.ahead=50)
pred_rw9m <- predict(rw9m, n.ahead=50)
pred_auto_arima9m <- predict(auto_arima9m, n.ahead=50)

mae(pred_VAR$fcst$month9, df_test$Y075[0:50])
mae(pred_rw9m$pred, df_test$Y075[0:50])
mae(pred_auto_arima9m$pred, df_test$Y075[0:50])


