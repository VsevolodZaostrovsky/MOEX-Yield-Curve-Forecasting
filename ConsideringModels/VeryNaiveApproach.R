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

dUse <- data.frame(month3 = exp(diff(log(df_train$Y025))) - 1, 
                   month6 = exp(diff(log(df_train$Y05))) - 1,
                   month9 = exp(diff(log(df_train$Y075))) - 1,
                   year1 = exp(diff(log(df_train$Y1))) - 1,
                   year5 = exp(diff(log(df_train$Y5))) - 1,
                   year15 = exp(diff(log(df_train$Y15))) - 1,
                   year30 = exp(diff(log(df_train$Y30))) - 1)


# Random Walk
arima3m <- arima(dUse$month3, order=c(1, 0, 0))
arima6m <- arima(dUse$month6, order=c(1, 0, 0))
arima9m <- arima(dUse$month9, order=c(1, 0, 0))
arima1y <- arima(dUse$year1, order=c(1, 0, 0))
arima5y <- arima(dUse$year5, order=c(1, 0, 0))
arima15y <- arima(dUse$year5, order=c(1, 0, 0))
arima30y <- arima(dUse$year15, order=c(1, 0, 0))



# VAR(1)
mod <- VAR(dUse, p = 1)


# VAR(1) vs RW
predVAR <- predict(mod, n.ahead=296)
predarima1y <- predict(arima1y, n.ahead=296)

mae(predVAR$fcst$year1, df_test$Y1)
mae(predarima1y$pred, df_test$Y1)



