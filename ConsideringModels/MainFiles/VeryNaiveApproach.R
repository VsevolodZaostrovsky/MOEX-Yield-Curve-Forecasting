library(forecast)
library(tseries)
library(vars)
library(openxlsx)
library(urca)
library(ggplot2)
library(Metrics)
library(xtable)
library(Hmisc)
library(urca)


df_train <- read_csv("~/GitHubRepos/FinancialEconometrics/DataWork/Data/main_df_train.csv", 
                    col_types = cols(date = col_date(format = "%Y-%m-%d")))

df_test <- read_csv("~/GitHubRepos/FinancialEconometrics/DataWork/Data/main_df_test.csv", 
                    col_types = cols(date = col_date(format = "%Y-%m-%d")))

bonds <- data.frame(month3 = df_train$Y025, 
                   month6 = df_train$Y05,
                   month9 = df_train$Y075,
                   year1 = df_train$Y1,
                   year2 = df_train$Y2,
                   year3 = df_train$Y3,
                   year5 = df_train$Y5,
                   year7 = df_train$Y7,
                   year10 = df_train$Y10,
                   year15 = df_train$Y15,
                   year20 = df_train$Y20,
                   year30 = df_train$Y30)

latex(adf.test(bonds$month3),  file="adfbonds3")
latex(adf.test(bonds$month6), file="adfbonds6")
latex(adf.test(diff(bonds$month9), file="adfbonds9"))
latex(adf.test(bonds$year1), file="adfbonds12")
latex(adf.test(bonds$year2), file="adfbonds24")
latex(adf.test(bonds$year3), file="adfbonds36")
latex(adf.test(bonds$year5), file="adfbonds60")
latex(adf.test(bonds$year10), file="adfbonds120")
latex(adf.test(bonds$year15), file="adfbonds180")
latex(adf.test(bonds$year30), file="adfbonds360")


latex(adf.test(exp(diff(log(bonds$month3))) - 1), file="adfexpdifflog3")
latex(adf.test(exp(diff(log(bonds$month6))) - 1), file="adfexpdifflog6")
latex(adf.test(exp(diff(log(bonds$month9))) - 1), file="adfexpdifflog9")
latex(adf.test(exp(diff(log(bonds$year1))) - 1), file="adfexpdifflog12")
latex(adf.test(exp(diff(log(bonds$year2))) - 1), file="adfexpdifflog24")
latex(adf.test(exp(diff(log(bonds$year3))) - 1), file="adfexpdifflog36")
latex(adf.test(exp(diff(log(bonds$year5))) - 1), file="adfexpdifflog60")
latex(adf.test(exp(diff(log(bonds$year10))) - 1), file="adfexpdifflog120")
latex(adf.test(exp(diff(log(bonds$year15))) - 1), file="adfexpdifflog180")
latex(adf.test(exp(diff(log(bonds$year30))) - 1), file="adfexpdifflog360")


bonds <- data.frame(month3 = exp(diff(log(df_train$Y025))) - 1, 
                   month6 = exp(diff(log(df_train$Y05))) - 1,
                   month9 = exp(diff(log(df_train$Y075))) - 1,
                   year1 = exp(diff(log(df_train$Y1))) - 1,
                   year5 = exp(diff(log(df_train$Y5))) - 1,
                   year15 = exp(diff(log(df_train$Y15))) - 1,
                   year30 = exp(diff(log(df_train$Y30))) - 1)

bonds_test <- data.frame(month3 = exp(diff(log(df_test$Y025))) - 1, 
                    month6 = exp(diff(log(df_test$Y05))) - 1,
                    month9 = exp(diff(log(df_test$Y075))) - 1,
                    year1 = exp(diff(log(df_test$Y1))) - 1,
                    year5 = exp(diff(log(df_test$Y5))) - 1,
                    year15 = exp(diff(log(df_test$Y15))) - 1,
                    year30 = exp(diff(log(df_test$Y30))) - 1)


# Random Walk
rw3m <- arima(dUse$month3, order=c(0, 1, 0))
rw6m <- arima(dUse$month6, order=c(0, 1, 0))
rw9m <- arima(dUse$month9, order=c(0, 1, 0))
rw1y <- arima(dUse$year1, order=c(0, 1, 0))
rw5y <- arima(dUse$year5, order=c(0, 1, 0))
rw15y <- arima(dUse$year5, order=c(0, 1, 0))
rw30y <- arima(dUse$year15, order=c(0, 1, 0))


# Auto Arima
arima3m <- auto.arima(dUse$month3)
arima6m <- auto.arima(dUse$month6)
arima9m <- auto.arima(dUse$month9)
arima1y <- auto.arima(dUse$year1)
arima5y <- auto.arima(dUse$year5)
arima15y <- auto.arima(dUse$year5)
arima30y <- auto.arima(dUse$year15)

# Constant
с3m <- arima(dUse$month3, order=c(0,0,0))
с6m <- arima(dUse$month6, order=c(0,0,0))
с9m <- arima(dUse$month9, order=c(0,0,0))
с1y <- arima(dUse$year1, order=c(0,0,0))
с5y <- arima(dUse$year5, order=c(0,0,0))
с15y <- arima(dUse$year5, order=c(0,0,0))
с30y <- arima(dUse$year15, order=c(0,0,0))


# ARIMA and RW
dm.test(predict(arima15y, 30)$pred - bonds_test$year15[1:30], predict(rw15y, 30)$pred - bonds_test$year15[1:30])

mae(predict(arima15y, 30)$pred , bonds_test$year15[1:30])
mae(predict(rw15y, 30)$pred , bonds_test$year15[1:30])
mae(predict(с15y, 30)$pred , bonds_test$year15[1:30])

dm.test(predict(arima30y, 30)$pred - bonds_test$year30[1:30], predict(rw30y, 30)$pred - bonds_test$year30[1:30])

mae(predict(arima30y, 30)$pred , bonds_test$year30[1:30])
mae(predict(rw30y, 30)$pred , bonds_test$year30[1:30])
mae(predict(с30y, 30)$pred , bonds_test$year30[1:30])

plot(predict(arima3m, 30)$pred)
