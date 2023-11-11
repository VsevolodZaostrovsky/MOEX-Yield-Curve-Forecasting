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

jotest=ca.jo(data.frame(bonds$month3, bonds$month6, bonds$month9), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

adf.test(bonds$month3 - 2.238505 * bonds$month6 + 1.241087 * bonds$month9)

pp.test(bonds$month3 - 2.238505 * bonds$month6 + 1.241087 * bonds$month9)

po.test(cbind(bonds$month3, -2.238505 * bonds$month6, 1.241087 * bonds$month9))

latex(adf.test(bonds$month3),  file="adfbonds3")
latex(adf.test(bonds$month6), file="adfbonds6")
latex(adf.test(diff(bonds$month9), file="adfbonds9")
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


# Random Walk
arima3m <- arima(dUse$month3, order=c(0, 1, 0))
arima6m <- arima(dUse$month6, order=c(0, 1, 0))
arima9m <- arima(dUse$month9, order=c(0, 1, 0))
arima1y <- arima(dUse$year1, order=c(0, 1, 0))
arima5y <- arima(dUse$year5, order=c(0, 1, 0))
arima15y <- arima(dUse$year5, order=c(0, 1, 0))
arima30y <- arima(dUse$year15, order=c(0, 1, 0))



# VAR(1)
mod <- VAR(dUse, p = 1)


# VAR(1) vs RW
predVAR <- predict(mod, n.ahead=296)
predarima1y <- predict(arima1y, n.ahead=296)

mae(predVAR$fcst$year1, df_test$Y1)
mae(predarima1y$pred, df_test$Y1)

