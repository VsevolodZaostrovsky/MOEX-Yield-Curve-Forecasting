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

jotest=ca.jo(data.frame(bonds$month3, bonds$month6, bonds$month9, bonds$year1, bonds$year5, bonds$year10), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

adf.test(bonds$month3 - 2.238505 * bonds$month6 + 1.241087 * bonds$month9)

pp.test(bonds$month3 - 2.238505 * bonds$month6 + 1.241087 * bonds$month9)

po.test(cbind(bonds$month3, -2.238505 * bonds$month6, 1.241087 * bonds$month9))


jotest=ca.jo(data.frame(bonds$year1, bonds$year2, bonds$year5, bonds$year10), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)
