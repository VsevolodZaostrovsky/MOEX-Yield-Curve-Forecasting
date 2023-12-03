library(forecast)
library(tseries)
library(vars)
library(openxlsx)
library(urca)
library(ggplot2)
library(midasr)
library(strucchange)
library(quantmod)
library(rusquant)
library(dplyr)
library(tsbox)
library(MSwM)


General_ds_cleaned <- read_csv("~/GitHubRepos/FinancialEconometrics/DataWork/Data/General_ds_cleaned.csv")

beta0 <- General_ds_cleaned$beta0
beta1 <- General_ds_cleaned$beta1
beta2 <- General_ds_cleaned$beta2

CUSUM_result <- efp(beta0 ~ 1)
MOSUM_result <- efp(beta0 ~ 1, type = "Rec-MOSUM")

CUSUM_result$process <- ts_ts(xts(x = CUSUM_result$process, 
                                  order.by = as.Date(General_ds_cleaned$date)))

plot(CUSUM_result, ylim = c(-10, 10))
plot(MOSUM_result)

bObj <- breakpoints(beta0 ~ 1, 
                    h = 0.05)

summary(bObj)

