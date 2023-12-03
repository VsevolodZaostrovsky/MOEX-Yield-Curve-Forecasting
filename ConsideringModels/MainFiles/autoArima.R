# LIBRARY IMPORT

library(forecast)
library(tseries)
library(ggplot2)
library(Metrics)
library(xts)
library(seasonal)


# --------------------
# --------------------
# DATA CONFIG

dataset <- read_csv("FinancialEconometrics/General_ds_cleaned.csv",
                    col_types = cols(date = col_date(format = "%Y-%m-%d")))

dataset_yield <- data.frame(
                            month3 = exp(diff(log(dataset$"0.25"))) - 1,
                            month6 = exp(diff(log(dataset$"0.5"))) - 1,
                            month9 = exp(diff(log(dataset$"0.75"))) - 1,
                            year1 = exp(diff(log(dataset$"1.0"))) - 1,
                            year2 =exp(diff(log(dataset$"2.0"))) - 1,
                            year3 = exp(diff(log(dataset$"3.0"))) - 1,
                            year5 = exp(diff(log(dataset$"5.0"))) - 1,
                            year7 = exp(diff(log(dataset$"7.0"))) - 1,
                            year10 = exp(diff(log(dataset$"10.0"))) - 1,
                            year15 = exp(diff(log(dataset$"15.0"))) - 1,
                            year20 = exp(diff(log(dataset$"20.0"))) - 1,
                            year30 = exp(diff(log(dataset$"30.0"))) - 1)



