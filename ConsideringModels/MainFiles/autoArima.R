# LIBRARY IMPORT

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
library(readr)

# --------------------
# --------------------
# DATA CONFIG

dataset <- read_csv("FinancialEconometrics/General_ds_cleaned.csv")
