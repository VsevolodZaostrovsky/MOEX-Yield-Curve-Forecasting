# LIBRARY IMPORT

graphics.off()

library(forecast)
library(tseries)
library(ggplot2)
library(Metrics)
library(vars)
library(urca)


# --------------------
# --------------------
# DATA CONFIG

dataset <- read_csv("FinancialEconometrics/General_ds_cleaned.csv",
                    col_types = cols(date = col_date(format = "%Y-%m-%d")))

dates_from_struct_ms <- c(657, 1384, 3943, 4502)
horizon_for_forecast <- 10

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


train_data <- dataset_yield[dates_from_struct_ms[1]:dates_from_struct_ms[2], ]

# 1) Посмотреть тест Грейнжера(причинность)
# 2) Еще раз проверить коинтеграцию
# 3) Найти лучшую модель по AIC
# 4) Проверить корреляцию остатков с помощью статистики Дербина
# 5) Обучиться на лучшей моделе



# пройдемся по лагам и посмотрим на AIC(макс = 4)
m <- VARselect(train_data, lag.max = 10, type = "const")
m$selection
# получаем, что лучший AIC на 5 лагах

var_model_to_fit <- VAR(train_data, p = 5, type = "const")
var_model_predict <- predict(var_model_to_fit, n.ahead = horizon_for_forecast)
var_model_predict$fc

