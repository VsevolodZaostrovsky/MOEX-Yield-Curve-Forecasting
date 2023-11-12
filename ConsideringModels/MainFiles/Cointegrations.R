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


df_train <- read_csv("FinancialEconometrics/DataWork/Data/main_df_train.csv",
    col_types = cols(date = col_date(format = "%Y-%m-%d"))
)

df_test <- read_csv("FinancialEconometrics/DataWork/Data/main_df_test.csv",
    col_types = cols(date = col_date(format = "%Y-%m-%d"))
)

bonds <- data.frame(
    month3 = df_train$Y025,
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
    year30 = df_train$Y30
)

# --------------------
# --------------------
# COINTEGRATION Johansen test


# COINTEGRATION 3m, 6m, 9m, 1y, 5y, 10y
jotest_1 <- ca.jo(
    data.frame(
        bonds$month3, bonds$month6, bonds$month9,
        bonds$year1, bonds$year5, bonds$year10
    ),
    type = "trace", K = 2, ecdet = "none", spec = "longrun"
)
# не отвергается r<=3
summary(jotest_1)


# COINTEGRATION 3m, 6m, 9m, 1y, 3y, 5y, 7y, 10y
jotest_2 <- ca.jo(
    data.frame(
        bonds$month3, bonds$month6, bonds$month9,
        bonds$year1, bonds$year3, bonds$year5, 
        bonds$year7, bonds$year10
    ),
    type = "trace", K = 2, ecdet = "none", spec = "longrun"
)
# не отвергается r<=5
summary(jotest_2)


# COINTEGRATION 3m, 6m, 9m
jotest_3 <- ca.jo(
    data.frame(
        bonds$month3, bonds$month6, bonds$month9
    ),
    type = "trace", K = 2, ecdet = "none", spec = "longrun"
)
# r<=1 не отвергаем
summary(jotest_3)


# COINTEGRATION 5y, 7y, 10y
jotest_4 <- ca.jo(
    data.frame(
        bonds$year5, bonds$year7, bonds$year10
    ),
    type = "trace", K = 2, ecdet = "none", spec = "longrun"
)
# r=0 не отвергаем
summary(jotest_4)



# --------------------
# --------------------
# CHECK FOR AUGMENTED RESULTS 

# тест = -7
combination_1 <- bonds$month3 - 4.66479297 * bonds$month6 + 6.85170173 * bonds$month9 - 3.24545910 * bonds$year1 + 0.07975815 * bonds$year5 - 0.02602660  * bonds$year10
plot(combination_1, type = "l")
adf.test(combination_1)

# супер хорошо , тест = -9
combination_2 <- bonds$month3 - 23.783878 * bonds$month6 + 64.646041 * bonds$month9 - 47.632082 * bonds$year1 + 14.917770 * bonds$year3 - 19.691163 * bonds$year5 + 13.953181 * bonds$year7 -3.426449 * bonds$year10
plot(combination_2, type = "l")
adf.test(combination_2)

# тест = -4
combination_3 <- bonds$month3 - 2.238505 * bonds$month6 + 1.241087 * bonds$month9
plot(combination_3, type = "l")
adf.test(combination_3)

# тест = -4
combination_4 <- bonds$year5 + 0.7475718 * bonds$year7 - 1.9333898 * bonds$year10
plot(combination_4, type = "l")
adf.test(combination_4)


# --------------------
# --------------------
# TO LATEX

latex(adf.test(combination_1),  file="COINTEGR_3m_6m_9m_1y_5y_10y.tex")
latex(adf.test(combination_2), file="COINTEGR__3m_6m_9m_1y_3y_5y_7y_10y.tex")
latex(adf.test(combination_3),  file="COINTEGR_3m_6m_9m.tex")
latex(adf.test(combination_4), file="COINTEGR_5y_7y_10y.tex")



# pp.test(bonds$month3 - 2.238505 * bonds$month6 + 1.241087 * bonds$month9)

# po.test(cbind(bonds$month3, -2.238505 * bonds$month6, 1.241087 * bonds$month9))