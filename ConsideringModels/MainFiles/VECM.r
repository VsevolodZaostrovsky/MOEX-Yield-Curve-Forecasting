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
library(urca)  # ca.jo, ur.df, finland
library(vars)  # vec2var
library(tsDyn) # VECM


# --------------------
# --------------------
# DATA CONFIG


df_train <- read_csv("~/GitHubRepos/FinancialEconometrics/DataWork/Data/main_df_train.csv", 
                     col_types = cols(date = col_date(format = "%Y-%m-%d")))

df_test <- read_csv("~/GitHubRepos/FinancialEconometrics/DataWork/Data/main_df_test.csv", 
                    col_types = cols(date = col_date(format = "%Y-%m-%d")))


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
        bonds$month3, bonds$month6, bonds$month9
    ),
    type = "trace", K = 2, ecdet = "none", spec = "longrun"
)
# не отвергается r<=3
summary(jotest_1)
adf.test(1.000000 * bonds$month3 - 2.238043 * bonds$month6 + 1.240642 * bonds$month9)

# COINTEGRATION 3m, 6m, 9m, 1y, 2y, 3y
jotest_2 <- ca.jo(
    data.frame(
        bonds$month3, bonds$month6, bonds$month9,
        bonds$year1, bonds$year2, bonds$year3
    ),
    type = "trace", K = 2, ecdet = "none", spec = "longrun"
)
# не отвергается r<=5
summary(jotest_2)
adf.test(bonds$month3 + 3.9705588 * bonds$month6 -17.8903858)

# COINTEGRATION y1, y2, y3
jotest_3 <- ca.jo(
    data.frame(
      bonds$year1, bonds$year2, bonds$year3 
    ),
    type = "trace", K = 2, ecdet = "none", spec = "longrun"
)
# r<=1 не отвергаем
summary(jotest_3)
adf.test(1.000000 * bonds$year1 - 2.787553 * bonds$year2 + 1.869056 * bonds$year3)

# COINTEGRATION 3m, 6m, 9m
jotest_3 <- ca.jo(
  data.frame(
    bonds$year5, bonds$year7, bonds$year10 
  ),
  type = "trace", K = 2, ecdet = "none", spec = "longrun"
)
# r<=1 не отвергаем
summary(jotest_3)
adf.test(1.000000 * bonds$year5 + 0.09400387 * bonds$year7 - 1.24520401 * bonds$year10)

# COINTEGRATION 5y, 7y, 10y
jotest_4 <- ca.jo(
    data.frame(
        bonds$year5, bonds$year7, bonds$year10
    ),
    type = "trace", K = 2, ecdet = "none", spec = "longrun"
)
# r=0 не отвергаем
summary(jotest_4)


# COINTEGRATION 3m, 6m, 9m, 1y, 2y, 3y
jotest_2 <- ca.jo(
  data.frame(
    bonds$month3,
    bonds$year3,
    bonds$year10
  ),
  type = "trace", K = 2, ecdet = "none", spec = "longrun"
)
# не отвергается r<=5
summary(jotest_2)
adf.test(bonds$month3 -5.874390 * bonds$year3 +6.258106*bonds$year10)

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

# VECM

b369 <- data.frame(month3 = df_train$Y025,
           month6 = df_train$Y05,
           month9 = df_train$Y075)


vec2var369 <- vec2var(jotest_1, r=1)

irf(vec2var369)
plot(irf(vec2var369))
pred_vec2var369 <- predict(vec2var369, n.ahead=30)
p3 <- exp(diff(log(pred_vec2var369$fcst$bonds.month3))) - 1
reald <- exp(diff(log(df_test$Y025[1:30]))) - 1
mae(p3, reald)

vec2var369 <- vec2var(jotest_1, r=2)
pred_vec2var369 <- predict(vec2var369, n.ahead=30)
p3 <- exp(diff(log(pred_vec2var369$fcst$bonds.month3))) - 1
reald <- exp(diff(log(df_test$Y025[1:30]))) - 1
mae(p3, reald)

p3 <- exp(diff(log(pred_vec2var369$fcst$bonds.month6))) - 1
reald <- exp(diff(log(df_test$Y05[1:30]))) - 1
mae(p3, reald)

p3 <- exp(diff(log(pred_vec2var369$fcst$bonds.month9))) - 1
reald <- exp(diff(log(df_test$Y075[1:30]))) - 1
mae(p3, reald)

jotest_2 <- ca.jo(
  data.frame(
    bonds$year1, bonds$year5, bonds$year15
  ),
  type = "trace", K = 2, ecdet = "none", spec = "longrun"
)
summary(jotest_2)

vec2var1515 <- vec2var(jotest_2, r=1)
pred_vec2var1515 <- predict(vec2var1515, n.ahead=30)
p3 <- exp(diff(log(pred_vec2var1515$fcst$bonds.year1))) - 1
reald <- exp(diff(log(df_test$Y1[1:30]))) - 1
mae(p3, reald)

p3 <- exp(diff(log(pred_vec2var1515$fcst$bonds.year5))) - 1
reald <- exp(diff(log(df_test$Y5[1:30]))) - 1
mae(p3, reald)

p3 <- exp(diff(log(pred_vec2var1515$fcst$bonds.year15))) - 1
reald <- exp(diff(log(df_test$Y15[1:30]))) - 1
mae(p3, reald)

# --------------------
# --------------------
# TO LATEX

latex(adf.test(combination_1),  file="COINTEGR_3m_6m_9m_1y_5y_10y.tex")
latex(adf.test(combination_2), file="COINTEGR__3m_6m_9m_1y_3y_5y_7y_10y.tex")
latex(adf.test(combination_3),  file="COINTEGR_3m_6m_9m.tex")
latex(adf.test(combination_4), file="COINTEGR_5y_7y_10y.tex")



# pp.test(bonds$month3 - 2.238505 * bonds$month6 + 1.241087 * bonds$month9)

# po.test(cbind(bonds$month3, -2.238505 * bonds$month6, 1.241087 * bonds$month9))