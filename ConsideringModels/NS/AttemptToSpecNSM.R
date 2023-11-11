library(statespacer)
library(readr)

# USA
data("FedYieldCurve")
y <- FedYieldCurve[FedYieldCurve$Month >= "1985-01-01" & FedYieldCurve$Month <= "2000-12-01",]


# Russia
FedYeildCurve1DayMonth <- read_csv("GitHubRepos/FinancialEconometrics/DataWork/Data/FedYeildCurve1DayMonth.csv", 
                                   col_types = cols(...1 = col_skip(), Month = col_date(format = "%Y-%m-%d")))
# View(FedYeildCurve1DayMonth)
y <- FedYeildCurve1DayMonth




years <- y$Month # Used for plots later on
y <- as.matrix(y[-1])

self_spec_list <- list()
self_spec_list$H_spec <- TRUE
# We have got 6 state parameters: 3 factors and 3 fixed means
self_spec_list$state_num <- 6
# In total we need 20 parameters:
#   1 for lambda
#   1 for sigma2 (H)
#   6 for the variance - covariance matrix Sigma_eta (Q)
#   9 for the vector autoregressive coefficient matrix Phi
#   3 for the means mu
self_spec_list$param_num <- 20
self_spec_list$R <- diag(1, 6, 6)
self_spec_list$P_inf <- matrix(0, 6, 6)
self_spec_list$state_only <- 4:6

self_spec_list$sys_mat_fun <- function(param) {
  
  # Maturities of the interest rates
  maturity <- c(3, 6, 12, 24, 36, 60, 84, 120)
  
  # The constant lambda
  lambda <- exp(2 * param[1])
  
  # The variance of the observation errors
  sigma2 <- exp(2 * param[2])
  H <- sigma2 * diag(1, 8, 8)
  
  # Z matrix corresponding to the factors
  lambda_maturity <- lambda * maturity
  z <- exp(-lambda_maturity)
  Z <- matrix(1, 8, 3)
  Z[, 2] <- (1 - z) / lambda_maturity
  Z[, 3] <- Z[, 2] - z
  
  # Variance of the state disturbances
  Q <- Cholesky(param = param[3:8], decompositions = FALSE, format = matrix(1, 3, 3))
  
  # Vector autoregressive coefficient matrix, enforcing stationarity
  Tmat <- CoeffARMA(A = array(param[9:17], dim = c(3, 3, 1)),
                    variance = Q,
                    ar = 1, ma = 0)$ar[,,1]
  
  # Initial uncertainty of the factors
  T_kronecker <- kronecker(Tmat, Tmat)
  Tinv <- solve(diag(1, dim(T_kronecker)[1], dim(T_kronecker)[2]) - T_kronecker)
  vecQ <- matrix(Q)
  vecPstar <- Tinv %*% vecQ
  P_star <- matrix(vecPstar, dim(Tmat)[1], dim(Tmat)[2])
  
  # Adding parts corresponding to the fixed means to the system matrices
  Z <- cbind(Z, matrix(0, 8, 3)) # Not used in the observation equation
  Q <- BlockMatrix(Q, matrix(0, 3, 3)) # Fixed, so no variance in its errors
  a1 <- matrix(param[18:20], 6, 1) # Fixed means go into the initial guess
  Tmat <- cbind(Tmat, diag(1, 3, 3) - Tmat)
  Tmat <- rbind(Tmat, cbind(matrix(0, 3, 3), diag(1, 3, 3)))
  P_star <- BlockMatrix(P_star, matrix(0, 3, 3))
  
  # Return the system matrices
  return(list(H = H, Z = Z, Tmat = Tmat, Q = Q, a1 = a1, P_star = P_star))
}

self_spec_list$transform_fun <- function(param) {
  lambda <- exp(2 * param[1])
  sigma2 <- exp(2 * param[2])
  means <- param[18:20]
  return(c(lambda, sigma2, means))
}

initial <- c(-1, -2, -1, -1, 1, 0, 0, 0, 4, 0, 0, 0, 3, 0, 0, 0, 2, 0, 0, 0)

fit <- statespacer(y = y,
                   self_spec_list = self_spec_list,
                   collapse = TRUE,
                   initial = initial,
                   method = "BFGS",
                   verbose = TRUE,
                   standard_errors = TRUE)

# The level beta_1
plot(years, fit$smoothed$a[, 1], type = 'l', 
     xlab = "year", ylab = "level of yield curve")



