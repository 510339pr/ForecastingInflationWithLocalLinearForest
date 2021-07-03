# Simulation study DGP
library(HDeconometrics)
library(randomForest)
library(grf)
library(glmnet)
library(ggplot2)

# large amount of data

set.seed(123)

n <- 600 # amount of observations
p <- 500 # amount of variables
sigma <- sqrt(500)

mu <- function(x) {

  sin(pi * x[1] * x[2]) + 20 * ((x[3] - 0.5) ** 2) + x[4] ** 3 + x[5] ** 4
  
}
# generate data
X <- matrix(runif(n * p, 0, 1), nrow = n)
Y <- apply(X, FUN = mu, MARGIN = 1) + sigma * rnorm(n)

X.test <- matrix(runif(n * p, 0, 1), nrow = n)
truth = apply(X.test, FUN = mu, MARGIN = 1)

# LLF predictions
LLF_rforest = ll_regression_forest(X, Y)
LLF_preds = predict(LLF_rforest, X.test)
LLF_rmse = mean((LLF_preds$predictions - truth)**2)
LLF_rmse

# RF predictions 
RF_rforest_grf <- regression_forest(X, Y)
RF_preds_grf <- predict(RF_rforest_grf, X.test)
RF_rmse_grf = mean((RF_preds_grf$predictions - truth)**2)
RF_rmse_grf

# small amount of data

n <- 600 # amount of observations
p <- 10 # amount of variables
sigma <- sqrt(500)

X <- matrix(runif(n * p, 0, 1), nrow = n)
Y <- apply(X, FUN = mu, MARGIN = 1) + sigma * rnorm(n)

X.test <- matrix(runif(n * p, 0, 1), nrow = n)
truth = apply(X.test, FUN = mu, MARGIN = 1)

# LLF predictions
LLF_rforest = ll_regression_forest(X, Y)
LLF_preds = predict(LLF_rforest, X.test)
LLF_rmse = mean((LLF_preds$predictions - truth)**2)
LLF_rmse

# RF - grf predictions 
RF_rforest_grf <- regression_forest(X, Y)
RF_preds_grf <- predict(RF_rforest_grf, X.test)
RF_rmse_grf = mean((RF_preds_grf$predictions - truth)**2)
RF_rmse_grf



