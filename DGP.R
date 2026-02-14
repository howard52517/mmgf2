
coefs <- readRDS("coefs.rds")


lin_reg <- function(dat, coefs) {
  n <- ncol(dat)
  x <- as.vector(model.matrix(~ .^2, data = dat) %*% coefs[[n-3]])
  return(x)
}


expit <- function(dat, coefs) {
  x <- lin_reg(dat, coefs)
  return(exp(x)/(1+exp(x)))
}


datasim <- function(n, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  V1 <- rnorm(n, sd=0.5); V2 <- rnorm(n, sd=0.5) 
  V3 <- rnorm(n, sd=0.5); V4 <- rnorm(n, sd=0.5)
  dat <- data.frame(V1, V2, V3, V4)
  
  pr <- expit(dat, coefs)
  A <- rbinom(n, 1, pr)
  dat$A <- A
  
  pr <- expit(dat, coefs)
  L_1 <- rbinom(n, 1, pr)
  dat$L_1 <- L_1
  
  pr <- expit(dat, coefs)
  M1_1 <- rbinom(n, 1, pr)
  dat$M1_1 <- M1_1
  
  pr <- expit(dat, coefs)
  M2_1 <- rbinom(n, 1, pr)
  dat$M2_1 <- M2_1
  
  pr <- expit(dat, coefs)
  L_2 <- rbinom(n, 1, pr)
  dat$L_2 <- L_2
  
  pr <- expit(dat, coefs)
  M1_2 <- rbinom(n, 1, pr)
  dat$M1_2 <- M1_2
  
  pr <- expit(dat, coefs)
  M2_2 <- rbinom(n, 1, pr)
  dat$M2_2 <- M2_2
  
  mu <- lin_reg(dat, coefs)
  Y <- rnorm(n, mu, 5)
  dat$Y <- Y
  
  return(dat)
}

