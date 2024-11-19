
library(dplyr)

coefs <- readRDS("coefs.rds")

coefs[[1]][1:5] <- c(0.2, -0.3, 0.1, 0.25, -0.1) / 2
coefs[[2]][1:6] <- c(-1.6, 0.1, -0.8, 0.6, -1, 2) / 4
coefs[[3]][1:7] <- c(-1.5, 0.2, -0.9, 0.5, -1, 2, -0.1) / 4
coefs[[4]][1:8] <- c(-1.4, 0.1, -0.5, 0.3, -1, 2, -0.05, -0.1) / 4
coefs[[5]][1:9] <- c(-1, 0.1, -0.7, 0.7, -1, 1.5, -0.2, -0.05, 0.05) / 5
coefs[[6]][1:10] <- c(-0.8, 0.2, -0.7, 0.3, -1, 1.5, -0.05, -0.3, -0.08, 0.02) / 5
coefs[[7]][1:11] <- c(-0.6, 0.1, 0.3, 0.1, -1, 1.5, -0.03, -0.08, -0.2, 0.02, -0.1) / 5
coefs[[8]][1:12] <- c(5, 0.6, -0.3, 0.4, -0.8, 1, 0.3, -0.8, -0.2, 0.4, -0.5, -0.3)
coefs[[8]] <- coefs[[8]] * 10


lin_reg <- function(dat, coefs) {
  n <- ncol(dat)
  x <- model.matrix(~ .^2, data = dat) %*% coefs[[n-3]] %>% as.vector()
  return(x)
}

expit <- function(dat, coefs) {
  x <- lin_reg(dat, coefs)
  return(exp(x)/(1+exp(x)))
}


datasim <- function(n, seed) {
  set.seed(seed)

  V1 <- rnorm(n, sd=0.5); V2 <- rnorm(n, sd=0.5) 
  V3 <- rnorm(n, sd=0.5); V4 <- rnorm(n, sd=0.5)
  dat <- data.frame(V1, V2, V3, V4)

  pr <- expit(dat, coefs) #;print(max(pr));print(min(pr))
  A <- rbinom(n, 1, pr)
  dat$A <- A
  
  pr <- expit(dat, coefs) #;print(max(pr));print(min(pr))
  L_1 <- rbinom(n, 1, pr)
  dat$L_1 <- L_1
  
  pr <- expit(dat, coefs) #;print(max(pr));print(min(pr))
  M1_1 <- rbinom(n, 1, pr)
  dat$M1_1 <- M1_1
  
  pr <- expit(dat, coefs) #;print(max(pr));print(min(pr))
  M2_1 <- rbinom(n, 1, pr)
  dat$M2_1 <- M2_1
  
  pr <- expit(dat, coefs) #;print(max(pr));print(min(pr))
  L_2 <- rbinom(n, 1, pr)
  dat$L_2 <- L_2
  
  pr <- expit(dat, coefs) #;print(max(pr));print(min(pr))
  M1_2 <- rbinom(n, 1, pr)
  dat$M1_2 <- M1_2
  
  pr <- expit(dat, coefs) #;print(max(pr));print(min(pr))
  M2_2 <- rbinom(n, 1, pr)
  dat$M2_2 <- M2_2
  
  mu <- lin_reg(dat, coefs) #;print(max(mu));print(min(mu))
  Y <- rnorm(n, mu, 5)
  dat$Y <- Y
  
  return(dat)
}


Phi_MC <- function(a_list) {
  n <- 10000000
  Y_v <- c()
  for (i in 1:100) {
    V1 <- rnorm(n, sd=0.5); V2 <- rnorm(n, sd=0.5) 
    V3 <- rnorm(n, sd=0.5); V4 <- rnorm(n, sd=0.5)
    dat <- data.frame(V1, V2, V3, V4)
    
    dat$A <- rep(a_list[1], n)
    pr <- expit(dat, coefs)
    L_1 <- rbinom(n, 1, pr)
    dat$L_1 <- L_1
    
    dat$A <- rep(a_list[2], n)
    pr <- expit(dat, coefs)
    M1_1 <- rbinom(n, 1, pr)
    dat$M1_1 <- M1_1
    
    dat$A <- rep(a_list[3], n)
    pr <- expit(dat, coefs)
    M2_1 <- rbinom(n, 1, pr)
    dat$M2_1 <- M2_1
    
    dat$A <- rep(a_list[1], n)
    pr <- expit(dat, coefs)
    L_2 <- rbinom(n, 1, pr)
    dat$L_2 <- L_2
    
    dat$A <- rep(a_list[2], n)
    pr <- expit(dat, coefs)
    M1_2 <- rbinom(n, 1, pr)
    dat$M1_2 <- M1_2
    
    dat$A <- rep(a_list[3], n)
    pr <- expit(dat, coefs)
    M2_2 <- rbinom(n, 1, pr)
    dat$M2_2 <- M2_2
    
    dat$A <- rep(a_list[1], n)
    mu <- lin_reg(dat, coefs)
    Y <- rnorm(n, mu, 15)
    
    Y_v <- c(Y_v, mean(Y))
  }
  
  return(Y_v)
}


# Phi_111_v <- Phi_MC(c(1,1,1))
# Phi_000_v <- Phi_MC(c(0,0,0))
# Phi_100_v <- Phi_MC(c(1,0,0))
# Phi_101_v <- Phi_MC(c(1,0,1))

# library(glue)
# write.csv(
#   data.frame(
#     Phi_101 = Phi_101_v
#   ),
#   glue("Phi_101_MC.csv"),
#   row.names = FALSE
# )

# write.csv(
#   data.frame(
#     Phi_100 = Phi_100_v
#   ),
#   glue("Phi_100_MC.csv"),
#   row.names = FALSE
# )

# write.csv(
#   data.frame(
#     Phi_111 = Phi_111_v
#   ),
#   glue("Phi_111_MC.csv"),
#   row.names = FALSE
# )

# write.csv(
#   data.frame(
#     Phi_000 = Phi_000_v
#   ),
#   glue("Phi_000_MC.csv"),
#   row.names = FALSE
# )




# library(hal9001)
# 
# simdata <- datasim(500, 5)
# 
# dep_var_bin <- simdata[, "M2_2"]
# dep_var_cont <- simdata[, "Y"]
# ind_var <- simdata[, c("Z_1", "Z_2", "Z_3", "Z_4",
#                        "A", "L_1", "M1_1", "M2_1",
#                        "L_2", "M1_2")]
# 
# loop_r_start_time <- Sys.time()
# hal_fit_bin <- fit_hal(X=ind_var, Y=dep_var_bin, family="binomial", num_knots = 5)
# loop_r_end_time <- Sys.time()
# print(loop_r_end_time - loop_r_start_time)
# 
# 
# loop_r_start_time <- Sys.time()
# hal_fit_cont <- fit_hal(X=ind_var, Y=dep_var_cont, num_knots = 5)
# loop_r_end_time <- Sys.time()
# print(loop_r_end_time - loop_r_start_time)



