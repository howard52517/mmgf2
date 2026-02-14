
required_pkgs <- c("glmnet", "SuperLearner")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}


build_model <- function(data, dependent_variable, independent_variables, 
                        family = "gaussian", nonp = FALSE) {
  Y <- data[, dependent_variable]
  X <- data[, independent_variables, drop = FALSE]
  
  if (nonp == FALSE) {
    final_fit <- glm(Y ~ .^2, data = X, family = family)
  } else {
    sl_libs <- c("SL.mean", "SL.glm", "SL.glmnet", "SL.gam", "SL.xgboost")
    final_fit <- SuperLearner(Y = Y, X = X, family = family, SL.library = sl_libs)
  }
  
  return(final_fit)
}


predict_result <- function(model, newdata, independent_variables, nonp = FALSE) {
  X <- newdata[, independent_variables, drop = FALSE]
  
  if (nonp == FALSE) {
    pre_val <- predict(model, newdata = X, type = "response")
  } else {
    pre_val <- as.vector(predict(model, newdata = X, type = "response")$pred)
  }
  
  return(pre_val)
}


# Intervene the values of variables related to A in the data
change_A_related_variables <- function(data, vars, value) {
  A_index <- grep("^A", vars)
  
  for (index in A_index) {
    data[[vars[index]]] <- value
  }
  return(data)
}


# Predict probabilities of A, L, and M in data with different interventions
predict_ALM_probability <- function(data, vars, sc, nonp = FALSE) {
  
  vars_no_VY <- vars[!vars %in% c(latent_var, "Y")]
  
  df0 <- data.frame(rep(NA, nrow(data)))
  df1 <- data.frame(rep(NA, nrow(data)))
  
  models_name <- c()
  
  # add the prediction results from each model to the dataframe
  for (dep_var in vars_no_VY) { # which variable should be used as the response variable
    v_in <- grep(dep_var, vars) # where is the current variable located
    ind_var <- vars[1:(v_in-1)]
    ind_var <- modify_var(dep_var, ind_var, "G", sc)
    
    model <- build_model(data, dep_var, ind_var, "binomial", nonp)
    
    # predict using data with intervene A = 0
    prob01 <- predict_result(model, data_A0, ind_var, nonp) # probability that the response variable equals 1
    if (grepl("^A", dep_var)) { # if it is A, store the predicted value of A=0
      prob0 <- 1 - prob01
    } else { # if not A, store the predicted value of this variable's actual value in the data
      indi_1 <- data[, dep_var] == 1 # in the data, is the response variable equal to 1
      prob0 <- (prob01 * indi_1) + ((1 - prob01) * (1 - indi_1))
    }
    df0 <- cbind(df0, prob0)
    
    # predict using data with intervene A = 1
    prob11 <- predict_result(model, data_A1, ind_var, nonp) # probability that the response variable equals 1
    if (grepl("^A", dep_var)) { # if it is A, store the predicted value of A=1
      prob1 <- prob11
    } else { # if not A, store the predicted value of this variable's actual value in the data
      indi_1 <- data[, dep_var] == 1 # in the data, is the response variable equal to 1
      prob1 <- (prob11 * indi_1) + ((1 - prob11) * (1 - indi_1))
    }
    df1 <- cbind(df1, prob1)
    
    model_name <- paste("mod", dep_var, sep = "_")
    models_name <- c(models_name, model_name)
  }
  
  df0 <- df0[, -1, drop = FALSE]
  df1 <- df1[, -1, drop = FALSE]
  
  colnames(df0) <- models_name
  colnames(df1) <- models_name
  
  out <- list(pA0 = df0, pA1 = df1)
  return(out)
}


indicator_A <- function(data) {
  df <- data.frame(rep(NA, nrow(data)))
  col_names <- c()
  
  if (is_A_time_varying) {
    for (t in 1:tau) {
      var_name <- paste("A", t, sep = "_")
      
      I_0 <- as.numeric(data[[var_name]]==0)
      I_1 <- as.numeric(data[[var_name]]==1)
      
      df <- cbind(df, I_0, I_1)
      col_name <- paste0("IA", t, "_equ_", 0); col_names <- c(col_names, col_name)
      col_name <- paste0("IA", t, "_equ_", 1); col_names <- c(col_names, col_name)
    }
  } else {
    I_0 <- as.numeric(data["A"]==0)
    I_1 <- as.numeric(data["A"]==1)
    
    df <- cbind(df, I_0, I_1)
    col_name <- paste0("IA", "_equ_", 0); col_names <- c(col_names, col_name)
    col_name <- paste0("IA", "_equ_", 1); col_names <- c(col_names, col_name)
  }
  
  df <- df[, -1, drop = FALSE]
  colnames(df) <- col_names
  
  return(df)
}


new_part_to_H <- function(p_dfs, indicator_A_df, a_list, vars, t, v) {
  
  n <- nrow(p_dfs$pA0)
  new_part <- rep(1, n)
  
  for (u in 1:length(vars)) { 
    # u is L, M1, M2, ... 
    # v_t is the subscript variable of the current H
    if (u==v) {
      
      if (is_A_time_varying) {
        # calculate I(At=at) / pAt
        # Note: These are all values at a single point in time, 
        # as they are incrementally multiplied by the added portion.
        if (t<=tau) {
          col_name <- paste0("IA", t, "_equ_", a_list[u])
          indi_A <- indicator_A_df[[col_name]]
          
          pa <- paste0("pA", a_list[u])
          pA <- p_dfs[[pa]]
          mod_name <- paste("mod_A", t, sep = "_")
          pAt <- pA[[mod_name]]
          
          temp <- indi_A / pAt
        } else {
          temp <- rep(1, n)
        }
        
      } else {
        
        if (t==1) {
          col_name <- paste0("IA", "_equ_", a_list[u])
          indi_A <- indicator_A_df[[col_name]]
          
          pa <- paste0("pA", a_list[u])
          pA <- p_dfs[[pa]]
          pAt <- pA[["mod_A"]]
          
          temp <- indi_A / pAt
        } else {
          temp <- rep(1, n)
        }
        
      }
      
    } else if (u > v) {
      
      # calculate the product of special pA
      if (a_list[u] == a_list[v] | t==1) {
        temp <- rep(1, n)
      } else {
        pau <- paste0("pA", a_list[u])
        pav <- paste0("pA", a_list[v])
        mod_name <- paste("mod", vars[u], t-1, sep = "_")
        
        temp <- p_dfs[[pau]][[mod_name]] / p_dfs[[pav]][[mod_name]]
      }
      
    } else { # u < v
      
      # calculate the product of special pA
      if (a_list[u] == a_list[v]) {
        temp <- rep(1, n)
      } else {
        pau <- paste0("pA", a_list[u])
        pav <- paste0("pA", a_list[v])
        mod_name <- paste("mod", vars[u], t, sep = "_")
        
        temp <- p_dfs[[pau]][[mod_name]] / p_dfs[[pav]][[mod_name]]
      }
      
    }
    
    new_part <- new_part * temp
  }
  
  return(new_part)
}


calculate_H <- function(data, p_dfs, a_list) {
  indicator_A_df <- indicator_A(data)
  
  # besides V, A, and Y, what other variables are there
  vars <- c()
  if (is_L_present) {
    vars <- c(vars, "L")
  }
  for (i in 1:num_M) {
    Mi <- paste0("M", i)
    vars <- c(vars, Mi)
  }
  num_V <- length(vars)
  cum_H <- matrix(1, nrow = nrow(data), ncol = num_V) # set the initial values for each H
  
  df <- data.frame(rep(NA, nrow(data)))
  col_names <- c()
  
  # H_L, H_M_i
  for (t in 1:tau) {
    for (v in 1:num_V) {
      new_part <- new_part_to_H(p_dfs, indicator_A_df, a_list, vars, t, v)
      cum_H[, v] <- cum_H[, v] * new_part
      
      df <- cbind(df, cum_H[, v])
      col_name <- paste("H", vars[v], t, sep = "_")
      col_names <- c(col_names, col_name)
    }
  }
  
  # H_Y
  new_part <- new_part_to_H(p_dfs, indicator_A_df, a_list, vars, tau+1, 1)
  cum_H[, 1] <- cum_H[, 1] * new_part
  df <- cbind(df, cum_H[, 1])
  col_names <- c(col_names, "H_Y")
  
  df <- df[, -1, drop = FALSE]
  colnames(df) <- col_names
  
  return(df)
}


algo_main <- function(data, structure, a_list, flag, sc, nonp = FALSE) {
  
  # besides V, A, and Y, what other variables are there
  vars <- c()
  if (is_L_present) {
    vars <- c(vars, "L")
  }
  for (i in 1:num_M) {
    Mi <- paste0("M", i)
    vars <- c(vars, Mi)
  }
  num_V <- length(vars)
  
  # calculate E[Y]
  dep_var <- "Y"
  v_in <- grep(dep_var, structure) # where is the current variable located
  ind_var <- structure[1:(v_in-1)]
  
  ind_var <- modify_var(dep_var, ind_var, "Q", sc)
  
  # initial value
  newdata <- get(paste0("data_A", a_list[1]))
  model <- build_model(data, dep_var, ind_var, 
                       ifelse(is_Y_binary, "binomial", "gaussian"), nonp)
  pre_val <- predict_result(model, newdata, ind_var, nonp)
  
  # update
  if (flag){
    data["Q_Y"] <- pre_val
    if (is_Y_binary) {
      model <- glm(Y ~ 1 + offset(qlogis(Q_Y)), weights = H_Y, family = "binomial", data = data)
      eps <- coef(model)
      pre_val <- plogis(qlogis(pre_val) + eps)
      
    } else {
      model <- glm(Y ~ 1 + offset(Q_Y), weights = H_Y, data = data)
      eps <- coef(model)
      pre_val <- pre_val + eps
    }
    
  }
  
  # avoid having identical predicted values, which prevents the next model from fitting properly
  if (length(unique(pre_val))==1) {
    pre_val <- pre_val + rnorm(length(pre_val), 0, 10e-6)
  }
  
  Q_target <- "Q_Y"
  data[[Q_target]] <- pre_val
  
  
  for (t in tau:1) {
    for (v in num_V:1) {
      dep_var <- paste(vars[v], t, sep = "_")
      v_in <- grep(dep_var, structure) # where is the current variable located
      ind_var <- structure[1:(v_in-1)]
      
      ind_var <- modify_var(dep_var, ind_var, "Q", sc)
      
      # initial value
      newdata <- get(paste0("data_A", a_list[v]))
      model <- build_model(data, Q_target, ind_var, 
                           ifelse(is_Y_binary, "binomial", "gaussian"), nonp)
      pre_val <- predict_result(model, newdata, ind_var, nonp)
      
      Q_init <- paste("Q", dep_var, sep = "_")
      
      # update
      if (flag){
        data[[Q_init]] <- pre_val
        weights_col <- paste("H", dep_var, sep = "_")
        
        if (is_Y_binary) {
          formula <- as.formula(paste0(Q_target, " ~ 1 + offset(qlogis(", Q_init, "))"))
          model <- glm(formula, weights = data[, weights_col], family = "binomial", data = data)
          eps <- coef(model)
          pre_val <- plogis(qlogis(pre_val) + eps)
          
        } else {
          formula <- as.formula(paste0(Q_target, " ~ 1 + offset(", Q_init, ")"))
          model <- glm(formula, weights = data[, weights_col], data = data)
          eps <- coef(model)
          pre_val <- pre_val + eps
        }
        
      }
      
      # avoid having identical predicted values
      if (length(unique(pre_val))==1) {
        pre_val <- pre_val + rnorm(length(pre_val), 0, 10e-6)
      }
      
      Q_target <- Q_init
      data[[Q_target]] <- pre_val
    }
  }
  
  return(data)
}


# for time-fixed A, bin L, bin M
algo_GCB <- function(data, structure, a_list, copy_num, sc, nonp = FALSE) {
  
  new_Vs <- list()
  for (v in latent_var) {
    new_V <- sample(data[, v], copy_num, replace = TRUE)
    new_Vs[[v]] <- new_V
  }
  new_Vs[['A']] <- rep(1, copy_num)
  
  gen_data <- data.frame(new_Vs)
  
  
  # besides V, A, and Y, what other variables are there
  vars <- c()
  if (is_L_present) {
    vars <- c(vars, "L")
  }
  for (i in 1:num_M) {
    Mi <- paste0("M", i)
    vars <- c(vars, Mi)
  }
  num_V <- length(vars)
  
  for (t in 1:tau) {
    for (v in 1:num_V) {
      
      dep_var <- paste(vars[v], t, sep = "_")
      v_in <- grep(dep_var, structure) # where is the current variable located
      ind_var <- structure[1:(v_in-1)]
      ind_var <- modify_var(dep_var, ind_var, "G", sc)
      
      model <- build_model(data, dep_var, ind_var, "binomial", nonp)
      
      gen_data[, "A"] <- a_list[v]
      pre_p <- predict_result(model, gen_data, ind_var, nonp)
      
      gen_data[dep_var] <- rbinom(copy_num, 1, pre_p)
      
    }
  }
  
  dep_var <- "Y"
  v_in <- grep(dep_var, structure) # where is the current variable located
  ind_var <- structure[1:(v_in-1)]
  ind_var <- modify_var(dep_var, ind_var, "Q", sc)
  
  model <- build_model(data, dep_var, ind_var, "gaussian", nonp)
  
  gen_data[, "A"] <- a_list[1]
  gen_Y <- predict_result(model, gen_data, ind_var, nonp)
  
  return(mean(gen_Y))
}


calculate_EIF <- function(data, Q_TMLE) {
  # besides V, A, and Y, what other variables are there
  vars <- c()
  if (is_L_present) {
    vars <- c(vars, "L")
  }
  for (i in 1:num_M) {
    Mi <- paste0("M", i)
    vars <- c(vars, Mi)
  }
  num_V <- length(vars)
  
  EIF <- rep(0, nrow(data))
  
  EIF <- EIF + data[, "H_Y"]*(data[, "Y"] - data[, "Q_Y"])
  Q_target <- "Q_Y"
  
  for (t in tau:1) {
    for (v in num_V:1) {
      var <- paste(vars[v], t, sep = "_")
      Q_est <- paste("Q", var, sep = "_")
      H_var <- paste("H", var, sep = "_")
      
      EIF <- EIF + data[, H_var]*(data[, Q_target] - data[, Q_est])
      
      Q_target <- Q_est
    }
  }
  
  EIF <- EIF + (data[, Q_target] - Q_TMLE)
  
  return(EIF)
}

