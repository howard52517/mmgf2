

set_robust <- 3

latent_var <- c("V1", "V2", "V3", "V4")


generate_causal_structure <- function() {
  # 提示用戶輸入相關資訊
  Y <- readline("Y(outcome)是二元變數嗎? (Y/N): ")
  M <- as.integer(readline("有幾個感興趣的M(mediator)? "))
  L <- readline("有L(time-varying confounder)嗎? (Y/N): ")
  A <- readline("A(exposure)是time-varying? (Y/N): ")
  tau <- as.integer(readline("tau(重複測量的次數)=? "))
  
  # 根據輸入，設定與因果結構相關的全域變數
  assign("num_M", M, envir = .GlobalEnv)
  assign("tau", tau, envir = .GlobalEnv)
  if (Y == "Y") {
    assign("is_Y_binary", TRUE, envir = .GlobalEnv)
  } else {
    assign("is_Y_binary", FALSE, envir = .GlobalEnv)
  }
  if (L == "Y") {
    assign("is_L_present", TRUE, envir = .GlobalEnv)
  } else {
    assign("is_L_present", FALSE, envir = .GlobalEnv)
  }
  if (A == "Y") {
    assign("is_A_time_varying", TRUE, envir = .GlobalEnv)
  } else {
    assign("is_A_time_varying", FALSE, envir = .GlobalEnv)
  }
  
  # 生成因果結構
  structure <- latent_var
  
  # 如果有 L
  if (is_L_present) {
    # 如果 A 是 time-varying
    if (is_A_time_varying) {
      # 加入 A、L、M1、M2
      for (i in 1:tau) {
        structure <- c(structure,
                       paste0("A_", i),
                       paste0("L_", i))
        for (j in 1:num_M) {
          structure <- c(structure,
                         paste0("M", j, "_", i))
        }
      }
    } else {
      # 只加入 A 一次
      structure <- c(structure, "A")
      
      # 加入 L、M1、M2
      for (i in 1:tau) {
        structure <- c(structure,
                       paste0("L_", i))
        for (j in 1:num_M) {
          structure <- c(structure,
                         paste0("M", j, "_", i))
        }
      }
    }
  } else {
    # 如果 A 是 time-varying
    if (is_A_time_varying) {
      # 加入 A、M1、M2
      for (i in 1:tau) {
        structure <- c(structure,
                       paste0("A_", i))
        for (j in 1:num_M) {
          structure <- c(structure,
                         paste0("M", j, "_", i))
        }
      }
    } else {
      # 只加入 A 一次
      structure <- c(structure, "A")
      
      # 加入 M1、M2
      for (i in 1:tau) {
        for (j in 1:num_M) {
          structure <- c(structure,
                         paste0("M", j, "_", i))
        }
      }
    }
  }
  
  # 加入 Y
  structure <- c(structure, "Y")
  assign("causal_structure", structure, envir = .GlobalEnv)
}


# scenario 1 : all correct
# scenario 2 : all Q correct but all G wrong
# scenario 3 : all G correct but all Q wrong
# scenario 4 : G of (A, L_1, M1_1, M2_1) correct but Q wrong;
# Q of (L_2, M1_2, M2_2, Y) correct but G wrong
# scenario 5: all wrong

modify_var <- function(ind_var, mod_type, sc) {
  
  if (sc==1) {
    return(ind_var)
  }
  
  if (sc==2) {
    if (mod_type=="Q") {
      return(ind_var)
    } else { # mod_type == "G"
      ind_var <- ind_var[-1:-2]
      return(ind_var)
    }
  }
  
  if (sc==3) {
    if (mod_type=="G") {
      return(ind_var)
    } else { # mod_type == "Q"
      ind_var <- ind_var[-1:-2]
      return(ind_var)
    }
  }
  
  if (sc==4) {
    if (mod_type=="Q") {
      if (length(ind_var)>=8) {
        return(ind_var)
      } else {
        ind_var <- ind_var[-1:-2]
        return(ind_var)
      }
    } else { # mod_type=="G"
      if (length(ind_var)<7) {
        return(ind_var)
      } else {
        ind_var <- ind_var[-1:-2]
        return(ind_var)
      }
    }
  }
  
  if (sc==5) {
    ind_var <- ind_var[-1:-2]
    return(ind_var)
  }
  
}


modify_var2 <- function(ind_var, dep_var, sc) {
  
  if (sc==5) {
    ind_var <- ind_var[-1:-2]
    return(ind_var)
  }

  if (grepl("^A", dep_var)) {
    return(ind_var)
  }
  
  if (sc==1) {
    return(ind_var)
  }
  
  if (sc==2) {
    if (grepl("^L", dep_var) | dep_var=="Y") {
      ind_var <- ind_var[-1:-2]
      return(ind_var)
    } else {
      return(ind_var)
    }
  }
  
  if (sc==3) {
    if (grepl("^M1", dep_var)) {
      ind_var <- ind_var[-1:-2]
      return(ind_var)
    } else {
      return(ind_var)
    }
  }
  
  if (sc==4) {
    if (grepl("^M2", dep_var)) {
      ind_var <- ind_var[-1:-2]
      return(ind_var)
    } else {
      return(ind_var)
    }
  }
  
}


modify_var3 <- function(dep_var, ind_var, mod_type, sc) {
  
  if (sc==0) {
    return(ind_var)
  }
  
  if (sc==12) {
    ind_var <- ind_var[-1:-2]
    return(ind_var)
  }

  if (sc %in% 1:8) {
    cutpoint <- sc + 4
    
    if (mod_type=="Q") {
      if (length(ind_var)>=cutpoint) {
        return(ind_var)
      } else {
        ind_var <- ind_var[-1:-2]
        return(ind_var)
      }
    } else { # mod_type=="G"
      if (length(ind_var)<(cutpoint-1)) {
        return(ind_var)
      } else {
        ind_var <- ind_var[-1:-2]
        return(ind_var)
      }
    }
  }
  
  
  if (sc==9) {
    if (grepl("^L", dep_var) | dep_var=="Y") {
      ind_var <- ind_var[-1:-2]
      return(ind_var)
    } else {
      return(ind_var)
    }
  }
  
  if (sc==10) {
    if (grepl("^M1", dep_var)) {
      ind_var <- ind_var[-1:-2]
      return(ind_var)
    } else {
      return(ind_var)
    }
  }
  
  if (sc==11) {
    if (grepl("^M2", dep_var)) {
      ind_var <- ind_var[-1:-2]
      return(ind_var)
    } else {
      return(ind_var)
    }
  }
  
}


##########################################################################################################



# build_model <- function(data, dependent_variable, independent_variables, model_name) {
#   formula <- as.formula(paste(dependent_variable, "~", paste(independent_variables, collapse = " + ")))
#   model <- glm(formula, data = data, family = "binomial")
#   assign(model_name, model, envir = .GlobalEnv)
# }


library(tidyverse) # for data manipulation
library(SuperLearner) # for ensemble learning

library(glmnet)
library(ranger)
library(earth)
library(hal9001)


SL.glm2 <- function(Y, X, newX, family = gaussian(), ...) {

  fit <- glm(Y ~ .^2, data = X, family = family)

  pred <- predict(fit, newX, type = "response")

  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c(" ", class(fit))
  return(out)
}

predict.SL.glm2 <- function(object, newdata, ...) {
  predict(object$fit, newdata = as.data.frame(newdata), type = "response")
}



# simulated_data_test <- simulated_data
# simulated_data_test$M2_1 <- simulated_data_test$M2_1 %>% as.numeric() - 1
# 
# dependent_variable <- c("M2_1")
# independent_variables <- c("C", "A", "L_1", "M1_1")
# 
# dependent_variable <- c("A")
# independent_variables <- c("C")
# 
# Y <- simulated_data_test[, dependent_variable]
# X <- simulated_data_test[, independent_variables, drop = FALSE]
# 
# testmod <- glm(Y ~ .^2,data = X, family = binomial())
# testmod <- SuperLearner(Y = Y, X = X, family = binomial(), SL.library = sl_libs)
# summary(testmod)
# 
# testmod$fit
# 
# newdata <- data_A_int[[paste0("data_A", 0)]]
# 
# predict(testmod, newdata = newdata, type = "response") %>% head()
# 
# testpred <- predict(testmod, newdata = data_A0, type = "response")$pred %>% as.vector()
# testpred <- qlogis(testpred)
# testmod <- SuperLearner(Y = testpred, X = X, SL.library = sl_libs)
# 
# testmod <- fit_hal(X, Y, family = binomial())
# 
# indx <- sapply(simulated_data_test, is.factor)
# simulated_data_test[indx] <- lapply(simulated_data_test[indx], function(x) as.numeric(as.character(x)))


# sl_libs <- c('SL.hal9001', 'SL.glmnet', 'SL.glm')



# 建立模型
build_model <- function(data, dependent_variable, independent_variables, family = "gaussian") {
  Y <- data[, dependent_variable]
  X <- data[, independent_variables, drop = FALSE]
  # X_interaction <- model.matrix(~ .^2, data = X)[, -1]
  # 
  # cv_fit <- cv.glmnet(x = X_interaction, y = Y, family = family, alpha = 1)
  # best_lambda <- cv_fit$lambda.min
  # final_fit <- glmnet(x = X_interaction, y = Y, family = family, alpha = 1, lambda = best_lambda)
  
  # final_fit <- glm(Y ~ .^2, data = X, family = family)
  
  sl_libs <- c('SL.glmnet', 'SL.xgboost')
  final_fit <- SuperLearner(Y = Y, X = X, family = family, SL.library = sl_libs)
  
  return(final_fit)
}

# 預測結果
predict_result <- function(model, newdata, independent_variables) {
  X <- newdata[, independent_variables, drop = FALSE]
  # X_interaction <- model.matrix(~ .^2, data = X)[, -1]
  # 
  # pre_val <- as.vector(predict(model, newx = X_interaction, type = "response"))
  
  pre_val <- predict(model, newdata = X, type = "response")$pred %>% as.vector()
  return(pre_val)
}



# 建立計算H所需的 A, L, M 模型
# build_ALM_models <- function(data, vars, sc) {
#   # vars 去除 C, Y
#   vars_no_CY <- vars[!vars %in% c(latent_var, "Y")]
#   
#   models_name <- c()
#   for (var in vars_no_CY) { # 以哪個變數作為應變數建立模型
#     v_in <- grep(var, vars) # 目前的變數在哪個位置
#     indep_vars <- vars[1:(v_in-1)]
#     
#     if (set_robust==1) {
#       indep_vars <- modify_var(indep_vars, "G", sc)
#     } else if (set_robust==2) {
#       indep_vars <- modify_var2(indep_vars, var, sc)
#     } else if (set_robust==3) {
#       indep_vars <- modify_var3(indep_vars, "G", sc)
#     }
#     
#     model_name <- paste("mod", var, sep = "_")
#     build_model(data, var, indep_vars, model_name)
#     models_name <- c(models_name, model_name)
#   }
#   
#   return(models_name)
# }


##########################################################################################################


# 修改資料中與 A 有關的變數值
change_A_related_variables <- function(data, vars, value) {
  A_index <- grep("^A", vars) # A在哪些位置
  
  # 修改 A 相關的變數值
  for (index in A_index) {
    data[[vars[index]]] <- value
  }
  return(data)
}


# A, L, M 在不同介入的資料中的預測的結果
predict_ALM_probability <- function(data, vars, sc) {
  
  # vars 去除 C, Y
  vars_no_CY <- vars[!vars %in% c(latent_var, "Y")]
  
  # 創建初始dataframe
  df0 <- data.frame(rep(NA, nrow(data)))
  df1 <- data.frame(rep(NA, nrow(data)))
  
  models_name <- c()

  # 將各個模型預測的結果加入dataframe
  for (dep_var in vars_no_CY) { # 以哪個變數作為應變數
    v_in <- grep(dep_var, vars) # 目前的變數在哪個位置
    ind_var <- vars[1:(v_in-1)]
    
    if (set_robust==1) {
      ind_var <- modify_var(ind_var, "G", sc)
    } else if (set_robust==2) {
      ind_var <- modify_var2(ind_var, dep_var, sc)
    } else if (set_robust==3) {
      ind_var <- modify_var3(dep_var, ind_var, "G", sc)
    }
    
    model <- build_model(data, dep_var, ind_var, "binomial")
    
    # 用 intervene A = 0 的資料預測
    prob01 <- predict_result(model, data_A0, ind_var) # 應變數 = 1 的機率
    if (grepl("^A", dep_var)) { # 如果是 A ，要存 A=0 的預測值
      prob0 <- 1 - prob01
    } else { # 如果不是 A ， 要存此變數在資料中實際的值的預測值
      indi_1 <- data[, dep_var] == 1 # 在資料中，應變數是否為 1
      prob0 <- (prob01 * indi_1) + ((1 - prob01) * (1 - indi_1))
    }
    df0 <- cbind(df0, prob0)
    
    # 用 intervene A = 1 的資料預測
    prob11 <- predict_result(model, data_A1, ind_var) # 應變數 = 1 的機率
    if (grepl("^A", dep_var)) { # 如果是 A ，要存 A=1 的預測值
      prob1 <- prob11
    } else { # 如果不是 A ， 要存此變數在資料中實際的值的預測值
      indi_1 <- data[, dep_var] == 1 # 在資料中，應變數是否為 1
      prob1 <- (prob11 * indi_1) + ((1 - prob11) * (1 - indi_1))
    }
    df1 <- cbind(df1, prob1)
    
    model_name <- paste("mod", dep_var, sep = "_")
    models_name <- c(models_name, model_name)
  }

  # 刪除第一行空值
  df0 <- df0[, -1, drop = FALSE]
  df1 <- df1[, -1, drop = FALSE]
  # 以預測的模型當作column names
  colnames(df0) <- models_name
  colnames(df1) <- models_name
  
  out <- list(pA0 = df0, pA1 = df1)
  return(out)
}


##########################################################################################################


# 計算I(A=a)
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


# H要新增的部分
new_part_to_H <- function(p_dfs, indicator_A_df, a_list, vars, t, v) {
  
  n <- nrow(p_dfs$pA0)
  new_part <- rep(1, n)
  
  for (u in 1:length(vars)) { # u 是 L, M1, M2, ... # v_t 是目前 H 的下標變數
    if (u==v) {
      
      if (is_A_time_varying) {
        # 計算 I(At=at) / pAt   note: 這裡都是單一時間點的值，因為是逐步乘上新增的部分。
        
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
      
      # 計算特殊pA連乘的部分
      if (a_list[u] == a_list[v] | t==1) {
        temp <- rep(1, n)
      } else {
        pau <- paste0("pA", a_list[u])
        pav <- paste0("pA", a_list[v])
        mod_name <- paste("mod", vars[u], t-1, sep = "_")
        
        temp <- p_dfs[[pau]][[mod_name]] / p_dfs[[pav]][[mod_name]]
      }
      
    } else { # u < v
      
      # 計算特殊pA連乘的部分
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

# 計算H
calculate_H <- function(data, p_dfs, a_list) {
  indicator_A_df <- indicator_A(data) # 先計算I(A=a)存下來
  
  # 除了C,A,Y有哪些變數
  vars <- c()
  if (is_L_present) {
    vars <- c(vars, "L")
  }
  for (i in 1:num_M) {
    Mi <- paste0("M", i)
    vars <- c(vars, Mi)
  }
  num_V <- length(vars) # 除了 C,A,Y 有幾種變數
  cum_H <- matrix(1, nrow = nrow(data), ncol = num_V) # 設定各個H的初始值
  
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

# 根據對H的觀察，計算流程如下
# 以 L,M1,M2 為例
# Hv <- c(1, 1, 1)
# Hv[1] <- Hv[1] * (I/p)
# Hv[2] <- Hv[2] * (I/p) * pH1
# Hv[3] <- Hv[3] * (I/p) * pH2 * pH1
# Hv[1] <- Hv[1] * (I/p) * pH3 * pH2
# Hv[2] <- Hv[2] * (I/p) * pH1 * pH3
# Hv[3] <- Hv[3] * (I/p) * pH2 * pH1

# Hv[1] <- Hv[1] * (I/p) * pH2 * pH3
# Hv[2] <- Hv[2] * pH1 * (I/p) * pH3
# Hv[3] <- Hv[3] * pH1 * pH2 * (I/p)


##########################################################################################################


algo_main <- function(data, structure, a_list, flag, sc) {
  
  # 除了C,A,Y有哪些變數
  vars <- c()
  if (is_L_present) {
    vars <- c(vars, "L")
  }
  for (i in 1:num_M) {
    Mi <- paste0("M", i)
    vars <- c(vars, Mi)
  }
  num_V <- length(vars) # 除了 C,A,Y 有幾種變數
  
  # 計算E[Y]
  dep_var <- "Y"
  v_in <- grep(dep_var, structure) # 目前的變數在哪個位置
  ind_var <- structure[1:(v_in-1)]
  
  if (set_robust==1) {
    ind_var <- modify_var(ind_var, "Q", sc)
  } else if (set_robust==2) {
    ind_var <- modify_var2(ind_var, dep_var, sc)
  } else if (set_robust==3) {
    ind_var <- modify_var3(dep_var, ind_var, "Q", sc)
  }
  
  # initial value
  newdata <- get(paste0("data_A", a_list[1]))
  model <- build_model(data, dep_var, ind_var, ifelse(is_Y_binary, "binomial", "gaussian"))
  pre_val <- predict_result(model, newdata, ind_var)

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
    
    
    # 計算IPW Estimator
    Q_IPW <- data$Y * data$H_Y
    data["Q_IPW"] <- Q_IPW
  }
  
  # 避免預測值都一樣 導致下一個模型無法擬合
  if (length(unique(pre_val))==1) {
    pre_val <- pre_val + rnorm(length(pre_val), 0, 10e-6)
  }
  
  Q_target <- "Q_Y"
  data[[Q_target]] <- pre_val
  
  
  for (t in tau:1) {
    for (v in num_V:1) {
      dep_var <- paste(vars[v], t, sep = "_")
      v_in <- grep(dep_var, structure) # 目前的變數在哪個位置
      ind_var <- structure[1:(v_in-1)]
      
      if (set_robust==1) {
        ind_var <- modify_var(ind_var, "Q", sc)
      } else if (set_robust==2) {
        ind_var <- modify_var2(ind_var, dep_var, sc)
      } else if (set_robust==3) {
        ind_var <- modify_var3(dep_var, ind_var, "Q", sc)
      }
      
      # initial value
      newdata <- get(paste0("data_A", a_list[v]))
      model <- build_model(data, Q_target, ind_var, ifelse(is_Y_binary, "binomial", "gaussian"))
      pre_val <- predict_result(model, newdata, ind_var)
      
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
      
      # 避免預測值都一樣 導致下一個模型無法擬合
      if (length(unique(pre_val))==1) {
        pre_val <- pre_val + rnorm(length(pre_val), 0, 10e-6)
      }
      
      Q_target <- Q_init
      data[[Q_target]] <- pre_val
    }
  }
  
  # 如果 Y 是 binary 將預測的 logit(p) 改回 p
  # if (is_Y_binary) {
  #   data["Q_Y"] <- plogis(data[, "Q_Y"])
  #   for (t in tau:1) {
  #     for (v in num_V:1) {
  #       Q_name <- paste("Q", vars[v], t, sep = "_")
  #       data[[Q_name]] <- plogis(data[, Q_name])
  #     }
  #   }
  # }
  
  return(data)
}


calculate_EIF <- function(data, Q_TMLE) {
  # 除了C,A,Y有哪些變數
  vars <- c()
  if (is_L_present) {
    vars <- c(vars, "L")
  }
  for (i in 1:num_M) {
    Mi <- paste0("M", i)
    vars <- c(vars, Mi)
  }
  num_V <- length(vars) # 除了 C,A,Y 有幾種變數
  
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


library(glue)

main <- function(a_list, flag, data, sc) {
  
  if (flag) {

    # 計算各種ALM的預測機率
    if (length(unique(a_list))==1) {
      p_dfs <- predict_ALM_probability(data, causal_structure[1:5], sc)
    } else {
      p_dfs <- predict_ALM_probability(data, causal_structure, sc)
    }
    
    H_df <- calculate_H(data, p_dfs, a_list)
    data <- cbind(data, H_df)
    
    data <- algo_main(data, causal_structure, a_list, flag, sc)
    Q_IPW <- mean(data[, "Q_IPW"])
    Q_TMLE <- mean(data[, ncol(data)])
    
    results_list <- list()
    results_list[["Q"]] <- c(Q_IPW, Q_TMLE)
    results_list[["EIF"]] <- calculate_EIF(data, Q_TMLE)
    
    return(results_list)
    
  } else {

    data <- algo_main(data, causal_structure, a_list, flag, sc)
    Q_ICE <- mean(data[, ncol(data)])
    
    return(Q_ICE)
  }
}


recorder <- function(n_v, loop, a_com, flag_v, scenario_v, file_name) {
  
  for (n in n_v) {
    
    # 動態創建向量
    for (sc in scenario_v) {
      for (a_list in a_com) {
        Q_name <- glue("Q{a_list[1]}{a_list[2]}{a_list[3]}")
        
        assign(glue("IPW_{Q_name}_v{sc}"), c())
        assign(glue("ICE_{Q_name}_v{sc}"), c())
        assign(glue("TMLE_{Q_name}_v{sc}"), c())
        
        assign(glue("EIF_{Q_name}_v{sc}"), c())
        assign(glue("CRB_{Q_name}_v{sc}"), c())
      }
      for (effect in c("TE", "DE", "PSE1", "PSE2")) {
        assign(glue("IPW_{effect}_v{sc}"), c())
        assign(glue("ICE_{effect}_v{sc}"), c())
        assign(glue("TMLE_{effect}_v{sc}"), c())
        
        assign(glue("CRB_{effect}_v{sc}"), c())
      }
    }
    
    seed_v <- c()
    
    for (r in 1:loop) {
      # 生成模擬資料
      seed <- round(runif(1, 1, 1e5)); seed_v <- c(seed_v, seed)
      simulated_data <- datasim(n, seed)
      
      # indx <- sapply(simulated_data, is.factor)
      # simulated_data[indx] <- lapply(simulated_data[indx], function(x) as.numeric(as.character(x)))
      
      # simulated_data["A"] <- simulated_data$A %>% factor()
      # simulated_data["Y"] <- simulated_data$Y %>% factor()
      
      # intervene A
      data_A0 <- change_A_related_variables(simulated_data, causal_structure, 0)
      assign("data_A0", data_A0, envir = .GlobalEnv)
      # data_A0["A"] <- data_A0$A %>% factor()
      data_A1 <- change_A_related_variables(simulated_data, causal_structure, 1)
      assign("data_A1", data_A1, envir = .GlobalEnv)
      # data_A1["A"] <- data_A1$A %>% factor()
      
      loop_r_start_time <- Sys.time()
      
      for (sc in scenario_v) {
        for (a_list in a_com) {
          Q_name <- glue("Q{a_list[1]}{a_list[2]}{a_list[3]}")
          
          for (flag in flag_v) {
            
            results <- main(a_list, flag, simulated_data, sc)
            
            if (flag) {
              # 動態取得向量名稱
              ipw_v_name <- glue("IPW_{Q_name}_v{sc}")
              tmle_v_name <- glue("TMLE_{Q_name}_v{sc}")
              
              eif_v_name <- glue("EIF_{Q_name}_v{sc}")
              crb_v_name <- glue("CRB_{Q_name}_v{sc}")
              
              # 將新的結果附加到對應的變數中
              assign(ipw_v_name, c(get(ipw_v_name), results[["Q"]][1]))
              assign(tmle_v_name, c(get(tmle_v_name), results[["Q"]][2]))
              
              assign(eif_v_name, results[["EIF"]])
              assign(crb_v_name, c(get(crb_v_name), var(results[["EIF"]])/n))
              
            } else {
              
              ice_v_name <- glue("ICE_{Q_name}_v{sc}")
              assign(ice_v_name, c(get(ice_v_name), results))
              
            }
          }
        }
        
        # 要先記錄各個effect的CRB
        for (effect in c("TE", "DE", "PSE1", "PSE2")) {
          crb_v_name <- glue("CRB_{effect}_v{sc}")
          if (effect=="TE") {
            assign(crb_v_name, c(get(crb_v_name), var(get(glue("EIF_Q111_v{sc}")) - get(glue("EIF_Q000_v{sc}")))/n))
          } else if (effect=="DE") {
            assign(crb_v_name, c(get(crb_v_name), var(get(glue("EIF_Q100_v{sc}")) - get(glue("EIF_Q000_v{sc}")))/n))
          } else if (effect=="PSE1") {
            assign(crb_v_name, c(get(crb_v_name), var(get(glue("EIF_Q111_v{sc}")) - get(glue("EIF_Q101_v{sc}")))/n))
          } else {
            assign(crb_v_name, c(get(crb_v_name), var(get(glue("EIF_Q101_v{sc}")) - get(glue("EIF_Q100_v{sc}")))/n))
          }
        }
        
      }
      
      for (sc in scenario_v) {
        for (effect in c("TE", "DE", "PSE1", "PSE2")) {
          
          ipw_v_name <- glue("IPW_{effect}_v{sc}")
          tmle_v_name <- glue("TMLE_{effect}_v{sc}")
          ice_v_name <- glue("ICE_{effect}_v{sc}")
          
          if (effect=="TE") {
            
            assign(ipw_v_name, get(glue("IPW_Q111_v{sc}")) - get(glue("IPW_Q000_v{sc}")))
            assign(tmle_v_name, get(glue("TMLE_Q111_v{sc}")) - get(glue("TMLE_Q000_v{sc}")))
            assign(ice_v_name, get(glue("ICE_Q111_v{sc}")) - get(glue("ICE_Q000_v{sc}")))
            
          } else if (effect=="DE") {
            
            assign(ipw_v_name, get(glue("IPW_Q100_v{sc}")) - get(glue("IPW_Q000_v{sc}")))
            assign(tmle_v_name, get(glue("TMLE_Q100_v{sc}")) - get(glue("TMLE_Q000_v{sc}")))
            assign(ice_v_name, get(glue("ICE_Q100_v{sc}")) - get(glue("ICE_Q000_v{sc}")))
  
          } else if (effect=="PSE1") {
            
            assign(ipw_v_name, get(glue("IPW_Q111_v{sc}")) - get(glue("IPW_Q101_v{sc}")))
            assign(tmle_v_name, get(glue("TMLE_Q111_v{sc}")) - get(glue("TMLE_Q101_v{sc}")))
            assign(ice_v_name, get(glue("ICE_Q111_v{sc}")) - get(glue("ICE_Q101_v{sc}")))
  
          } else {
            
            assign(ipw_v_name, get(glue("IPW_Q101_v{sc}")) - get(glue("IPW_Q100_v{sc}")))
            assign(tmle_v_name, get(glue("TMLE_Q101_v{sc}")) - get(glue("TMLE_Q100_v{sc}")))
            assign(ice_v_name, get(glue("ICE_Q101_v{sc}")) - get(glue("ICE_Q100_v{sc}")))
            
          }
        }
      }
      
      loop_r_end_time <- Sys.time()
      print(r)
      print(loop_r_end_time - loop_r_start_time)
      
    }
    
    ## 儲存結果
    
    # 創建空的list存放向量
    ipw_Q_list <- list(); ice_Q_list <- list(); tmle_Q_list <- list(); crb_Q_list <- list()
    ipw_E_list <- list(); ice_E_list <- list(); tmle_E_list <- list(); crb_E_list <- list()
    
    # 填充list
    for (sc in scenario_v) {
      for (a_list in a_com) {
        
        Q_name <- glue("Q{a_list[1]}{a_list[2]}{a_list[3]}")
        
        ipw_Q_list[[glue("IPW_{Q_name}_s{sc}")]] <- 
          get(glue("IPW_{Q_name}_v{sc}"))
        
        ice_Q_list[[glue("ICE_{Q_name}_s{sc}")]] <- 
          get(glue("ICE_{Q_name}_v{sc}"))
        
        tmle_Q_list[[glue("TMLE_{Q_name}_s{sc}")]] <- 
          get(glue("TMLE_{Q_name}_v{sc}"))
        
        crb_Q_list[[glue("CRB_{Q_name}_s{sc}")]] <- 
          get(glue("CRB_{Q_name}_v{sc}"))
        
      }
      for (effect in c("TE", "DE", "PSE1", "PSE2")) {
        
        ipw_E_list[[glue("IPW_{effect}_s{sc}")]] <- 
          get(glue("IPW_{effect}_v{sc}"))
        
        ice_E_list[[glue("ICE_{effect}_s{sc}")]] <- 
          get(glue("ICE_{effect}_v{sc}"))
        
        tmle_E_list[[glue("TMLE_{effect}_s{sc}")]] <- 
          get(glue("TMLE_{effect}_v{sc}"))
        
        crb_E_list[[glue("CRB_{effect}_s{sc}")]] <- 
          get(glue("CRB_{effect}_v{sc}"))
        
      }
    }
    
    # 將所有list合併成data.frame
    results_df <- data.frame(c(ipw_Q_list, ice_Q_list, tmle_Q_list, crb_Q_list,
                               ipw_E_list, ice_E_list, tmle_E_list, crb_E_list), 
                             seed = seed_v)
    
    # 寫入CSV檔案
    write.csv(results_df, glue(file_name), row.names = FALSE)
    
  }
  
}



############## 測試結果 ##############



library(glue)

generate_causal_structure()

n_v <- c(1000)
loop <- 100

a111 <- c(1,1,1); a000 <- c(0,0,0); a100 <- c(1,0,0); a101 <- c(1,0,1)
a_com <- list(a111, a000, a100, a101)

flag_v <- c(FALSE, TRUE)

if (set_robust==3) {
  scenario_v <- 0:12
} else {
  scenario_v <- 1:5
}

scenario_v <- 0

file_title <- "Q_n{n}"
file_name <- paste0(file_title, ".csv")

recorder(n_v, loop, a_com, flag_v, scenario_v, file_name)



# n = 10^7, r = 100
df_MC <- read.csv("Phi_111_MC.csv")
tQ111 <- df_MC$Phi_111 %>% mean()
df_MC <- read.csv("Phi_000_MC.csv")
tQ000 <- df_MC$Phi_000 %>% mean()
df_MC <- read.csv("Phi_100_MC.csv")
tQ100 <- df_MC$Phi_100 %>% mean()
df_MC <- read.csv("Phi_101_MC.csv")
tQ101 <- df_MC$Phi_101 %>% mean()

tTE <- tQ111 - tQ000
tDE <- tQ100 - tQ000
tPSE1 <- tQ111 - tQ101
tPSE2 <- tQ101 - tQ100



library(ggplot2)
library(patchwork)

com3method_Q <- function(n, loop, k, a_list, p_type, tPhi) {
  
  df <- read.csv(glue("result/Q_n{n}.csv"))
  
  if (k==3) {
    scenario_v <- c(0, 1, 8, 9, 10, 11)
  } else {
    scenario_v <- 0
  }
  
  method_v <- c()
  Q_results <- c()
  for (sc in scenario_v) {
    
    Q_name <- glue("Q{a_list[1]}{a_list[2]}{a_list[3]}")
    
    Q_IPW_v <- df[, glue("IPW_{Q_name}_s{sc}")]
    Q_ICE_v <- df[, glue("ICE_{Q_name}_s{sc}")]
    Q_TMLE_v <- df[, glue("TMLE_{Q_name}_s{sc}")]
    
    Q_results <- c(Q_results, Q_IPW_v, Q_ICE_v, Q_TMLE_v)
    
    # if (sc==scenario_v[1]) {
    #   tPhi <- mean(Q_IPW_v)
    # }
    
  }
  scenario_v <- c("i", "ii", "iii", "iv", "v", "vi")
  scen_v = rep(as.character(scenario_v), each = loop*3)
  method_v = rep(c("IPW", "ICEs", "TMLE"), each = loop) %>% rep(times = length(scenario_v))
  
  # Create a dataframe
  df1 <- data.frame(
    Method = method_v,
    Scenario = scen_v,
    Q = Q_results - tPhi
  )
  
  df1 <- df1 %>% mutate(Method_Scenario = paste0(Method, "(", Scenario, ")"))
  if (p_type==1) {
    df1$Method_Scenario <- df1$Method_Scenario %>%
      factor(levels = unique(df1$Method_Scenario))
  }
  
  # summary
  df1_summary <- df1 %>%
    group_by(Method, Scenario, Method_Scenario) %>%
    summarise(
      Mean = mean(Q),
      SD = sd(Q)
    )
  
  # Explicitly set the order of Method
  df1_summary$Method <- factor(df1_summary$Method, levels = c("IPW", "ICEs", "TMLE"))
  
  # Create the plot
  p1 <- ggplot(df1_summary, aes(x = Method_Scenario, y = Mean, 
                                color = Method, # shape = Scenario, 
                                ymin =  0 - 1, ymax = 0 + 1)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = Mean - 1 * SD, ymax = Mean + 1 * SD), width = 0.2) +
    geom_hline(yintercept = 0, color = "black") +
    geom_vline(xintercept = seq(3.5, length(scenario_v) * 3, by = 3), linetype = "dashed", color = "grey") + # Add vertical lines
    scale_color_grey(start = 0.6, end = 0.2) +  # Use grey scale for colors
    theme_minimal() +
    theme(axis.ticks.x = element_blank()) +  # Hide x-axis text
    labs(#title = glue("Phi_{a_list[1]}{a_list[2]}{a_list[3]}"),
      x = "Scenario",
      y = "Bias") +
    scale_x_discrete(labels = rbind(" ", paste0("(", scenario_v, ")"), " "))  # Custom x-axis labels
  
  return(p1)
}

com3method_effect <- function(n, loop, k, effect, p_type, tPhi) {
  
  df <- read.csv(glue("result/Q_n{n}.csv"))
  
  if (k==3) {
    scenario_v <- c(0, 1, 8, 9, 10, 11)
  } else {
    scenario_v <- 0
  }
  
  method_v <- c()
  Q_results <- c()
  for (sc in scenario_v) {
    
    Q_IPW_v <- df[, glue("IPW_{effect}_s{sc}")]
    
    Q_ICE_v <- df[, glue("ICE_{effect}_s{sc}")]
    
    Q_TMLE_v <- df[, glue("TMLE_{effect}_s{sc}")]
    
    Q_results <- c(Q_results, Q_IPW_v, Q_ICE_v, Q_TMLE_v)
    
    # if (sc==scenario_v[1]) {
    #   tPhi <- mean(Q_IPW_v)
    # }
    
  }
  scenario_v <- c("i", "ii", "iii", "iv", "v", "vi")
  scen_v = rep(as.character(scenario_v), each = loop*3)
  method_v = rep(c("IPW", "ICEs", "TMLE"), each = loop) %>% rep(times = length(scenario_v))
  
  # Create a dataframe
  df1 <- data.frame(
    Method = method_v,
    Scenario = scen_v,
    Q = Q_results - tPhi
  )
  
  df1 <- df1 %>% mutate(Method_Scenario = paste0(Method, "(", Scenario, ")"))
  if (p_type==1) {
    df1$Method_Scenario <- df1$Method_Scenario %>%
      factor(levels = unique(df1$Method_Scenario))
  }
  
  # summary
  df1_summary <- df1 %>%
    group_by(Method, Scenario, Method_Scenario) %>%
    summarise(
      Mean = mean(Q),
      SD = sd(Q)
    )
  
  # Explicitly set the order of Method
  df1_summary$Method <- factor(df1_summary$Method, levels = c("IPW", "ICEs", "TMLE"))
  
  # Create the plot
  p1 <- ggplot(df1_summary, aes(x = Method_Scenario, y = Mean, 
                                color = Method, # shape = Scenario, 
                                ymin =  0 - 1, ymax = 0 + 1)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = Mean - 1 * SD, ymax = Mean + 1 * SD), width = 0.2) +
    geom_hline(yintercept = 0, color = "black") +
    geom_vline(xintercept = seq(3.5, length(scenario_v) * 3, by = 3), linetype = "dashed", color = "grey") + # Add vertical lines
    scale_color_grey(start = 0.6, end = 0.2) +  # Use grey scale for colors
    theme_minimal() +
    theme(axis.ticks.x = element_blank()) +  # Hide x-axis text
    labs(#title = glue("Phi_{a_list[1]}{a_list[2]}{a_list[3]}"),
      x = "Scenario",
      y = "Bias") +
    scale_x_discrete(labels = rbind(" ", paste0("(", scenario_v, ")"), " "))  # Custom x-axis labels
  
  return(p1)
}


p1 <- com3method_Q(500, 1000, 3, c(1,1,1), 1, tQ111)
p2 <- com3method_Q(500, 1000, 3, c(0,0,0), 1, tQ000)
p3 <- com3method_Q(500, 1000, 3, c(1,0,0), 1, tQ100)
p4 <- com3method_Q(500, 1000, 3, c(1,0,1), 1, tQ101)

p1 <- com3method_effect(500, 1000, 3, "TE", 1, tTE)
p2 <- com3method_effect(500, 1000, 3, "DE", 1, tDE)
p3 <- com3method_effect(500, 1000, 3, "PSE1", 1, tPSE1)
p4 <- com3method_effect(500, 1000, 3, "PSE2", 1, tPSE2)

p1 + p2 + p3 + p4
p4



result_demo <- function(n, k, a_com) {
  
  df <- read.csv(glue("Q_n{n}_com.csv"))
  r <- nrow(df)
  
  if (k==3) {
    scenario_v <- 0:12
  } else {
    scenario_v <- 0
  }
  
  for (sc in scenario_v) {
    print(sc)
    
    for (a_list in a_com) {
      print(a_list)
      
      for (method in c("IPW", "ICE", "TMLE")) {
        print(method)
        
        Q_name <- glue("Q{a_list[1]}{a_list[2]}{a_list[3]}")
        Q_est_v <- df[[glue("{method}_{Q_name}_s{sc}")]]
        Q_true <- get(glue("t{Q_name}"))
        
        Q_bias <- mean(Q_est_v - Q_true)
        Q_var <- var(Q_est_v)
        
        print(c("Bias", "Var", "SD"))
        print(c(Q_bias, Q_var, sqrt(Q_var)))
        
        if (method == "TMLE") {
          crb_V <- df[[glue("CRB_{Q_name}_s{sc}")]]
          
          print(c("est_var", "est_sd"))
          print(c(mean(crb_V), mean(sqrt(crb_V))))
          
          upb_v <- Q_est_v + 1.96 * sqrt(crb_V)
          lowb_v <- Q_est_v - 1.96 * sqrt(crb_V)
          
          cover_rate <- sum(between(rep(Q_true, r), lowb_v, upb_v)) / r
          
          print("95% CI")
          print(cover_rate)
        }
        
      }
      print("===============")
    }
    
    for (effect in c("TE", "DE", "PSE1", "PSE2")) {
      print(effect)
      
      for (method in c("IPW", "ICE", "TMLE")) {
        print(method)
        
        E_est_v <- df[[glue("{method}_{effect}_s{sc}")]]
        E_true <- get(glue("t{effect}"))
        
        E_bias <- mean(E_est_v - E_true)
        E_var <- var(E_est_v)
        
        print(c("Bias", "Var", "SD"))
        print(c(E_bias, E_var, sqrt(E_var)))
        
        if (method == "TMLE") {
          crb_V <- df[[glue("CRB_{effect}_s{sc}")]]
          
          print(c("est_var", "est_sd"))
          print(c(mean(crb_V), mean(sqrt(crb_V))))
          
          upb_v <- E_est_v + 1.96 * sqrt(crb_V)
          lowb_v <- E_est_v - 1.96 * sqrt(crb_V)
          
          cover_rate <- sum(between(rep(E_true, r), lowb_v, upb_v)) / r
          
          print("95% CI")
          print(cover_rate)
        }
        
      }
      print("===============")
    }
    
  }
  
}


result_demo(2000, 1, a_com)





# n <- 1000
# df1 <- read.csv(glue("Q_n{n} (1).csv"))
# df2 <- read.csv(glue("Q_n{n} (2).csv"))
# merged_df <- bind_rows(df1, df2)
# write.csv(merged_df, glue("Q_n{n}_com.csv"), row.names = FALSE)
# for (i in 3:20) {
#   df1 <- read.csv(glue("Q_n{n}_com.csv"))
#   df2 <- read.csv(glue("Q_n{n} ({i}).csv"))
#   merged_df <- bind_rows(df1, df2)
#   write.csv(merged_df, glue("Q_n{n}_com.csv"), row.names = FALSE)
# }






############## 待考慮問題 ##############

# part_H 有需要算那麼多個嗎
# 計算pA的部分其實可以全部合併，建完A的模型就直接預測並儲存下來
# eps 要用機器學習估嗎
# update 到底要對Q 還是  (Y?)參考網站
# 開頭的E[Y] 要算它的H嗎
# causal structure 需要嚴謹到甚麼程度 A, L1, M1, L2, M2, ...
# 建立A的模型會重複創建部分模型
# 沒有L時 計算H 的 a_list 會跟變數對不上
# Q_TMLE 如果沒有迭代就不算TMLE
# 要考慮Y不是二元變數的情況
# bootstrap 的 var 要除以 r 嗎?
# calculate_H 裡面 H_Y 的算法是基於 A, L, M1, M2, ... , Y 的架構執行
# main裡面的write.csv的seed有可能不是global

# L,M的模型 要考慮不是二元變數 或是 多個變數 # 計算H的部分還沒有L,M的部分

# Simulation 設定
# 整體模型複雜度: CALMY 都是二元變數 or CLM 為三類別
# H 模型複雜度: A time-varying or not

# 改用其他 EIF-based estimator? i.e. one step TMLE

