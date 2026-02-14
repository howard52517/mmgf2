
required_pkgs <- c("glue")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}


main <- function(a_list, methods, data, sc, nonp = FALSE) {
  results_list <- list()
  
  # 1. Calculate weights H_df if IPW or TMLE are requested
  if (any(c("IPW", "TMLE") %in% methods)) {
    # if the intervention A values are constant, H calculation requires only V and A data
    if (length(unique(a_list)) == 1) {
      p_dfs <- predict_ALM_probability(data, causal_structure[1:(length(latent_var)+1)], sc, nonp)
    } else {
      p_dfs <- predict_ALM_probability(data, causal_structure, sc, nonp)
    }
    H_df <- calculate_H(data, p_dfs, a_list)
    data <- cbind(data, H_df)
    
    # 2. Independently calculate IPW Estimator: E(H_Y * Y)
    if ("IPW" %in% methods) {
      results_list[["IPW"]] <- mean(data$H_Y * data$Y)
    }
  }
  
  # 3. Calculate TMLE (flag = TRUE)
  if ("TMLE" %in% methods) {
    data_tmle <- algo_main(data, causal_structure, a_list, flag = TRUE, sc, nonp)
    Q_TMLE <- mean(data_tmle[[ncol(data_tmle)]])
    results_list[["TMLE"]] <- Q_TMLE
    # Get EIF for CRB calculation
    results_list[["EIF"]] <- calculate_EIF(data_tmle, Q_TMLE)
  }
  
  # 4. Calculate ICE (flag = FALSE)
  if ("ICE" %in% methods) {
    data_ice <- algo_main(data, causal_structure, a_list, flag = FALSE, sc, nonp)
    results_list[["ICE"]] <- mean(data_ice[[ncol(data_ice)]])
  }
  
  # 5. Calculate GCB
  if ("GCB" %in% methods) {
    results_list[["GCB"]] <- algo_GCB(data, causal_structure, a_list, copy_num = 10000, sc, nonp)
  }
  
  return(results_list)
}


recorder <- function(n_v, loop, file_title, nonp = FALSE, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  a111 <- c(1,1,1); a000 <- c(0,0,0); a100 <- c(1,0,0); a101 <- c(1,0,1)
  a_com <- list(a111, a000, a100, a101)
  
  methods_v <- c("GCB", "IPW", "ICE", "TMLE")
  
  scenario_v <- 1:8
  
  for (n in n_v) {
    storage <- list()
    
    for (r in 1:loop) {
      loop_r_start_time <- Sys.time()
      
      simulated_data <- datasim(n)
      
      # these two variables must remain global as they are accessed via get() in other estimator functions
      assign("data_A0", change_A_related_variables(simulated_data, causal_structure, 0), envir = .GlobalEnv)
      assign("data_A1", change_A_related_variables(simulated_data, causal_structure, 1), envir = .GlobalEnv)
      
      for (sc in scenario_v) {
        # store results for each a_list in the current loop
        curr_q <- list() 
        curr_eif <- list()
        
        for (idx in 1:length(a_com)) {
          a_list <- a_com[[idx]]
          Q_name <- glue("Q{paste(a_list, collapse='')}")
          res <- main(a_list, methods_v, simulated_data, sc, nonp)
          
          # record point estimates of Q for each method
          for (m in methods_v) {
            col <- glue("{m}_{Q_name}_s{sc}")
            storage[[col]] <- c(storage[[col]], res[[m]])
            curr_q[[m]][[idx]] <- res[[m]]
          }
          
          # record TMLE-specific CRB (variance of EIF / n)
          if ("TMLE" %in% methods_v) {
            crb_col <- glue("CRB_{Q_name}_s{sc}")
            storage[[crb_col]] <- c(storage[[crb_col]], var(res$EIF)/n)
            curr_eif[[idx]] <- res$EIF # temporarily store EIF for Effect calculation
          }
        }
        
        # --- Calculate Effects ---
        # Index correspondence: 1=a111, 2=a000, 3=a100, 4=a101
        for (m in methods_v) {
          te <- curr_q[[m]][[1]] - curr_q[[m]][[2]]
          de <- curr_q[[m]][[3]] - curr_q[[m]][[2]]
          pse1 <- curr_q[[m]][[1]] - curr_q[[m]][[4]]
          pse2 <- curr_q[[m]][[4]] - curr_q[[m]][[3]]
          
          storage[[glue("{m}_TE_s{sc}")]]   <- c(storage[[glue("{m}_TE_s{sc}")]], te)
          storage[[glue("{m}_DE_s{sc}")]]   <- c(storage[[glue("{m}_DE_s{sc}")]], de)
          storage[[glue("{m}_PSE1_s{sc}")]] <- c(storage[[glue("{m}_PSE1_s{sc}")]], pse1)
          storage[[glue("{m}_PSE2_s{sc}")]] <- c(storage[[glue("{m}_PSE2_s{sc}")]], pse2)
        }
        
        # calculate Effect CRB for TMLE (Variance based on EIF differences)
        if ("TMLE" %in% methods_v) {
          storage[[glue("CRB_TE_s{sc}")]]   <- c(storage[[glue("CRB_TE_s{sc}")]], var(curr_eif[[1]] - curr_eif[[2]])/n)
          storage[[glue("CRB_DE_s{sc}")]]   <- c(storage[[glue("CRB_DE_s{sc}")]], var(curr_eif[[3]] - curr_eif[[2]])/n)
          storage[[glue("CRB_PSE1_s{sc}")]] <- c(storage[[glue("CRB_PSE1_s{sc}")]], var(curr_eif[[1]] - curr_eif[[4]])/n)
          storage[[glue("CRB_PSE2_s{sc}")]] <- c(storage[[glue("CRB_PSE2_s{sc}")]], var(curr_eif[[4]] - curr_eif[[3]])/n)
        }
      }
      
      message(glue("n={n}, r={r} done."))
      loop_r_end_time <- Sys.time()
      print(loop_r_end_time - loop_r_start_time)
    }
    
    # Convert the temporary list to a dataframe and save it
    results_df <- as.data.frame(storage)
    
    file_name <- glue("{file_title}_n{n}.csv")
    write.csv(results_df, file_name, row.names = FALSE)
    message(paste("Saved:", file_name))
  }
}

