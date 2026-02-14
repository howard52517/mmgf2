
summary_all_effects <- function(data, alpha = 0.05) {
  
  n <- nrow(data)
  # these two variables must remain global as they are accessed via get() in other estimator functions
  assign("data_A0", change_A_related_variables(data, causal_structure, 0), envir = .GlobalEnv)
  assign("data_A1", change_A_related_variables(data, causal_structure, 1), envir = .GlobalEnv)
  
  a111 <- c(1,1,1); a000 <- c(0,0,0); a100 <- c(1,0,0); a101 <- c(1,0,1)
  a_com <- list(a111, a000, a100, a101)
  
  res_list <- lapply(a_com, function(a) main(a, methods = "TMLE", data = data, sc = 1, nonp = TRUE))
  
  # res_list contains the execution results of main() for four interventions: a111, a000, a100, and a101
  # Index reference: 1:a111, 2:a000, 3:a100, 4:a101
  
  effects_names <- c("TE", "PSE0", "PSE1", "PSE2")
  
  # define pairs of indices for subtraction (Index Pairs)
  # TE: 1-2, PSE0: 3-2, PSE1: 1-4, PSE2: 4-3
  pairs <- list(c(1, 2), c(3, 2), c(1, 4), c(4, 3))
  
  results <- data.frame()
  
  for (i in 1:length(pairs)) {
    idx_a <- pairs[[i]][1]
    idx_b <- pairs[[i]][2]
    
    # 1. Point estimate difference
    diff_est <- res_list[[idx_a]]$TMLE - res_list[[idx_b]]$TMLE
    
    # 2. Calculate Standard Error (SE) using EIF differences
    # Var(A - B) = Var(EIF_A - EIF_B) / n
    eif_diff <- res_list[[idx_a]]$EIF - res_list[[idx_b]]$EIF
    se <- sqrt(var(eif_diff) / n)
    
    # 3. Statistical inference (Z-score, P-value, CI)
    z_score <- diff_est / se
    p_val <- 2 * (1 - pnorm(abs(z_score)))
    z_crit <- qnorm(1 - alpha/2)
    ci_low <- diff_est - z_crit * se
    ci_high <- diff_est + z_crit * se
    
    # store in table
    row <- data.frame(
      Effect = effects_names[i],
      Estimate = diff_est,
      Std.Error = se,
      z.value = z_score,
      `Pr(>|z|)` = p_val,
      `95% CI Lower` = ci_low,
      `95% CI Upper` = ci_high,
      check.names = FALSE
    )
    results <- rbind(results, row)
  }
  
  # add significance labels
  results$Sig <- cut(results$`Pr(>|z|)`, breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, 1),
                     labels = c("***", "**", "*", ".", " "))
  
  cat("\nPath-specific effect analysis (TMLE):\n")
  cat("Sample Size (n):", n, "\n")
  cat("------------------------------------------------------------------------------------\n")
  print(results, row.names = FALSE)
  cat("------------------------------------------------------------------------------------\n")
  cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
  
  return(invisible(results))
}

