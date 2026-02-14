
required_pkgs <- c("glue", "dplyr", "ggplot2", "patchwork", "gridExtra", "openxlsx")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}


bias_plot <- function(file_title, n, effect, tPhi, title_name, y_lim) {
  
  file_name <- glue("{file_title}_n{n}.csv")
  df <- read.csv(file_name)
  
  scenario_v <- 1:8
  
  bias_v <- c()
  sd_v <- c()
  for (sc in scenario_v) {
    
    Q_GCB_v <- df[, glue("GCB_{effect}_s{sc}")] - tPhi
    Q_IPW_v <- df[, glue("IPW_{effect}_s{sc}")] - tPhi
    Q_ICE_v <- df[, glue("ICE_{effect}_s{sc}")] - tPhi
    Q_TMLE_v <- df[, glue("TMLE_{effect}_s{sc}")] - tPhi
    
    bias_v <- c(bias_v, mean(Q_GCB_v), mean(Q_IPW_v), mean(Q_ICE_v), mean(Q_TMLE_v))
    sd_v <- c(sd_v, sd(Q_GCB_v), sd(Q_IPW_v), sd(Q_ICE_v), sd(Q_TMLE_v))
  }
  
  scenario <- c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)")
  
  df_result <- data.frame(
    scenario = rep(scenario, each = 4),
    Estimator = rep(c("GCB", "IPW", "ICE", "TMLE"), times = length(scenario_v)),
    bias = bias_v,
    sd = sd_v
  )
  
  # set scenario as a factor to ensure order
  df_result$scenario <- factor(df_result$scenario)
  df_result$Estimator <- factor(df_result$Estimator, levels = c("GCB", "IPW", "ICE", "TMLE"))
  
  # plot the graph, using position_dodge to adjust the spacing between methods
  p1 <- ggplot(df_result, aes(x = scenario, y = bias, shape = Estimator, 
                              ymin = -y_lim, ymax = y_lim)) +
    geom_point(position = position_dodge(width = 0.6), 
               size = 2) +
    geom_errorbar(aes(ymin = bias - sd, ymax = bias + sd),
                  position = position_dodge(width = 0.6), 
                  width = 0.2) +
    geom_hline(yintercept = 0, color = "black") +
    labs(x = "Scenario", y = "Bias", title = title_name) + 
    theme_minimal() + 
    theme(
      panel.grid = element_blank(),  # remove panel grid lines
      axis.line = element_line(color = "black"),  # display black axis lines
      axis.ticks = element_line(color = "black")
    ) + 
    coord_cartesian(ylim = c(-y_lim, y_lim)) # limit the Y-axis range
  
  return(p1)
}


com_plot <- function(file_title) {
  
  truth_df <- readRDS("truth.rds")
  tDE <- truth_df[truth_df$parameter == "PSE0",]$truth
  tPSE1 <- truth_df[truth_df$parameter == "PSE1",]$truth
  tPSE2 <- truth_df[truth_df$parameter == "PSE2",]$truth
  
  n <- 500
  p500_0 <- bias_plot(file_title, n, "DE", tDE, 
                      bquote(PSE[0] ~ " ( sample size " ~ .(n) ~")"), 1.3)
  p500_1 <- bias_plot(file_title, n, "PSE1", tPSE1, 
                      bquote(PSE[1] ~ " ( sample size " ~ .(n) ~")"), 1.3)
  p500_2 <- bias_plot(file_title, n, "PSE2", tPSE2, 
                      bquote(PSE[2] ~ " ( sample size " ~ .(n) ~")"), 1.3)
  
  n <- 1000
  p1000_0 <- bias_plot(file_title, n, "DE", tDE, 
                       bquote(PSE[0] ~ " ( sample size " ~ .(n) ~")"), 1.3)
  p1000_1 <- bias_plot(file_title, n, "PSE1", tPSE1, 
                       bquote(PSE[1] ~ " ( sample size " ~ .(n) ~")"), 1.3)
  p1000_2 <- bias_plot(file_title, n, "PSE2", tPSE2, 
                       bquote(PSE[2] ~ " ( sample size " ~ .(n) ~")"), 1.3)
  
  n <- 2000
  p2000_0 <- bias_plot(file_title, n, "DE", tDE, 
                       bquote(PSE[0] ~ " ( sample size " ~ .(n) ~")"), 1.3)
  p2000_1 <- bias_plot(file_title, n, "PSE1", tPSE1, 
                       bquote(PSE[1] ~ " ( sample size " ~ .(n) ~")"), 1.3)
  p2000_2 <- bias_plot(file_title, n, "PSE2", tPSE2, 
                       bquote(PSE[2] ~ " ( sample size " ~ .(n) ~")"), 1.3)
  
  p0 <- p500_0 + p1000_0 + p2000_0 + 
    p500_1 + p1000_1 + p2000_1 + 
    p500_2 + p1000_2 + p2000_2
  
  
  # combine plots and collect the legend
  combined_plot <- p0 +
    plot_layout(guides = "collect")
  
  # set the legend position to the right
  combined_plot <- combined_plot & theme(
    legend.position = "right",
    legend.justification = "top",  # legend top-aligned
    plot.margin = margin(5.5, 70, 5.5, 5.5)  # adjust right margin
  )
  
  scenario_description <- data.frame(
    Scenario = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)"),
    Misspecified = c("None", "Models related to A", "Models related to L", 
                     "Models related to M1", "Models related to M2", "Models related to Y", 
                     "all conditional probabilities", "all conditional expectations")
  )
  
  # adjust table style and specify fonts
  tableGrob_scenarios <- tableGrob(
    scenario_description, 
    rows = NULL,  # hide row numbers
    theme = ttheme_minimal(
      core = list(fg_params = list(fontsize = 10)), # adjust content font size
      colhead = list(fg_params = list(fontsize = 12)) # adjust header font size
    )
  )
  
  # add annotation_custom to place scenario description in the top-right corner
  combined_plot <- combined_plot + annotation_custom(
    grob = tableGrob_scenarios,
    xmin = 14, ymin = 10 # adjust position
  )
  
  # display the combined plot
  print(combined_plot)
  
  file_name <- glue("bias plots ({file_title}).pdf")
  pdf(file_name, width = 20, height = 12)
  print(combined_plot)
  dev.off()
  message(paste("Saved:", file_name))
}


summary_table <- function(file_title, sc, effect, truth) {
  
  estimator_v <- c()
  n_v <- c()
  bias_v <- c()
  sqrtnbias_v <- c()
  empSE_v <- c()
  MSE_v <- c()
  nMSE_v <- c()
  empCR_v <- c()
  estSE_v <- c()
  estCR_v <- c()
  
  for (method in c("GCB", "IPW", "ICE", "TMLE")) {
    for (n in c(500, 1000, 2000)) {
      estimator_v <- c(estimator_v, method)
      n_v <- c(n_v, n)
      
      df <- read.csv(glue("{file_title}_n{n}.csv"))
      est_v <- df[[glue("{method}_{effect}_s{sc}")]]
      r <- nrow(df)
      
      bias_v <- c(bias_v, mean(est_v - truth))
      sqrtnbias_v <- c(sqrtnbias_v, sqrt(n) * mean(est_v - truth))
      empSE_v <- c(empSE_v, sd(est_v))
      MSE_v <- c(MSE_v, mean((est_v - truth)^2))
      nMSE_v <- c(nMSE_v, n * mean((est_v - truth)^2))
      
      emp_upb_v <- est_v + 1.96 * sd(est_v)
      emp_lowb_v <- est_v - 1.96 * sd(est_v)
      
      emp_cover_rate <- sum(between(rep(truth, r), emp_lowb_v, emp_upb_v)) / r
      empCR_v <- c(empCR_v, emp_cover_rate)
      
      if (method == "TMLE") {
        estCRB_v <- df[[glue("CRB_{effect}_s{sc}")]]
        estSE_v <- c(estSE_v, sqrt(mean(estCRB_v)))
        
        est_upb_v <- est_v + 1.96 * sqrt(estCRB_v)
        est_lowb_v <- est_v - 1.96 * sqrt(estCRB_v)
        
        est_cover_rate <- sum(between(rep(truth, r), est_lowb_v, est_upb_v)) / r
        estCR_v <- c(estCR_v, est_cover_rate)
      } else {
        estSE_v <- c(estSE_v, NA)
        estCR_v <- c(estCR_v, NA)
      }
    }
  }
  
  df_summary <- data.frame(scenario = sc, 
                           effect = ifelse(effect == "DE", "PSE0", effect),
                           estimator = estimator_v, 
                           n = n_v, 
                           bias = round(bias_v, 3), 
                           sqrt_n_bias = round(sqrtnbias_v, 3), 
                           emp_SE = round(empSE_v, 3), 
                           MSE = round(MSE_v, 3), 
                           n_MSE = round(nMSE_v, 3), 
                           emp_CR = round(empCR_v, 3), 
                           est_SE = round(estSE_v, 3), 
                           est_CR = round(estCR_v, 3))
  return(df_summary)
}


com_table <- function(file_title) {
  
  truth_df <- readRDS("truth.rds")
  tDE <- truth_df[truth_df$parameter == "PSE0",]$truth
  tPSE1 <- truth_df[truth_df$parameter == "PSE1",]$truth
  tPSE2 <- truth_df[truth_df$parameter == "PSE2",]$truth
  
  # Create Excel workbook
  wb <- createWorkbook()
  
  # Define sheet names
  sheet_names <- c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4", 
                   "Scenario 5", "Scenario 6", "Scenario 7", "Scenario 8")
  
  # Write tables to different sheets
  tables_list <- list()
  for (sc in 1:8) {
    DE_summary <- summary_table(file_title, sc, "DE", tDE)
    PSE1_summary <- summary_table(file_title, sc, "PSE1", tPSE1)
    PSE2_summary <- summary_table(file_title, sc, "PSE2", tPSE2)
    
    summary_df <- rbind(DE_summary, PSE1_summary, PSE2_summary)
    tables_list[[sc]] <- summary_df
  }
  
  for (i in 1:8) {
    addWorksheet(wb, sheet_names[i])
    writeData(wb, sheet = i, x = tables_list[[i]], startRow = 1, startCol = 1)
    
    # Auto-adjust column widths
    setColWidths(wb, sheet = i, cols = 1:ncol(tables_list[[i]]), widths = "auto")
  }
  
  # Save Excel file
  file_name <- glue("metrics ({file_title}).xlsx")
  saveWorkbook(wb, file_name, overwrite = TRUE)
  message(paste("Saved:", file_name))
}

