
source("DGP.R")
source("sim_setting.R")
source("estimators.R")
source("sim.R")

recorder(n_v = c(500, 1000, 2000), 
         loop = 100, 
         file_title = "sim_res", 
         nonp = FALSE, 
         seed = 2026)

