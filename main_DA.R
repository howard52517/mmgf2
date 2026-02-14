
source("DGP.R")
source("sim_setting.R")
source("estimators.R")
source("sim.R")
source("DA_functions.R")

n <- 500
demo_data <- datasim(n, seed = 2026)

summary_all_effects(demo_data)

