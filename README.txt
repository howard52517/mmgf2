1. Project overview

This repository contains the R code used to demo the simulation studies
and numerical results in "Targeted maximum likelihood estimation for 
mediation analysis with multiple time-varying mediators".


2. Folder structure
.
├── DGP.R                 # Data generating processes (DGPs)
├── sim_setting.R         # Simulation settings
├── estimators.R          # Estimation methods
├── sim.R                 # Core simulation engine
├── main.R                # Entry point for running simulations
│
├── utils.R               # Functions to summarize simulation results
├── main_summary.R        # Script to generate tables and figures
│
├── DA_functions.R        # Functions for data analysis
├── main_DA.R             # Script to run the data analysis demo
│
├── coefs.rds              	# Parameters used in the DGPs
├── truth.rds              	# True estimand values
│
├── sim_res_n500.csv       	# Precomputed simulation results
├── sim_res_n1000.csv      	# Precomputed simulation results
├── sim_res_n2000.csv      	# Precomputed simulation results
│
├── bias plots (sim_res).pdf # bias plots for precomputed simulation results
└── metrics (sim_res).xlsx   # metrics for precomputed simulation results


3. Software requirements

R version: >= 4.2.0

Required packages:
 - glmnet
 - SuperLearner
 - glue
 - dplyr
 - ggplot2
 - patchwork
 - gridExtra
 - openxlsx


4. Simulation studies

To allow users to quickly test and understand the simulation framework, the script
main.R is configured to run a simplified version of the simulation study
reported in the paper.

Specifically:

- Only Scenarios 1–8 are included, corresponding to parametric model settings
used to examine multiple robustness.

- The number of simulation replications is set to 100
(the paper uses 1,000 replications).

- Simulations involving Super Learner are not included, as they are
computationally expensive.

To run the simulation, execute:

	source("main.R")

Running this simplified simulation takes approximately 30 minutes
on a standard desktop computer (Intel i5-12400, 32 GB RAM).

If you prefer not to rerun the simulations, precomputed results are available.


5. Summarizing simulation results

To reproduce the tables and figures based on the saved simulation results, run:

	source("main_summary.R")

This script organizes the simulation outputs into tables and figures comparable
to those reported in the paper.


6. Data analysis demonstration

Due to data sharing restrictions, the original study data cannot be made publicly
available.

Instead of the original data, we provide a demo dataset that is generated using 
the data generating process (DGP) functions defined in DGP.R.

This dataset is not intended to be a fully synthetic replacement of the original
data, but rather serves as a stand-alone example to demonstrate the full data
analysis pipeline and the behavior of the proposed estimators.

To run the data analysis demo, execute:

	source("main_DA.R")

In this demonstration:

- The demo dataset has a sample size of n = 500.

- Estimation is performed using TMLE combined with Super Learner.
(SL.library = c("SL.mean", "SL.glm", "SL.glmnet", "SL.gam", "SL.xgboost"))

- Running TMLE once provides point estimates, standard errors, and 95%
confidence intervals directly.

On a standard desktop computer (Intel i5-12400, 32GB RAM), one TMLE run takes
approximately 4 minutes.

This example highlights an important practical advantage of TMLE: unlike methods
such as GCB, IPW, or ICE, TMLE does not require bootstrapping to obtain valid
standard errors and confidence intervals.


7. Contact information

For questions regarding the code, please contact: [shenglin@nycu.edu.tw].


