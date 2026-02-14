
latent_var <- c("V1", "V2", "V3", "V4")
causal_structure <- c(latent_var, 
                      "A", "L_1", "M1_1", "M2_1", 
                      "L_2", "M1_2", "M2_2", "Y")
is_Y_binary <- FALSE
is_L_present <- TRUE
is_A_time_varying <- FALSE
num_M <- 2
tau <- 2


# Set different simulation scenarios
# -------------------------------------------
# Scenario  | Misspecified
# -------------------------------------------
#       (1) | None
#       (2) | Models related to A
#       (3) | Models related to L
#       (4) | Models related to M1
#       (5) | Models related to M2
#       (6) | Models related to Y
#       (7) | all conditional probabilities
#       (8) | all conditional expectations
# -------------------------------------------
modify_var <- function(dep_var, ind_var, mod_type, sc) {
  
  if (sc==1) {
    return(ind_var)
  }
  
  if (sc==2) {
    if (grepl("^A", dep_var)) {
      ind_var <- ind_var[-1:-2]
      return(ind_var)
    } else {
      return(ind_var)
    }
  }
  
  if (sc==3) {
    if (grepl("^L", dep_var)) {
      ind_var <- ind_var[-1:-2]
      return(ind_var)
    } else {
      return(ind_var)
    }
  }
  
  if (sc==4) {
    if (grepl("^M1", dep_var)) {
      ind_var <- ind_var[-1:-2]
      return(ind_var)
    } else {
      return(ind_var)
    }
  }
  
  if (sc==5) {
    if (grepl("^M2", dep_var)) {
      ind_var <- ind_var[-1:-2]
      return(ind_var)
    } else {
      return(ind_var)
    }
  }
  
  if (sc==6) {
    if (dep_var=="Y") {
      ind_var <- ind_var[-1:-2]
      return(ind_var)
    } else {
      return(ind_var)
    }
  }
  
  if (sc==7) {
    if (mod_type=="Q") {
      return(ind_var)
    } else { # mod_type == "G"
      ind_var <- ind_var[-1:-2]
      return(ind_var)
    }
  }
  
  if (sc==8) {
    if (mod_type=="G") {
      return(ind_var)
    } else { # mod_type == "Q"
      ind_var <- ind_var[-1:-2]
      return(ind_var)
    }
  }
  
}

