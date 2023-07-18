# remotes::install_github(repo = 'GiancarloMCorrea/wham', ref='growth', INSTALL_opts = c("--no-docs", "--no-multiarch", "--no-demo"))


# Load libraries ----------------------------------------------------------
library(wham)
library(tidyr)
library(dplyr)

source(file.path('code', "make_basic_info.R"))
source(file.path('code', "set_M.R"))
source(file.path('code', "set_q.R"))
source(file.path('code', "set_simulation_options.R"))
source(file.path('code', "make_om.R"))
source(file.path('code', "sim_management.R"))

# Create data.frame with EM configurations:
method = c('EWAA', 'WAA', 'WAA')
re_method = c(NA, '2dar1', '2dar1')
est_fixed = c(NA, FALSE, TRUE)
catch_data = c('paa', 'paa', 'paa')
index_data = c('paa', 'paa', 'paa')
df.ems = data.frame(method = method, re_method = re_method, est_fixed = est_fixed, catch_data = catch_data, 
                    index_data = index_data)
saveRDS(df.ems, file.path("inputs", "df.ems.RDS"))

# Make basic inputs
gf_info = make_basic_info()

# Selectivity configuration (age-based for now)
gf_selectivity = list(
  model = c("double-normal", "logistic"),
  initial_pars = list(c(4,-3,-1,-1.5,0,0), c(1,0.5)))

# Natural mortality
gf_M = list(initial_means = 0.2, model = "constant")
# Q pars:
gf_Q = list(initial_q = 1,
            q_lower = c(0),
            q_upper = c(10), prior_sd = c(NA))
# NAA configuration
gf_NAA_re = list(N1_pars = c(1e+05, 0),
                sigma = "rec", #random about mean
                cor = "iid", #random effects are independent
                recruit_model = 2,
                N1_model = 1)

# True parameter values:
Linf <- 100
k <- 0.2
t0 <- 0
a_LW <- exp(-12.1)
b_LW <- 3.2
L_a <- Linf*(1-exp(-k*(1:10 - t0)))
W_a <- a_LW*L_a^b_LW

# WAA configuration:
gf_WAA = list(WAA_vals = W_a,
              re = c('none'))

#make inputs for estimating model (smaller objects to save, can overwrinte data elements with simulated data)
em_inputs = list()
for(i in 1:NROW(df.ems)){
  print(paste0("EM config ", i))
  NAA_re_i = gf_NAA_re
  M_i = gf_M
  WAA_i = gf_WAA
  selectivity_i = gf_selectivity
  Q_i = gf_Q

  # Change WAA information -------------------------------
  if(df.ems$method[i] == 'WAA') { # nonparametric approach
    if(!is.na(df.ems$re_method[i])) { # random effects structure
      WAA_i$re = df.ems$re_method[i]
    }
    if(df.ems$est_fixed[i]) { # estimate fixed effects?
      WAA_i$est_pars = 1:10
    }
  }
  if(df.ems$method[i] == 'EWAA') { # EWAA approach
    WAA_i = NULL
  }

  # Basic info created above:
  basic_info <- gf_info
  # Add length information:
  ny <- length(basic_info$years)
  basic_info$lengths <- seq(2, 120, by=2) # length bins
  nlbins <- length(basic_info$lengths)
  basic_info$n_lengths <- nlbins
  # Choose data to be used in EM (IMPORTANT STEP!)
  # For fishery:
  if(df.ems$catch_data[i] == 'paa') basic_info$use_catch_paa <- matrix(1, ncol = basic_info$n_fleets, nrow = ny)
  if(df.ems$catch_data[i] == 'pal') {
    basic_info$use_catch_pal <- matrix(1, ncol = basic_info$n_fleets, nrow = ny)
    basic_info$use_catch_paa <- matrix(0, ncol = basic_info$n_fleets, nrow = ny) # turn off paa because default = 1
  }
  if(df.ems$catch_data[i] == 'caal') {
    basic_info$use_catch_caal <- array(1, dim = c(ny, basic_info$n_fleets, nlbins))
    basic_info$use_catch_paa <- matrix(0, ncol = basic_info$n_fleets, nrow = ny) # turn off paa because default = 1
  }
  # For survey:
  if(df.ems$index_data[i] == 'paa') basic_info$use_index_paa <- matrix(1, ncol = basic_info$n_indices, nrow = ny)
  if(df.ems$catch_data[i] == 'pal') {
    basic_info$use_index_pal <- matrix(1, ncol = basic_info$n_indices, nrow = ny)
    basic_info$use_index_paa <- matrix(0, ncol = basic_info$n_indices, nrow = ny) # turn off paa because default = 1
  }
  if(df.ems$index_data[i] == 'caal') {
    basic_info$use_index_caal <- array(1, dim = c(ny, basic_info$n_indices, nlbins))
    basic_info$use_index_paa <- matrix(0, ncol = basic_info$n_indices, nrow = ny) # turn off paa because default = 1
  }  
  
  # Continue....
  em_inputs[[i]] = prepare_wham_input(basic_info = basic_info,
                                      selectivity = selectivity_i, NAA_re = NAA_re_i, 
                                      M= M_i, WAA = WAA_i, catchability = Q_i,
                                      age_comp = "multinomial",
                                      len_comp = 'multinomial')
  
  #turn off bias correction
  em_inputs[[i]] = set_simulation_options(em_inputs[[i]], simulate_data = TRUE, simulate_process = TRUE, simulate_projection = FALSE,
                                          bias_correct_pe = FALSE, bias_correct_oe = FALSE)
  em_inputs[[i]]$map$log_NAA_sigma <- factor(NA*em_inputs[[i]]$par$log_NAA_sigma) # Fix NAA sigma
  em_inputs[[i]]$map$log_N1_pars <- factor(c(1, NA)) # Fix F1 initial
  
}

saveRDS(em_inputs, file.path("inputs", "em_inputs.RDS")) 
