# remotes::install_github(repo = 'GiancarloMCorrea/wham', ref='growth', INSTALL_opts = c("--no-docs", "--no-multiarch", "--no-demo"))


# Load libraries ----------------------------------------------------------
library(wham)
library(tidyr)
library(dplyr)

source(file.path('code', "config_params.R"))
source(file.path('code', "make_basic_info.R"))
source(file.path('code', "set_simulation_options.R"))

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
gf_info = make_basic_info(base_years = years_base, ages = ages_base, fish_len = lengths_base)

# Selectivity configuration (age-based for now)
gf_selectivity = list(
  model = agesel_based$model,
  initial_pars = agesel_based$initial_pars,
  fix_pars = list(5:6, NULL))

# Natural mortality (estimated)
gf_M = list(initial_means = M_base, model = "constant", est_ages = 1)
# Q pars (estimated):
gf_Q = list(initial_q = Q_base,
            q_lower = c(0),
            q_upper = c(10), prior_sd = c(NA))
# NAA configuration
gf_NAA_re = list(N1_pars = c(N1_base, 0),
                sigma = "rec", #random about mean
                cor = "iid", #random effects are independent
                recruit_model = 2,
                N1_model = 1)

# True parameter values:
Linf <- G_base[2]
k_par <- G_base[1]
L1 <- G_base[3]
a_LW <- LW_base[1]
b_LW <- LW_base[2]
L_a <- Linf + (L1 - Linf)*exp(-k_par*(ages_base - 1))
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
      WAA_i$est_pars = ages_base
    }
  }
  if(df.ems$method[i] == 'EWAA') { # EWAA approach
    WAA_i = NULL
  }

  # Basic info created above:
  basic_info <- gf_info
  # Add length information:
  ny <- length(basic_info$years)
  nlbins <- length(basic_info$lengths)
  # F placeholder:
  basic_info$F = matrix(0.1, ncol = basic_info$n_fleets, nrow = ny)
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
  # Turn on use of EWAA as obs (only use survey data):
  if(df.ems$method[i] == 'WAA') basic_info$use_index_waa = matrix(1, ncol = basic_info$n_indices, nrow = ny)
  
  # Continue....
  em_inputs[[i]] = prepare_wham_input(basic_info = basic_info,
                                      selectivity = selectivity_i, NAA_re = NAA_re_i, 
                                      M= M_i, WAA = WAA_i, catchability = Q_i,
                                      age_comp = "multinomial",
                                      len_comp = 'multinomial')
  
  #turn off bias correction
  em_inputs[[i]] = set_simulation_options(em_inputs[[i]], simulate_data = TRUE, simulate_process = TRUE, simulate_projection = FALSE,
                                          bias_correct_pe = FALSE, bias_correct_oe = FALSE)
  # Fix some parameters:
  em_inputs[[i]]$map$log_NAA_sigma <- factor(NA*em_inputs[[i]]$par$log_NAA_sigma) # Fix NAA sigma
  em_inputs[[i]]$map$log_N1_pars <- factor(c(1, NA)) # Fix F1 initial
  if(df.ems$method[i] == 'WAA') em_inputs[[i]]$random = 'WAA_re'
    
}

saveRDS(em_inputs, file.path("inputs", "em_inputs.RDS")) 
