
# -------------------------------------------------------------------------
# Create EM WHAM inputs 

# Load auxiliary functions:
source(file.path('code', "config_params.R"))
source(file.path('code', "make_basic_info.R"))
source(file.path('code', "set_simulation_options.R"))

# Read Config Scenarios DF:
df.scenario = readRDS(file.path("inputs", "df.scenarios.RDS"))

# --------------------------------------------------------
# Parameter information:

# Selectivity configuration (age-based)
gf_selectivity_age = list(
  model = agesel_based$model,
  initial_pars = agesel_based$initial_pars,
  # fix_pars = list(NULL, NULL), # estimate both parameters
  fix_pars = list(2, 2), # fix second parameter
  n_selblocks = n_fisheries + n_indices)
# Selectivity configuration (len-based)
gf_selectivity_len = list(
  model = lensel_based$model,
  initial_pars = lensel_based$initial_pars,
  # fix_pars = list(NULL, NULL), # estimate both parameters
  fix_pars = list(2, 2), # fix second parameter
  n_selblocks = n_fisheries + n_indices)


# Natural mortality (fixed)
gf_M = list(initial_means = M_base, model = "constant")
# Q pars (estimated):
gf_Q = list(initial_q = Q_base,
            q_lower = rep(0, times = length(Q_base)),
            q_upper = rep(10, times = length(Q_base)), 
            prior_sd = rep(NA, times = length(Q_base)))
# NAA configuration
gf_NAA_re = list(N1_pars = rep(N1_base, times = length(ages_base)), # will be replaced later
                sigma = "rec", #random about mean
                cor = "iid", #random effects are independent
                recruit_model = 2,
                N1_model = 0) # age-specific
# Ecov configuration:
gf_ecov <- list(
  label = "Ecov_sim",
  lag = 0,
  mean = cbind(rep(0, n_years_base)), # replace by sim data
  year = (n_years_burnin+1):(n_years_base+n_years_burnin),
  ages = list(ages_base),
  use_obs = cbind(rep(1, n_years_base)),
  how = 0) 

# True parameter values:
Linf <- G_base[2]
k_par <- G_base[1]
L1 <- G_base[3]
a_LW <- LW_base[1]
b_LW <- LW_base[2]
L_a <- Linf + (L1 - Linf)*exp(-k_par*(ages_base - 1))
W_a <- a_LW*L_a^b_LW

# Biological configuration:
gf_LAA <- list(model='vB_classic', init_vals=G_base[1:3],
                  SD_vals=G_base[4:5],
                  SD_est = 1:2) # Always estimate SD parameters
gf_WAA <- list(model = 'Allometric', init_vals = LW_base) # fixed
gf_mat = list(model = 'len-logistic', init_vals = mat_base_len)

#make inputs for estimating model (smaller objects to save, can overwrinte data elements with simulated data)
em_inputs = list()
for(i in 1:NROW(df.scenario)){

  print(paste0("EM config ", i))
  NAA_re_i = gf_NAA_re
  M_i = gf_M
  WAA_i = gf_WAA
  LAA_i = gf_LAA
  mat_i = gf_mat
  Ecov_i = gf_ecov
  selectivity_i = gf_selectivity_age # will be replaced below
  Q_i = gf_Q

  # ---------------------
  # Define obs error scenarios (data rich vs data poor):
  if(df.scenario$data_scen[i] == 'rich') {
    catch_sigma = matrix(0.025, ncol = n_fisheries, nrow = n_years_base)
    agg_index_cv = matrix(0.1, ncol = n_indices, nrow = n_years_base)
    catch_Neff = matrix(100, ncol = n_fisheries, nrow = n_years_base)
    index_Neff = matrix(200, ncol = n_indices, nrow = n_years_base)
    catch_NeffL = matrix(100, ncol = n_fisheries, nrow = n_years_base)
    index_NeffL = matrix(200, ncol = n_indices, nrow = n_years_base)
    # Go to sim_core.R file to change the Nsamp for CAAL. Remember it should be smaller than PAL Nsamp 
    Ecov_i$logsigma = cbind(rep(log(0.4), n_years_base)) # logsigma Ecov
    # Nsamp for WAA, this should change in the future (function of NAA), TODO:
    waa_cv = array(0.1, dim = c(n_fisheries+n_indices+2, n_years_base, length(ages_base)))
  }

  if(df.scenario$data_scen[i] == 'poor') {
    catch_sigma = matrix(0.1, ncol = n_fisheries, nrow = n_years_base)
    agg_index_cv = matrix(0.4, ncol = n_indices, nrow = n_years_base)
    catch_Neff = matrix(25, ncol = n_fisheries, nrow = n_years_base)
    index_Neff = matrix(50, ncol = n_indices, nrow = n_years_base)
    catch_NeffL = matrix(25, ncol = n_fisheries, nrow = n_years_base)
    index_NeffL = matrix(50, ncol = n_indices, nrow = n_years_base)
    # Go to sim_core.R file to change the Nsamp for CAAL. Remember it should be smaller than PAL Nsamp 
    Ecov_i$logsigma = cbind(rep(log(0.8), n_years_base)) # logsigma Ecov
    # Nsamp for WAA, this should change in the future (function of NAA), TODO:
    waa_cv = array(0.2, dim = c(n_fisheries+n_indices+2, n_years_base, length(ages_base)))
  }

  # Change input parameters information -------------------------------

  # EWAA approach:
  if(df.scenario$method[i] == 'EWAA') { 
    WAA_i = NULL
    mat_i = NULL
    LAA_i = NULL
    Ecov_i = NULL 
  }
  # nonparametric WAA approach:
  if(df.scenario$method[i] == 'WAA') { 
    WAA_i$model = 'Nonparametric'
    WAA_i$init_vals = W_a
    WAA_i$re = df.scenario$re_method[i] # random effects structure
    WAA_i$est_pars = ages_base # estimate fixed effects? Yes
    LAA_i = NULL # turn off LAA
    mat_i = NULL
    Ecov_i = NULL # turn off ecov
  }
  # parametric approach:
  if(df.scenario$method[i] == 'growth') { 
	  if(df.scenario$growth_var[i] == 1) LAA_i$re = c(df.scenario$re_method[i], df.scenario$re_method[i], 'none')
	  if(df.scenario$growth_var[i] == 2) LAA_i$re = c('none', 'none', df.scenario$re_method[i])
	  LAA_i$est_pars = 1:3 # estimate growth parameters
    Ecov_i = NULL # turn off ecov
  }
  # Ecov approach:
  if(df.scenario$method[i] == 'Ecov') { 
    Ecov_i$process_model = NA # random effects structure (always)
    if(df.scenario$growth_var[i] == 0) {
      Ecov_i = NULL
    } else {
      Ecov_i$process_model = df.scenario$re_method[i]
      Ecov_i$where = 'LAA'
      if(df.scenario$growth_var[i] == 1) Ecov_i$parameter_i = list(c(1,2))
      if(df.scenario$growth_var[i] == 2) Ecov_i$parameter_i = list(c(3))
    }
    LAA_i$est_pars = 1:3 # estimate growth parameters
  }  

  # Make basic inputs (defined above)
  basic_info = make_basic_info(n_years_base = n_years_base, n_years_burnin = n_years_burnin, 
                               type = 'em',
                              ages = ages_base, fish_len = lengths_base,
                              n_fisheries = n_fisheries, n_indices = n_indices,
                              catch_sigma = catch_sigma, agg_index_cv = agg_index_cv,
                              catch_Neff = catch_Neff, index_Neff = index_Neff, 
                              catch_NeffL = catch_NeffL,
                              index_NeffL = index_NeffL, catch_Neff_caal = catch_Neff_caal, 
                              index_Neff_caal = index_Neff_caal, waa_cv = waa_cv)

  # Add length information:
  ny <- length(basic_info$years)
  nlbins <- length(basic_info$lengths)
  # F placeholder:
  basic_info$F = matrix(0.1, ncol = basic_info$n_fleets, nrow = ny)
  # Choose data to be used in EM (IMPORTANT STEP!)

  # For fishery:
  if(df.scenario$catch_data[i] == 'paa') {
	 basic_info$use_catch_paa <- matrix(1, ncol = basic_info$n_fleets, nrow = ny)
	 selectivity_i$model[1] = agesel_based$model[1] # use age-based selectivity when only age data for fishery or survey
	 selectivity_i$initial_pars[[1]] = agesel_based$initial_pars[[1]]
  }
  if(df.scenario$catch_data[i] == 'pal') {
    basic_info$use_catch_pal <- matrix(1, ncol = basic_info$n_fleets, nrow = ny)
    basic_info$use_catch_paa <- matrix(0, ncol = basic_info$n_fleets, nrow = ny) # turn off paa because default = 1
	  selectivity_i$model[1] = lensel_based$model[1] # use len-based selectivity when len data for fishery or survey
	  selectivity_i$initial_pars[[1]] = lensel_based$initial_pars[[1]]
  }
  if(df.scenario$catch_data[i] == 'caal') {
    basic_info$use_catch_pal <- matrix(1, ncol = basic_info$n_fleets, nrow = ny)
    basic_info$use_catch_caal <- array(1, dim = c(ny, basic_info$n_fleets, nlbins)) # will be replaced in sim_core.R
    basic_info$use_catch_paa <- matrix(0, ncol = basic_info$n_fleets, nrow = ny) # turn off paa because default = 1
	  selectivity_i$model[1] = lensel_based$model[1] # use len-based selectivity when len data for fishery or survey
	  selectivity_i$initial_pars[[1]] = lensel_based$initial_pars[[1]]
  }

  # For survey:
  if(df.scenario$index_data[i] == 'paa') {
	 basic_info$use_index_paa <- matrix(1, ncol = basic_info$n_indices, nrow = ny)
	 selectivity_i$model[2] = agesel_based$model[2] # use age-based selectivity when only age data for fishery or survey
	 selectivity_i$initial_pars[[2]] = agesel_based$initial_pars[[2]]
  }
  if(df.scenario$index_data[i] == 'pal') {
    basic_info$use_index_pal <- matrix(1, ncol = basic_info$n_indices, nrow = ny)
    basic_info$use_index_paa <- matrix(0, ncol = basic_info$n_indices, nrow = ny) # turn off paa because default = 1
	  selectivity_i$model[2] = lensel_based$model[2] # use len-based selectivity when len data for fishery or survey
	  selectivity_i$initial_pars[[2]] = lensel_based$initial_pars[[2]]
  }
  if(df.scenario$index_data[i] == 'caal') {
    basic_info$use_index_pal <- matrix(1, ncol = basic_info$n_indices, nrow = ny) # use len comps as well
    basic_info$use_index_caal <- array(1, dim = c(ny, basic_info$n_indices, nlbins)) # will be replaced in sim_core.R
    basic_info$use_index_paa <- matrix(0, ncol = basic_info$n_indices, nrow = ny) # turn off paa because default = 1
	  selectivity_i$model[2] = lensel_based$model[2] # use len-based selectivity when len data for fishery or survey
	  selectivity_i$initial_pars[[2]] = lensel_based$initial_pars[[2]]
  }
  # Turn on use of waa as obs (only use survey data):
  if(df.scenario$method[i] == 'WAA') basic_info$use_index_waa = matrix(1, ncol = basic_info$n_indices, nrow = ny)
  # Model time varying selectivity? (only for EWAA and WAA)
  if(df.scenario$age_selex[i] == 'varying') {
    selectivity_i$re = c('iid', 'iid') # for both fishery and survey
  }
  
  # Continue....
  em_inputs[[i]] = prepare_wham_input(basic_info = basic_info,
                                      selectivity = selectivity_i, NAA_re = NAA_re_i, 
                                      M= M_i, catchability = Q_i, WAA = WAA_i,
                                      LAA = LAA_i, maturity = mat_i,
                                      ecov = Ecov_i,
                                      age_comp = "multinomial",
                                      len_comp = 'multinomial')
  
  #turn on bias correction?
  em_inputs[[i]] = set_simulation_options(em_inputs[[i]], simulate_data = TRUE, 
                                          simulate_process = TRUE, simulate_projection = FALSE,
                                          bias_correct_pe = TRUE, bias_correct_oe = TRUE)
  # Fix some parameters:
  #em_inputs[[i]]$par$log_NAA_sigma = log(sigma_R)
  #em_inputs[[i]]$map$log_NAA_sigma <- factor(NA) # Fix NAA sigma
  em_inputs[[i]]$map$log_N1_pars <- factor(rep(NA, times = length(ages_base))) # Fix N1 pars
  # Define random variable:
  #em_inputs[[i]]$random = NULL # default for EWAA
  #if(df.scenario$method[i] == 'WAA') em_inputs[[i]]$random = 'WAA_re'
  #if(df.scenario$method[i] == 'growth' & df.scenario$growth_var[i] > 0) em_inputs[[i]]$random = 'LAA_re'
  #if(df.scenario$method[i] == 'Ecov' & df.scenario$growth_var[i] > 0) em_inputs[[i]]$random = 'Ecov_re' 
  # if(df.scenario$age_selex[i] == 'varying') {
  #   em_inputs[[i]]$map$sel_repars = factor(rep(NA, times = length(em_inputs[[i]]$map$sel_repars)))
  #   em_inputs[[i]]$par$sel_repars[,1] = log(0.15) # fixed
  # }

}

saveRDS(em_inputs, file.path("inputs", "em_inputs.RDS")) 
