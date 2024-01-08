# remotes::install_github(repo = 'GiancarloMCorrea/wham', ref='growth', INSTALL_opts = c("--no-docs", "--no-multiarch", "--no-demo"))

# -------------------------------------------------------------------------
# Create OM WHAM inputs 

# Load auxiliary functions:
source(file.path('code', "config_params.R"))
source(file.path('code', "make_basic_info.R"))
source(file.path('code', "make_om.R"))
source(file.path('code', "set_simulation_options.R"))

# Read Config Scenarios DF:
df.scenario = readRDS(file.path("inputs", "df.scenarios.RDS"))

# --------------------------------------------------------
# Parameter information:

#selectivity pars (len based always):
gf_selectivity = list(
  model = lensel_based$model,
  initial_pars = lensel_based$initial_pars)
# M pars:
gf_M = list(model = "constant",
            initial_means = M_base) 
# Q pars:
gf_Q = list(initial_q = Q_base,
            q_lower = rep(0, times = length(Q_base)),
            q_upper = rep(10, times = length(Q_base)), 
            prior_sd = rep(NA, times = length(Q_base)))
# Recruitment pars:
gf_NAA_re = list(N1_pars = c(N1_base, 0),
                sigma = "rec", #random about mean
                cor = "iid", #random effects are independent
                recruit_model = 2,
                recruit_pars = N1_base, # mean recruitment
                N1_model = 1) #defined above from naa_om_inputs
# Ecov pars:
gf_ecov <- list(
  label = "Ecov",
  process_model = 'ar1',
  lag = 0,
  mean = cbind(rep(0, length(years_base))),
  year = years_base,
  ages = list(ages_base),
  use_obs = cbind(rep(1, length(years_base))),
  how = 0
)

#  Growth configuration:
Linf <- G_base[2]
k_par <- G_base[1]
L1 <- G_base[3]
a_LW <- LW_base[1]
b_LW <- LW_base[2]
L_a <- Linf + (L1 - Linf)*exp(-k_par*(ages_base - 1))
W_a <- a_LW*L_a^b_LW
CV_a <- .1
gf_growth <- list(model='vB_classic', init_vals=c(k_par, Linf, L1),
                  SD_vals=c(CV_a*L_a[1], CV_a*L_a[max(ages_base)]))
gf_LW <- list(init_vals=c(a_LW, b_LW))

# Make OMs --------------------------------------------------------------
om_inputs = list()
for(i in 1:NROW(df.scenario)){
  
  # Print model name:
  print(paste0("row ", i))

  # Ecov information
  ecov_i = gf_ecov

  # ---------------------
  # Define obs error scenarios (data rich vs data poor):
  if(df.scenario$data_scen[i] == 'rich') {
    catch_sigma = matrix(0.025, ncol = n_fisheries, nrow = length(years_base))
    agg_index_cv = matrix(0.1, ncol = n_indices, nrow = length(years_base))
    # Neff values in OM:
    catch_Neff = matrix(200, ncol = n_fisheries, nrow = length(years_base)) # This will not be used, will be replaced later
    index_Neff = matrix(400, ncol = n_indices, nrow = length(years_base)) # This will not be used, will be replaced later
    catch_NeffL = matrix(8000, ncol = n_fisheries, nrow = length(years_base))
    index_NeffL = matrix(16000, ncol = n_indices, nrow = length(years_base))
    # Go to sim_core.R file to change the Nsamp for CAAL. Remember it should be smaller than PAL Nsamp 
    ecov_i$logsigma = cbind(rep(log(0.1), length(years_base))) # logsigma Ecov
    # Nsamp for WAA, this should change in the future (function of NAA), TODO:
    waa_cv = array(0.1, dim = c(n_fisheries+n_indices+2, length(years_base), length(ages_base)))
  }

  if(df.scenario$data_scen[i] == 'poor') {
    catch_sigma = matrix(0.1, ncol = n_fisheries, nrow = length(years_base))
    agg_index_cv = matrix(0.4, ncol = n_indices, nrow = length(years_base))
    # Neff values in OM:
    catch_Neff = matrix(50, ncol = n_fisheries, nrow = length(years_base)) # This will not be used, will be replaced later
    index_Neff = matrix(100, ncol = n_indices, nrow = length(years_base)) # This will not be used, will be replaced later
    catch_NeffL = matrix(2000, ncol = n_fisheries, nrow = length(years_base))
    index_NeffL = matrix(4000, ncol = n_indices, nrow = length(years_base))
    # Go to sim_core.R file to change the Nsamp for CAAL. Remember it should be smaller than PAL Nsamp 
    ecov_i$logsigma = cbind(rep(log(0.4), length(years_base))) # logsigma Ecov
    # Nsamp for WAA, this should change in the future (function of NAA), TODO:
    waa_cv = array(0.2, dim = c(n_fisheries+n_indices+2, length(years_base), length(ages_base)))
  }

  # ---------------------
  # Add more information to Ecov:
  if(df.scenario$growth_par[i] == 0){
    ecov_i$where = "none" # none effect
  } else {
    ecov_i$where = "growth" # effect on growth parameter
    ecov_i$where_subindex = df.scenario$growth_par[i] # select growth parameter
  }
  om_inputs[[i]] <- make_om(Fmax = F_max, 
                            years_base = years_base, ages_base = ages_base, lengths_base = lengths_base,
                            F_change_time = 0.8,
							               sigma_R = sigma_R,
                            selectivity = gf_selectivity,
                            M = gf_M, NAA_re = gf_NAA_re, ecov = ecov_i,
                            growth = gf_growth, LW = gf_LW,
                            catchability = gf_Q, 
                            n_fisheries = n_fisheries, n_indices = n_indices,
                            catch_sigma = catch_sigma, agg_index_cv = agg_index_cv,
                            catch_Neff = catch_Neff, index_Neff = index_Neff, catch_NeffL = catch_NeffL,
                            index_NeffL = index_NeffL, catch_Neff_caal = catch_Neff_caal, 
                            index_Neff_caal = index_Neff_caal, waa_cv = waa_cv,
                            Ecov_re_sig = Ecov_re_sig, Ecov_re_cor = Ecov_re_cor, Ecov_effect = Ecov_effect[df.scenario$growth_par[i]+1],
                            df.scenario = df.scenario[i,]) 
  om_inputs[[i]] = set_simulation_options(om_inputs[[i]], simulate_data = TRUE, simulate_process = TRUE, simulate_projection = FALSE,
    bias_correct_pe = TRUE, bias_correct_oe = TRUE) # do bias correction?
  
}

# Save OM inputs:
saveRDS(om_inputs, file.path(write.dir, "om_inputs.RDS"))


# -------------------------------------------------------------------------
# Define seeds:
#I don't think we want to use the same (e.g. 1000) seeds for everything.
set.seed(8675309)
seeds = sample(x = (-1e9):(1e9), size = NROW(df.scenario)*1000, replace = FALSE)
seeds <- lapply(1:NROW(df.scenario), function(x) seeds[(1:1000) + 1000*(x-1)])
saveRDS(seeds, file.path(write.dir,"seeds.RDS"))
seeds = readRDS(file.path(write.dir,"seeds.RDS"))
