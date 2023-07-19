# remotes::install_github(repo = 'GiancarloMCorrea/wham', ref='growth', INSTALL_opts = c("--no-docs", "--no-multiarch", "--no-demo"))

# -------------------------------------------------------------------------
# OM configuration 

# Load required libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(wham)

# Load auxiliary functions:
source(file.path('code', "config_params.R"))
source(file.path('code', "make_basic_info.R"))
source(file.path('code', "make_om.R"))
source(file.path('code', "set_simulation_options.R"))
source(file.path('code', "make_plot_om.R"))

# folder to write dfom and dfem
write.dir = "inputs"

# OM parameters:
Ecov_obs_sig = c(0.1) # obs error variance for Ecov
Ecov_re_sig = c(0) # Ecov process error SD (this will be exp() in WHAM)
Ecov_re_cor <- c(0) # Ecov process error autocorrelation
Ecov_effect <- c(0.25) # Effect on selected parameter (Beta)
growth_par = 1:3 # on k, Linf, and L1 separately
# Make OM df:
df.oms <- expand.grid(Ecov_obs_sig=Ecov_obs_sig, Ecov_re_sig=Ecov_re_sig, Ecov_re_cor=Ecov_re_cor, Ecov_effect = Ecov_effect,
                      growth_par = growth_par,  stringsAsFactors = FALSE)
n.mods = dim(df.oms)[1] 
df.oms$Model <- paste0("om_",1:n.mods)
df.oms <- df.oms %>% select(Model, everything()) 
saveRDS(df.oms, file.path(write.dir, "df.oms.RDS"))

#selectivity pars:
gf_selectivity = list(
  model = agesel_based$model,
  initial_pars = agesel_based$initial_pars)
# M pars:
gf_M = list(model = "constant",
            initial_means = M_base)
# Q pars:
gf_Q = list(initial_q = Q_base,
            q_lower = c(0),
            q_upper = c(10), prior_sd = c(NA))
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
  process_model = "ar1",
  lag = 0,
  mean = cbind(rep(0, length(years_base))),
  year = years_base,
  ages = list(ages_base),
  use_obs = cbind(rep(1, length(years_base)))
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
                  SD_vals=c(CV_a*L_a[1], CV_a*L_a[10]))
gf_LW <- list(init_vals=c(a_LW, b_LW))

# Make OMs --------------------------------------------------------------
om_inputs = list()
for(i in 1:NROW(df.oms)){
  
  # Print model name:
  print(paste0("row ", i))
  
  # Add more information to Ecov:
  ecov_i = gf_ecov
  ecov_i$logsigma = cbind(rep(log(df.oms$Ecov_obs_sig[i]), length(ecov_i$year)))
  if(df.oms$Ecov_effect[i] < 1e-7){
    ecov_i$how = 0
    ecov_i$where = "none"
  } else {
    ecov_i$how = 1
    ecov_i$where = "growth"
    ecov_i$where_subindex = df.oms$growth_par[i]
  }
  om_inputs[[i]] <- make_om(Fmax = F_max, 
                            years_base = years_base, ages_base = ages_base, lengths_base = lengths_base,
                            F_change_time = 0.8,
                            selectivity = gf_selectivity,
                            M = gf_M, NAA_re = gf_NAA_re, ecov = ecov_i,
                            growth = gf_growth, LW = gf_LW,
                            catchability = gf_Q, 
                            df.oms = df.oms[i,]) 
  om_inputs[[i]] = set_simulation_options(om_inputs[[i]], simulate_data = TRUE, simulate_process = TRUE, simulate_projection = FALSE,
    bias_correct_pe = FALSE, bias_correct_oe = FALSE)
  
  # Make basic plots:
  if(make_OM_figures){
    
    om_toPlot = fit_wham(input = om_inputs[[i]], do.fit = FALSE, MakeADFun.silent = TRUE)
    make_plot_om(om_toPlot, i)
    
  }
}

# Save OM inputs:
saveRDS(om_inputs, file.path(write.dir, "om_inputs.RDS"))


# -------------------------------------------------------------------------
# Define seeds:
#I don't think we want to use the same (e.g. 1000) seeds for everything.
set.seed(8675309)
seeds = sample(x = (-1e9):(1e9), size = NROW(df.oms)*1000, replace = FALSE)
seeds <- lapply(1:NROW(df.oms), function(x) seeds[(1:1000) + 1000*(x-1)])
saveRDS(seeds, file.path(write.dir,"seeds.RDS"))
seeds = readRDS(file.path(write.dir,"seeds.RDS"))
