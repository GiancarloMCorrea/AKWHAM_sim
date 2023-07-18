# remotes::install_github(repo = 'GiancarloMCorrea/wham', ref='growth', INSTALL_opts = c("--no-docs", "--no-multiarch", "--no-demo"))

# -------------------------------------------------------------------------
# OM configuration 

# Load required libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(wham)

# Load auxiliary functions:
source(file.path('code', "make_basic_info.R"))
source(file.path('code', "make_om.R"))
source(file.path('code', "sim_management.R"))
source(file.path('code', "set_simulation_options.R"))
source(file.path('code', "set_q.R"))
source(file.path('code', "set_M.R"))

# folder to write dfom and dfem
write.dir = "inputs"

# OM parameters:
Ecov_obs_sig = c(0.1) # obs error variance for Ecov
Ecov_re_sig = c(0.1) # Ecov process error variance
Ecov_re_cor <- c(0) # Ecov process error autocorrelation
Ecov_effect <- c(0.5) # Effect on selected parameter (Beta)
growth_par = 1:3 # on k, Linf, and L1 separately
# Make OM df:
df.oms <- expand.grid(Ecov_obs_sig=Ecov_obs_sig, Ecov_re_sig=Ecov_re_sig, Ecov_re_cor=Ecov_re_cor, Ecov_effect = Ecov_effect,
                      growth_par = growth_par,  stringsAsFactors = FALSE)
n.mods = dim(df.oms)[1] 
df.oms$Model <- paste0("om_",1:n.mods)
df.oms <- df.oms %>% select(Model, everything()) 
saveRDS(df.oms, file.path(write.dir, "df.oms.RDS"))

# Create basic WHAM input:
gf_info = make_basic_info()
#selectivity pars:
gf_selectivity = list(
  model = c("double-normal", "logistic"),
  initial_pars = list(c(4,-3,-1,-1.5,0,0), c(1,0.5)))
# M pars:
gf_M = list(model = "constant",
            initial_means = 0.2)
# Q pars:
gf_Q = list(initial_q = 1,
            q_lower = c(0),
            q_upper = c(10), prior_sd = c(NA))
# Recruitment pars:
gf_NAA_re = list(N1_pars = c(1e+05, 0),
                sigma = "rec", #random about mean
                cor = "iid", #random effects are independent
                recruit_model = 2,
                N1_model = 1) #defined above from naa_om_inputs
# Ecov pars:
gf_ecov <- list(
  label = "Ecov",
  process_model = "ar1",
  lag = 0,
  mean = cbind(rep(0, length(gf_info$years))),
  year = gf_info$years,
  ages = list(1:10),
  use_obs = cbind(rep(1, length(gf_info$years)))
)

#  Growth configuration:
Linf <- 100
k <- 0.2
t0 <- 0
a_LW <- exp(-12.1)
b_LW <- 3.2
L_a <- Linf*(1-exp(-k*(1:10 - t0)))
W_a <- a_LW*L_a^b_LW
CV_a <- .1
gf_growth <- list(model='vB_classic', init_vals=c(k, Linf, L_a[1]),
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
  om_inputs[[i]] <- make_om(Fmax = 0.5, Fmin = 0.1,
                            F_change_time = 0.7,
                            selectivity = gf_selectivity,
                            M = gf_M, NAA_re = gf_NAA_re, ecov = ecov_i,
                            growth = gf_growth, LW = gf_LW,
                            catchability = gf_Q, 
                            om_input = TRUE, df.oms = df.oms[i,]) 
  om_inputs[[i]] = set_simulation_options(om_inputs[[i]], simulate_data = TRUE, simulate_process = TRUE, simulate_projection = FALSE,
    bias_correct_pe = FALSE, bias_correct_oe = FALSE)
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
