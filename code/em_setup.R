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
em_method = c('EWAA', 'WAA', 'WAA')
em_re_method = c(NA, '2dar1', '2dar1')
em_est_fixed = c(NA, FALSE, TRUE)
df.ems = data.frame(method = em_method, re_method = em_re_method, est_fixed = em_est_fixed)
saveRDS(df.ems, file.path("inputs", "df.ems.RDS"))

# Make basic inputs
gf_info = make_basic_info()

# Selectivity configuration (age-based for now)
gf_selectivity = list(
  model = c("double-normal", "logistic"),
  initial_pars = list(c(4,-1,1.5,1.4,0,0.4), c(1,0.5)))

# Natural mortality
gf_M = list(initial_means = 0.2, model = "constant")

# NAA configuration
gf_NAA_re = list(
  N1_pars = c(1e+05, 0),
  sigma = "rec", #random about mean
  cor = "iid", #random effects are independent
  recruit_model = 2,
  N1_model = 1
)

# True parameter values:
Linf <- 85
k <- 0.3
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
  selectivity = gf_selectivity

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
  # Age comps:
  basic_info$use_catch_paa <- matrix(1, ncol = basic_info$n_fleets, nrow = ny)
  basic_info$use_index_paa <- matrix(1, ncol = basic_info$n_indices, nrow = ny)

  em_inputs[[i]] = prepare_wham_input(basic_info = basic_info,
                                       selectivity = selectivity, NAA_re = NAA_re_i, M= M_i,
                                       WAA = WAA_i, 
                                       age_comp = "logistic-normal-miss0",
                                       len_comp = 'multinomial')

  #turn off bias correction
  em_inputs[[i]] = set_simulation_options(em_inputs[[i]], simulate_data = TRUE, simulate_process = TRUE, simulate_projection = FALSE,
    bias_correct_pe = FALSE, bias_correct_oe = FALSE)
  #change Neff so scalar doesn't affect L-N SD
  em_inputs[[i]]$data$catch_Neff[] = 1
  ## not sure why this has to be done (me either, Giancarlo)
  em_inputs[[i]]$data$index_Neff[] <- 1
  em_inputs[[i]]$data$FXSPR_init[] = 0.3
  em_inputs[[i]]$data$FMSY_init[] = 0.3
  em_inputs[[i]]$map$log_NAA_sigma <- factor(NA*em_inputs[[i]]$par$log_NAA_sigma)
}

saveRDS(em_inputs, file.path("inputs", "em_inputs.RDS"))
