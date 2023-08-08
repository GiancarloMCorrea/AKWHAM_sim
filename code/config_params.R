# Parameters to be used in OM to simulate data
# These values are also used in EM as starting values

# Years, Ages and lengths --------------------------------------------------------
years_base = 1981:2020 # 40 years
ages_base = 1:10
lengths_base = seq(from = 2, to = 120, by = 2)
n_fisheries = 1
n_indices = 1

# Mortality -------------------------------------------------------
M_base = 0.35
F_max = 0.35

# Catchability ------------------------------------------------------------
Q_base = 1

# Growth ------------------------------------------------------------------
G_base = c(0.2, 90, 10) # K, Linf, L1

# LW relationship ---------------------------------------------------------
LW_base = c(exp(-12.1), 3.2) # a and b parameters LW

# Initial abundance and sigmaR -------------------------------------------------------
N1_base = 1e+05 # Initial Recruitment and mean recruitment over time period
sigma_R = 0.6 # sigma R recruitment

# Selectivity ------------------------------------------------------
agesel_based = list(model = c("double-normal", "logistic"),
                    initial_pars = list(c(4, -2, 0, 0, -5, -3), c(1.5, 0.3)))
lensel_based = list(model = c("len-double-normal", "len-logistic"),
                    initial_pars = list(c(50,-1,4,4,-5,-2), c(15, 3)))

# Ecov information ------------------------------------------------------

Ecov_re_sig = c(0) # Ecov process error SD (this will be exp() in WHAM)
Ecov_re_cor <- c(0) # Ecov process error autocorrelation
Ecov_effect <- c(0.25, 0.15, 0.3) # Effect on growth parameter (Beta, parameter-specific: k, Linf, L1)

# Obs error information ------------------------------------------------------
catch_sigma = matrix(0.05, ncol = n_fisheries, nrow = length(years_base))
agg_index_cv = matrix(0.2, ncol = n_indices, nrow = length(years_base))
catch_Neff = matrix(100, ncol = n_fisheries, nrow = length(years_base))
index_Neff = matrix(100, ncol = n_indices, nrow = length(years_base))
catch_NeffL = matrix(100, ncol = n_fisheries, nrow = length(years_base))
index_NeffL = matrix(100, ncol = n_indices, nrow = length(years_base))
catch_Neff_caal = array(5, dim = c(length(years_base), n_fisheries, length(lengths_base)))
index_Neff_caal = array(5, dim = c(length(years_base), n_indices, length(lengths_base)))
Ecov_obs = 0.2
waa_cv = array(0.1, dim = c(n_fisheries+n_indices+2, length(years_base), length(ages_base)))

