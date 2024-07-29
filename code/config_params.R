# Parameters to be used in OM to simulate data
# These values are also used in EM as starting values

# Years, Ages and lengths --------------------------------------------------------
n_years_base = 45 # for EM and OM
n_years_burnin = 10 # only for OM
ages_base = 1:10
lengths_base = seq(from = 2, to = 130, by = 2)
n_fisheries = 1
n_indices = 1

# Mortality -------------------------------------------------------
M_base = 0.35
F_max = 0.35

# Catchability ------------------------------------------------------------
Q_base = 1

# Growth ------------------------------------------------------------------
G_base = c(0.2, 90, 10, 2, 8) # K, Linf, L1, SD1, SDA

# LW relationship ---------------------------------------------------------
LW_base = c(exp(-12.1), 3.2) # a and b parameters LW

# Maturity relationship ---------------------------------------------------------
mat_base_len = c(0.5, 45) # a and b parameters len maturity
mat_base_age = c(5, 3.5) # a and b parameters age maturity

# Initial abundance and sigmaR -------------------------------------------------------
N1_base = 1e+05 # Initial Recruitment and mean recruitment over time period
sigma_R = 0.6 # sigma R recruitment

# Selectivity ------------------------------------------------------
agesel_based = list(model = c("logistic", "logistic"),
                    initial_pars = list(c(3.5, 0.2), c(1.5, 0.2)))
lensel_based = list(model = c("len-logistic", "len-logistic"),
                    initial_pars = list(c(45, 3), c(15, 3)))

# Ecov information ------------------------------------------------------

Ecov_re_sig = c(0) # Ecov process error SD (this will be exp() in WHAM)
Ecov_re_cor <- c(0.3) # Ecov process error autocorrelation: in WHAM: -1 + 2/(1 + exp(-phi))
Ecov_effect <- c(0.2, -0.1, 0.25) # Effect on growth parameter (Beta, parameter-specific: k, Linf, L1)
# K and Linf have negatively correlated variability
Ecov_trend = c(0, 0.03)
# When ecov trend positive, year when this trend will start to be simulated:
Ecov_year_trend = 40

# Obs error information ------------------------------------------------------
# Defined in om_setup.R and em_setup.R depending on the data quality scenario