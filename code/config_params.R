# Parameters to be used in OM to simulate data
# These values are also used in EM as starting values

# Years, Ages and lengths --------------------------------------------------------
years_base = 1970:2021
ages_base = 1:10
lengths_base = seq(from = 2, to = 120, by = 2)


# Mortality -------------------------------------------------------
M_base = 0.35
F_max = 0.35

# Catchability ------------------------------------------------------------
Q_base = 1


# Growth ------------------------------------------------------------------
G_base = c(0.2, 100, 10) # K, Linf, L1
# CV = 0.1*L[1] and 0.1*L[n_ages]


# LW relationship ---------------------------------------------------------
LW_base = c(exp(-12.1), 3.2) # a and b parameters LW


# Initial abundance -------------------------------------------------------
N1_base = 1e+05 # Initial Recruitment and mean recruitment over time period


# Selectivity ------------------------------------------------------
agesel_based = list(model = c("double-normal", "logistic"),
                    initial_pars = list(c(4, -2, 0, 0, -5, -3), c(1.5, 0.3)))
lensel_based = list(model = c("len-double-normal", "len-logistic"),
                    initial_pars = list(c(50,-1,4,4,-5,-2), c(15, 3)))



