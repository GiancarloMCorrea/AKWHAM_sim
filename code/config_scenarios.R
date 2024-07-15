# Load required libraries
library(dplyr)

# folder to write scenarios df:
write.dir = "inputs"

# We will create the scenarios df by blocks:
# growth_var:
# 0: no growth variability
# 1: variability on k and Linf
# 2: variability on L1

# -------------------------------------------------------------------------
# Age only scenarios:
growth_var = 0:2 # none, k-Linf, or  L1
samp_scheme = c('random', 'strat')
Ecov_sim = c('stationary', 'trend')
method = c('EWAA', 'WAA')
age_selex = c('fixed', 'varying')
catch_data = 'paa'
index_data = 'paa'

age_df = expand.grid(growth_var = growth_var, caal_samp = samp_scheme, 
                     Ecov_sim = Ecov_sim, method = method, 
                     age_selex = age_selex, catch_data = catch_data,
                     index_data = index_data, stringsAsFactors = FALSE)
age_df = age_df %>% mutate(re_method = if_else(method == 'EWAA', 'none', '2dar1'))
# Only time varying selex when growth_var is zero:
age_df$age_selex[age_df$growth_var == 0] = 'fixed'
# No Ecov sim type when growth_var = 0
age_df$Ecov_sim[age_df$growth_var == 0] = 'none'

# Delete repeating scenarios:
age_df = age_df[!duplicated(age_df), ]

# -------------------------------------------------------------------------
# Length only scenarios:
growth_var = 0:2 # none, k-Linf, or  L1
samp_scheme = 'none'
Ecov_sim = c('stationary', 'trend')
method = c('growth', 'Ecov')
age_selex = 'fixed'
catch_data = 'pal'
index_data = 'pal'

len_df = expand.grid(growth_var = growth_var, caal_samp = samp_scheme, 
                     Ecov_sim = Ecov_sim, method = method, 
                     age_selex = age_selex, catch_data = catch_data,
                     index_data = index_data, stringsAsFactors = FALSE)
len_df = len_df %>% mutate(re_method = 'none')
# Change RE method depending on modelling approach
len_df$re_method[len_df$method == 'growth' & len_df$growth_var > 0] = 'ar1_y'
len_df$re_method[len_df$method == 'Ecov' & len_df$growth_var > 0] = 'ar1'
# No Ecov sim type when growth_var = 0
len_df$Ecov_sim[len_df$growth_var == 0] = 'none'
# Growth and Ecov same approach when growth_var is 0
len_df$method[len_df$growth_var == 0] = 'growth'

# Delete repeating scenarios:
len_df = len_df[!duplicated(len_df), ]

# -------------------------------------------------------------------------
# Age-length scenarios:
growth_par = 0:2 # none, L1, or k-Linf
samp_scheme = c('random', 'strat')
Ecov_sim = c('stationary', 'trend')
method = c('growth', 'Ecov')
age_selex = 'fixed'
catch_data = 'pal'
index_data = c('caal', 'paa')

agelen_df = expand.grid(growth_var = growth_var, caal_samp = samp_scheme, 
                     Ecov_sim = Ecov_sim, method = method, 
                     age_selex = age_selex, catch_data = catch_data,
                     index_data = index_data, stringsAsFactors = FALSE)
agelen_df = agelen_df %>% mutate(re_method = 'none')
agelen_df$re_method[agelen_df$method == 'growth' & agelen_df$growth_var > 0] = 'ar1_y'
agelen_df$re_method[agelen_df$method == 'Ecov' & agelen_df$growth_var > 0] = 'ar1'
# No Ecov sim type when growth_var = 0
agelen_df$Ecov_sim[agelen_df$growth_var == 0] = 'none'
# Growth and Ecov same approach when growth_var is 0
agelen_df$method[agelen_df$growth_var == 0] = 'growth'

# Delete repeating scenarios:
agelen_df = agelen_df[!duplicated(agelen_df), ]

# -------------------------------------------------------------------------
# Make scenario DF:
tmp_scenario = rbind(age_df, len_df, agelen_df)

# -------------------------------------------------------------------------
# Merge both data.frames:
df.scenario = tmp_scenario

# Save scenario DF:
n.mods = dim(df.scenario)[1] 
df.scenario$scenario <- 1:n.mods
rownames(df.scenario) = df.scenario$scenario
saveRDS(df.scenario, file.path(write.dir, "df.scenarios.RDS"))


# -------------------------------------------------------------------------
# Define seeds:
# These seeds will be the same across scenarios
set.seed(8675309)
seeds = sample(x = (-1e9):(1e9), size = 1000, replace = FALSE) # max number of replicates per scenario: 1000, but will only use 100 or 150
saveRDS(seeds, file.path(write.dir,"seeds.RDS"))
seeds = readRDS(file.path(write.dir,"seeds.RDS"))

