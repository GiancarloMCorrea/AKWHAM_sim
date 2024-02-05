# Load required libraries
library(dplyr)

# folder to write scenarios df:
write.dir = "inputs"

# --------------------------------------------------------
# Create OM configuration df:
growth_par = 0:3 # none, k, Linf, and L1 separately
data_scen = c('poor', 'rich')
samp_scheme = c('random', 'strat')
Ecov_sim = c('stationary', 'trend')

# -------------------------------------------------------------------------
# Create EM configuration df (first set: EWAA, WAA, growth, Ecov)
method1 = c('EWAA', 'EWAA', 'WAA', 'WAA',
           'growth', 'growth', 'growth', 'growth', 'growth',
           'Ecov', 'Ecov', 'Ecov', 'Ecov', 'Ecov')
re_method1 = c(NA, NA, '2dar1', '2dar1', 
              'ar1_y', 'ar1_y', 'ar1_y', 'ar1_y', 'ar1_y',
              'ar1', 'ar1', 'ar1', 'ar1', 'ar1')
est_fixed1 = c(NA, NA, TRUE, TRUE, 
              TRUE, TRUE, TRUE, TRUE, TRUE, 
              TRUE, TRUE, TRUE, TRUE, TRUE)
catch_data1 = c('paa', 'paa', 'paa', 'paa',
               'pal', 'pal', 'pal', 'pal', 'pal',
               'pal', 'pal', 'pal', 'pal', 'pal')
index_data1 = c('paa', 'paa',  'paa', 'paa',
               'pal', 'paa', 'paa', 'caal', 'caal', 
               'pal', 'paa', 'paa', 'caal', 'caal')
caal_samp1 = c(samp_scheme, samp_scheme,
              NA,samp_scheme, samp_scheme,
              NA, samp_scheme, samp_scheme)

# Make scenario DF1:
tmp.scenario1 = data.frame(growth_par = rep(growth_par, times = length(method1)),
                          method = rep(method1, each = length(growth_par)),
                          re_method = rep(re_method1, each = length(growth_par)), 
                          est_fixed = rep(est_fixed1, each = length(growth_par)), 
                          catch_data = rep(catch_data1, each = length(growth_par)), 
                          index_data = rep(index_data1, each = length(growth_par)),
                          caal_samp = rep(caal_samp1, each = length(growth_par)))
df.scenario1 = tmp.scenario1 %>% slice(rep(1:n(), times = length(data_scen))) %>% 
                  mutate(data_scen = rep(data_scen, each = nrow(tmp.scenario1)))

# 
# # -------------------------------------------------------------------------
# # Create EM configuration df (second set: LAA or SemiG)
# method2 = c('SemiG', 'SemiG', 'SemiG', 'SemiG',
#             'LAA', 'LAA', 'LAA', 'LAA')
# re_method2 = c('2dar1', '2dar1', '2dar1', '2dar1', 
#                '2dar1', '2dar1', '2dar1', '2dar1')
# est_fixed2 = c(TRUE, TRUE, TRUE, TRUE,
#                TRUE, TRUE, TRUE, TRUE)
# catch_data2 = c('pal', 'pal', 'pal', 'pal', 
#                 'pal', 'pal', 'pal', 'pal')
# index_data2 = c('pal', 'paa', 'caal', 'caal',
#                 'pal', 'paa', 'caal', 'caal')
# caal_samp2 = c(NA, NA, 'random', 'strat',
#                NA, NA, 'random', 'strat')
# 
# # Make scenario DF2:
# tmp.scenario2 = data.frame(growth_par = rep(growth_par, times = length(method2)),
#                            method = rep(method2, each = length(growth_par)),
#                            re_method = rep(re_method2, each = length(growth_par)), 
#                            est_fixed = rep(est_fixed2, each = length(growth_par)), 
#                            catch_data = rep(catch_data2, each = length(growth_par)), 
#                            index_data = rep(index_data2, each = length(growth_par)),
#                            caal_samp = rep(caal_samp2, each = length(growth_par)))
# df.scenario2 = tmp.scenario2 %>% slice(rep(1:n(), times = length(data_scen))) %>% 
#   mutate(data_scen = rep(data_scen, each = nrow(tmp.scenario2)))


# -------------------------------------------------------------------------
# Merge both data.frames:
#df.scenario = rbind(df.scenario1, df.scenario2)
df.scenario = df.scenario1 %>% slice(rep(1:n(), times = length(Ecov_sim))) %>% 
  mutate(Ecov_sim = rep(Ecov_sim, each = nrow(df.scenario1)))

# Save scenario DF:
n.mods = dim(df.scenario)[1] 
df.scenario$Scenario <- paste0("Scenario_",1:n.mods)
df.scenario <- df.scenario %>% select(Scenario, everything()) 
# Turn off RE structure when growth_par = 0 (except for Ecov)
# df.scenario$re_method[df.scenario$method %in% c('WAA', 'growth', 'LAA', 'SemiG') & df.scenario$growth_par == 0] = 'none'
df.scenario$re_method[df.scenario$method %in% c('WAA', 'growth') & df.scenario$growth_par == 0] = 'none'
saveRDS(df.scenario, file.path(write.dir, "df.scenarios.RDS"))


# -------------------------------------------------------------------------
# Define seeds:
# These seeds will be the same across scenarios
set.seed(8675309)
seeds = sample(x = (-1e9):(1e9), size = 1000, replace = FALSE) # max number of replicates per scenario: 1000, but will only use 100 or 150
saveRDS(seeds, file.path(write.dir,"seeds.RDS"))
seeds = readRDS(file.path(write.dir,"seeds.RDS"))

