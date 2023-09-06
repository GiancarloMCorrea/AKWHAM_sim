# Load required libraries
library(dplyr)

# folder to write scenarios df:
write.dir = "inputs"

# --------------------------------------------------------
# Create OM configuration df:
growth_par = 0:3 # none, k, Linf, and L1 separately
data_scen = c('poor', 'rich')

# Create EM configuration df:
method = c('EWAA', 'WAA', 
           'growth', 'growth', 'growth', 'growth',
           'Ecov', 'Ecov', 'Ecov', 'Ecov')
           #'SemiG', 'SemiG', 'SemiG', 'SemiG',
           #'LAA', 'LAA', 'LAA', 'LAA')
re_method = c(NA, '2dar1', 
              'ar1_y', 'ar1_y', 'ar1_y', 'ar1_y',
              'ar1', 'ar1', 'ar1', 'ar1')
              #'2dar1', '2dar1', '2dar1', '2dar1', 
              #'2dar1', '2dar1', '2dar1', '2dar1')
est_fixed = c(NA, TRUE, 
              TRUE, TRUE, TRUE, TRUE, 
              TRUE, TRUE, TRUE, TRUE)
              #TRUE, TRUE, TRUE, TRUE,
              #FALSE, FALSE, FALSE, FALSE)
catch_data = c('paa', 'paa', 
               'pal', 'pal', 'pal', 'pal',
               'pal', 'pal', 'pal', 'pal')
               #'pal', 'pal', 'pal', 'pal', 
               #'pal', 'pal', 'pal', 'pal')
index_data = c('paa', 'paa', 
               'pal', 'paa', 'caal', 'caal', 
               'pal', 'paa', 'caal', 'caal')
               #'pal', 'paa', 'caal', 'caal',
               #'pal', 'paa', 'caal', 'caal')
caal_samp = c(NA, NA, 
              NA, NA, 'random', 'strat',
              NA, NA, 'random', 'strat')
              #NA, NA, 'random', 'strat',
              #NA, NA, 'random', 'strat')

# Make scenario DF:
tmp.scenario = data.frame(growth_par = rep(growth_par, times = length(method)),
                         method = rep(method, each = length(growth_par)),
                         re_method = rep(re_method, each = length(growth_par)), 
                         est_fixed = rep(est_fixed, each = length(growth_par)), 
                         catch_data = rep(catch_data, each = length(growth_par)), 
                         index_data = rep(index_data, each = length(growth_par)),
                         caal_samp = rep(caal_samp, each = length(growth_par)))
df.scenario = tmp.scenario %>% slice(rep(1:n(), times = length(data_scen))) %>% 
                                mutate(data_scen = rep(data_scen, each = nrow(tmp.scenario)))

# Save scenario DF:
n.mods = dim(df.scenario)[1] 
df.scenario$Scenario <- paste0("Scenario_",1:n.mods)
df.scenario <- df.scenario %>% select(Scenario, everything()) 
# Turn off RE structure when growth_par = 0 (except for Ecov)
df.scenario$re_method[df.scenario$method %in% c('WAA', 'growth', 'LAA', 'SemiG') & df.scenario$growth_par == 0] = 'none'
saveRDS(df.scenario, file.path(write.dir, "df.scenarios.RDS"))
