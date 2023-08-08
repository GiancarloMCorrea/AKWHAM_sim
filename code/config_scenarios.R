# Load required libraries
library(dplyr)

# folder to write scenarios df:
write.dir = "inputs"

# --------------------------------------------------------
# Create OM configuration df:
growth_par = 1:3 # on k, Linf, and L1 separately

# Create EM configuration df:
method = c('EWAA', 'WAA', 'growth', 'growth', 'Ecov', 'Ecov', 'SemiG', 'SemiG', 'LAA', 'LAA')
re_method = c(NA, '2dar1', 'ar1_y', 'ar1_y', 'ar1', 'ar1', '2dar1', '2dar1', '2dar1', '2dar1')
est_fixed = c(NA, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
catch_data = c('paa', 'paa', 'paa', 'pal', 'paa', 'pal', 'paa', 'pal', 'paa', 'pal')
index_data = c('paa', 'paa', 'pal', 'paa', 'pal', 'paa', 'pal', 'paa', 'pal', 'paa')

# Make scenario DF:
df.scenario = data.frame(growth_par = rep(growth_par, times = length(method)),
                         method = rep(method, each = length(growth_par)),
                         re_method = rep(re_method, each = length(growth_par)), 
                         est_fixed = rep(est_fixed, each = length(growth_par)), 
                         catch_data = rep(catch_data, each = length(growth_par)), 
                         index_data = rep(index_data, each = length(growth_par)))

# Save scenario DF:
n.mods = dim(df.scenario)[1] 
df.scenario$Scenario <- paste0("Scenario_",1:n.mods)
df.scenario <- df.scenario %>% select(Scenario, everything()) 
saveRDS(df.scenario, file.path(write.dir, "df.scenarios.RDS"))
