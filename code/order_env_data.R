# Organize Env_time series data:
source(here::here('code', 'config_params.R'))

# EBS Bottom temperature:
# Obtained from https://github.com/afsc-gap-products/coldpool
# 1985-2024, 2020 value is imputated from average 2019 and 2021
# Standardized to mean of zero
stationary_df = read.csv("env_data/EBS_Temp.csv")
stationary_df = stationary_df %>% dplyr::select(YEAR, MEAN_GEAR_TEMPERATURE)
imputate_2020 = mean(stationary_df$MEAN_GEAR_TEMPERATURE[stationary_df$YEAR %in% c(2019, 2021)])
stationary_df = stationary_df %>% add_row(YEAR = 2020, MEAN_GEAR_TEMPERATURE = imputate_2020, .after = 38)
stationary_df = tail(stationary_df, n = n_years_base)
stationary_df = stationary_df %>% dplyr::mutate(year_id = row_number(), 
                                         var_std = scale(MEAN_GEAR_TEMPERATURE),
                                         type = 'stationary') %>%
  dplyr::select(year_id, var_std, type)

# MidAtlantic Bight:
# Obtained from: https://github.com/NOAA-EDAB/ecodata
# 1984-2023
# Standardized to mean of zero
trend_df = read.csv("env_data/MidAtlantic_Bight.csv")
trend_df = trend_df %>% dplyr::filter(Var == 'cold_pool_index', 
                                      source %in% c('ROMS', 'GLORYS')) %>% 
  dplyr::select(Time, Value)
trend_df = tail(trend_df, n = n_years_base)
trend_df = trend_df %>% dplyr::mutate(year_id = row_number(), var_std = scale(Value), type = 'trend') %>%
  dplyr::select(year_id, var_std, type)

# Merge dfs and save:
env_df = bind_rows(stationary_df, trend_df)
saveRDS(env_df, file = 'env_data/env_sim.rds')