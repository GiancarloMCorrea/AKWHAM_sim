# Organize Env_time series data:
require(forecast)
source(here::here('code', 'config_params.R'))
# Save ARIMA objects:
arima_mod = list()

# EBS Bottom temperature:
# Obtained from https://github.com/afsc-gap-products/coldpool
# 1985-2024, 2020 value is imputated from average 2019 and 2021
# Standardized to mean of zero
stationary_df = read.csv("env_data/EBS_Temp.csv")
stationary_df = stationary_df %>% dplyr::select(YEAR, AREA_LTE2_KM2)
imputate_2020 = mean(stationary_df$AREA_LTE2_KM2[stationary_df$YEAR %in% c(2019, 2021)])
stationary_df = stationary_df %>% add_row(YEAR = 2020, AREA_LTE2_KM2 = imputate_2020, .after = 38)
stationary_df = tail(stationary_df, n = n_years_base)
stationary_df = stationary_df %>% dplyr::mutate(year_id = row_number(), 
                                         var_std = scale(AREA_LTE2_KM2),
                                         #var_std = AREA_LTE2_KM2 - mean(AREA_LTE2_KM2),
                                         type = 'stationary') %>%
  dplyr::select(year_id, var_std, type)

# Fit ARIMA model:
stat_mod = forecast::Arima(stationary_df$var_std, order = c(1,0,0))
arima_mod[[1]] = stat_mod

# MidAtlantic Bight:
# Obtained from: https://github.com/NOAA-EDAB/ecodata
# 1984-2023
# Standardized to mean of zero
trend_df = read.csv("env_data/MidAtlantic_Bight.csv")
trend_df = trend_df %>% dplyr::filter(Var == 'cold_pool_index', 
                                      source %in% c('ROMS', 'GLORYS')) %>% 
  dplyr::select(Time, Value)
trend_df = tail(trend_df, n = n_years_base)
trend_df = trend_df %>% dplyr::mutate(year_id = row_number(), 
                                      var_std = scale(Value), 
                                      #var_std = Value - mean(Value),
                                      type = 'trend') %>%
  dplyr::select(year_id, var_std, type)

# Fit ARIMA model:
trend_mod = forecast::Arima(trend_df$var_std, order = c(1,0,0), include.drift = TRUE)
arima_mod[[2]] = trend_mod

# Save models:
names(arima_mod) = c('stationary', 'trend')
save(arima_mod, file = 'env_data/arima_mod.RData')
# Merge dfs and save:
env_df = bind_rows(stationary_df, trend_df)
saveRDS(env_df, file = 'env_data/env_sim.rds')
