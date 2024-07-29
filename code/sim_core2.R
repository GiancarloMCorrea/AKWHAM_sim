#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
simi = as.integer(args[1])
scenj = as.integer(args[2])

# Set WD
main_dir = here::here() 
out_dir = here::here('results') # folder where all simulations will be saved. preferably out of GitHub folder

# Load required libraries:
library(wham)
library(dplyr)
source(here::here("code", "make_om_plots.R"))
source(here::here("code", "config_params.R"))
# Read inputs:
om_inputs <- readRDS(here::here("inputs", "om_inputs.RDS"))
em_inputs <- readRDS(here::here("inputs", "em_inputs.RDS"))
df.scenario <- readRDS(here::here("inputs", "df.scenarios.RDS"))
seeds <- readRDS(here::here("inputs","seeds.RDS"))

# Make data.frame summarizing scenario:
this_scenario <- data.frame(df.scenario[scenj, ])
model <- cbind(im = simi, scenario = scenj, optimized=FALSE, sdreport=FALSE, this_scenario)
this_dat_scen = match(this_scenario$data_scen, c('poor', 'rich'))
this_paagen_scen = match(this_scenario$paa_generation, c('traditional', 'stepwise'))
this_om_input = om_inputs[[this_scenario$growth_var + 1]][[this_paagen_scen]][[this_dat_scen]] # select the OM based on growth_var

#######################################################
# Print scenario name:
cat(paste0("START Scenario: ", scenj, " Sim: ", simi, "\n"))

# -------------------------------------------------------------------------
# Simulate environmental time series:

# none or stationary. when none, it wont affect any growth parameter
if(this_scenario$Ecov_sim %in% c('none', 'stationary')) {
  
  set.seed(seeds[simi])
  ecov_error = rnorm(n_years_base+n_years_burnin, mean = 0, sd = exp(Ecov_re_sig))
  alpha = 0
  beta = Ecov_trend[1] # trend
  theta = -1 + 2/(1 + exp(-Ecov_re_cor)) # as in WHAM
  sim_ecov = 0
  for(i in 2:length(ecov_error)) sim_ecov[i] = alpha+beta*i+theta*sim_ecov[i-1] + ecov_error[i]
  sim_ecov = scale(sim_ecov)

}

if(this_scenario$Ecov_sim == 'trend') {
  
  set.seed(seeds[simi])
  ecov_error = rnorm(n_years_base+n_years_burnin, mean = 0, sd = exp(Ecov_re_sig))
  alpha = 0
  beta = Ecov_trend[2] # trend
  theta = -1 + 2/(1 + exp(-Ecov_re_cor)) # as in WHAM
  sim_ecov = 0
  for(i in 2:length(ecov_error)) {
    if(i < Ecov_year_trend) {
      sim_ecov[i] = alpha+0*i+theta*sim_ecov[i-1] + ecov_error[i] # beta = 0
    } else {
      sim_ecov[i] = alpha+beta*i+theta*sim_ecov[i-1] + ecov_error[i]
    }
  }
  sim_ecov = scale(sim_ecov)
  # plot(sim_ecov, type = 'l')
}

# Now replace the sim_ecov in the OM input:
this_om_input$par$Ecov_re = sim_ecov

#######################################################
# Run OM:
om <- fit_wham(this_om_input, do.fit = FALSE, MakeADFun.silent = TRUE)
# Define seed and simulate WHAM data:
set.seed(seeds[simi])
sim_data <- om$simulate(complete=TRUE)
if(simi == 1 & scenj %in% c(1:3, 25:27)) {
  saveRDS(object = om, file = file.path(main_dir, "sample_data", 'om_sample', paste0("om_sample_", scenj, ".RDS"))) # Save OM data to make plots later
  if(this_scenario$paa_generation == 'traditional') make_plot_traditional(sim_data, scenj, main_dir) # Make plot
  if(this_scenario$paa_generation == 'stepwise') make_plot_stepwise(sim_data, scenj, main_dir) # Make plot
}
if(simi <= 10 & scenj %in% c(2:3)) { # LAA variability by Ecov type. Only 10 iterations
  # Simulated LAA in jan 1:
  this_laa = sim_data$jan1LAA
  colnames(this_laa) = 1:sim_data$n_ages
  rownames(this_laa) = 1:sim_data$n_years_model
  this_df = reshape2::melt(this_laa, varnames = c('year', 'age'))
  this_df$growth_var = this_scenario$growth_var
  this_df$sim = simi
  this_df$ecov = this_scenario$Ecov_sim
  saveRDS(object = this_df, file = file.path(main_dir, "sample_data", 'LAA_sample', paste0("sample_", scenj, '-', simi, ".RDS"))) # Save sim LAA to make plots later
  # Simulate growth parameters:
  this_par = sim_data$LAA_par[,1,]
  colnames(this_par) = c('k', 'Linf', 'L1')
  rownames(this_par) = 1:sim_data$n_years_model
  this_df = reshape2::melt(this_par, varnames = c('year', 'par'))
  this_df$growth_var = this_scenario$growth_var
  this_df$sim = simi
  this_df$ecov = this_scenario$Ecov_sim
  saveRDS(object = this_df, file = file.path(main_dir, "sample_data", 'LAApar_sample', paste0("sample_", scenj, '-', simi, ".RDS"))) # Save sim parameters to make plots later
}

# CAAL sampling: ----------------------------------------------
# Only works for multinomial 

#  Fishery:
if(this_scenario$catch_data == 'caal' | this_scenario$catch_data == 'paa') {
  
  if(this_scenario$data_scen == 'poor') Nsamp_CAAL = 13 # Nsamp size for CAAL
  if(this_scenario$data_scen == 'rich') Nsamp_CAAL = 25 # Nsamp size for CAAL
  # Nsamp_CAAL = 25 # Nsamp size for CAAL

  # Order to sort: year, fleet, len bin, age
  to_obsvec = NULL
  for(j in 1:sim_data$n_years_model) {
    for(i in 1:sim_data$n_fleets) {
      if(this_scenario$caal_samp == 'random') {
        # Random sampling:
        len_subsam = rmultinom(n = 1, size = Nsamp_CAAL, prob = sim_data$catch_pal[i,j,]) 
        len_subsam = as.vector(len_subsam)
      }
      if(this_scenario$caal_samp == 'strat') {
        # Length-stratified sampling:
        len_samp = sim_data$catch_pal[i,j,]*sim_data$catch_NeffL[j,i]
        len_subsam = numeric(length(len_samp)) # save CAAL Neff
        if(sum(len_samp) == Nsamp_CAAL) {
          len_subsam = len_samp
        } else {
          i_len_samp = len_samp
          for(s in 1:Nsamp_CAAL) {
            n_left_i = length(which(i_len_samp > 0)) 
            n_in_i = sum(len_subsam)
            if((n_left_i+n_in_i) < Nsamp_CAAL) {
              # Sample all lengths in length sample
              pos_samp = which(i_len_samp > 0)
              len_subsam[pos_samp] = len_subsam[pos_samp] + 1
              i_len_samp = len_samp - len_subsam
            } else {
              # Random sample of remaining lengths to complete Nsamp CAAL
              pos_samp = sample(x = which(i_len_samp > 0), size = Nsamp_CAAL-n_in_i, replace = F)
              len_subsam[pos_samp] = len_subsam[pos_samp] + 1
              break
            }
          } # loop
        }
      } # conditional
      
      # Continue code:
      for(k in 1:sim_data$n_lengths) {
        sim_data$catch_caal_Neff[j,i,k] = len_subsam[k]
        if(sum(sim_data$pred_CAAL[j,i,k,]) == 0) tmp_caal_sim = matrix(0, ncol = 1, nrow = sim_data$n_ages)
        else tmp_caal_sim = rmultinom(n = 1, size = len_subsam[k], prob = sim_data$pred_CAAL[j,i,k,])
        # Save sim sampling:
        to_obsvec = c(to_obsvec, tmp_caal_sim*sim_data$catch_caal_Neff[j,i,k])
        if(sum(tmp_caal_sim) == 0) sim_data$catch_caal[i,j,k,] = 0
        else sim_data$catch_caal[i,j,k,] = tmp_caal_sim[,1]/sum(tmp_caal_sim[,1])
      } # len bin loop
      # Replace use/not use:
      sim_data$use_catch_caal[j,i,] = ifelse(test = len_subsam > 0, yes = 1, no = 0)
      
    } # fleet loop
  } # year loop
  
  # Now replace values CAAL in obsvec vector:
  sim_data$obsvec[sim_data$obs$type == 'catchcaal'] = to_obsvec
  
  # Age comps calculation:
  # First, calculate average ALK across years to be used later:
  avg_alk = list(sim_data$n_fleets)
  for(i in 1:sim_data$n_fleets) {
    out_alk = matrix(0, ncol = sim_data$n_ages, nrow = sim_data$n_lengths)
    for(j in 1:sim_data$n_years_model) {
      tmp_alk = sim_data$catch_caal[i,j,,] * sim_data$catch_caal_Neff[j,i,]
      out_alk = out_alk + tmp_alk
    }
    avg_alk[[i]] = t(apply(t(out_alk), 2, function(i) i/sum(i)))
    avg_alk[[i]][is.nan(avg_alk[[i]])] = 0 # replace nan with zeros
  }
  # now calculate age comps:
  to_obsvec_paa = NULL
  for(j in 1:sim_data$n_years_model) {
    for(i in 1:sim_data$n_fleets) {
        if(this_scenario$paa_generation == 'stepwise') {
          tmp_paa = numeric(sim_data$n_ages)
          for(k in 1:sim_data$n_lengths) {
            obs_len = sim_data$catch_pal[i,j,k] > 0
            if(obs_len) {
              obs_caal = sum(sim_data$catch_caal[i,j,k,]) > 0
              if(obs_caal) { # use year-specific ALK
                tmp_paa = tmp_paa + sim_data$catch_pal[i,j,k] * sim_data$catch_caal[i,j,k,]
              } else { # use average ALK across years
                tmp_paa = tmp_paa + sim_data$catch_pal[i,j,k] * avg_alk[[i]][k,]
              }
            } # obs_len conditional
          } # len loop
          tmp_paa = tmp_paa/sum(tmp_paa) # in case there no len info in CAAL
          sim_data$catch_paa[i,j,] = tmp_paa
        } # Conditional Stepwise
        if(this_scenario$paa_generation == 'traditional') {
          tmp_paa = sim_data$catch_paa[i,j,]
        } # Conditional Traditional
      to_obsvec_paa = c(to_obsvec_paa, tmp_paa*sim_data$catch_Neff[j,i]) # Neff of EM
    } # fleet loop
  } # year loop
  
  # Now replace values PAA in obsvec vector:
  sim_data$obsvec[sim_data$obs$type == 'catchpaa'] = to_obsvec_paa
  
  # Simulate observed/empirical mean weight-at-age:
  if(this_scenario$method == 'WAA' | this_scenario$method == 'EWAA') {
    to_obsvec = NULL
    for(j in 1:sim_data$n_years_model) {
      for(i in 1:sim_data$n_fleets) {
        caal_obs = (sim_data$catch_caal_Neff[j,i,]) * sim_data$catch_caal[i,j,,] 
        for(a in 1:sim_data$n_ages) {
          ind_wt = rep(x = sim_data$wt_at_len[j,], times = caal_obs[,a])
          if(length(ind_wt) == 0) {
            mean_wt = NA
            cv_wt = 0 # when there is no information in ALK. WHAM will ignore this obs when using WAA approach
          }
          if(length(ind_wt) == 1) {
            mean_wt = mean(ind_wt)
            cv_wt = 0.25 # when there is only one observation
          }
          if(length(ind_wt) > 1 & length(unique(ind_wt)) == 1) {
            mean_wt = mean(ind_wt)
            cv_wt = 0.1 # when there is 2+ observations but they have the same value
          }
          if(length(ind_wt) > 1 & length(unique(ind_wt)) > 1) {
            mean_wt = mean(ind_wt)
            cv_wt = sd(ind_wt)/(sqrt(length(ind_wt))*mean_wt) # when there is 2+ observations
          }
          if(this_scenario$paa_generation == 'stepwise') {
            sim_data$waa[sim_data$waa_pointer_fleets[i],j,a] = mean_wt
            sim_data$waa_cv[sim_data$waa_pointer_fleets[i],j,a] = cv_wt
          }
        } # ages loop
      } # fleet loop
    } # year loop
    
    # replace NA (waa obs) with average value:
    for(i in 1:sim_data$n_fleets) {
      if(this_scenario$paa_generation == 'stepwise') {
        for(a in 1:sim_data$n_ages) {
          avg_waa = mean(sim_data$waa[sim_data$waa_pointer_fleets[i],,a], na.rm = TRUE)
          these_na = which(is.na(sim_data$waa[sim_data$waa_pointer_fleets[i],,a]))
          sim_data$waa[sim_data$waa_pointer_fleets[i],these_na,a] = avg_waa
          if(is.na(avg_waa)) { # this only useful for EWAA. maybe not realistic but whatever. also, this will probably never be used.
            this_mean = log(sim_data$pred_waa[sim_data$waa_pointer_fleets[i],these_na,a])
            this_sd = sqrt(log(0.2^2 + 1.0)) # CV = 0.2
            this_mean = this_mean - (this_sd^2)*0.5 # corrected mean
            sim_data$waa[sim_data$waa_pointer_fleets[i],these_na,a] = exp(mapply(FUN = rnorm, n = 1, mean = this_mean, sd = this_sd))
          }
        }
      } # if paa generation
      to_obsvec = c(to_obsvec, as.vector(t(sim_data$waa[sim_data$waa_pointer_fleets[i],,])))
    }
    
    # Now replace values in obsvec WAA vector:
    sim_data$obsvec[sim_data$obs$type == 'catchwaa'] = to_obsvec
  } # waa conditional
  
} # catch_caal conditional


#  Survey:
if(this_scenario$index_data == 'caal' | this_scenario$index_data == 'paa') {
  
  if(this_scenario$data_scen == 'poor') Nsamp_CAAL = 50 # Nsamp size for CAAL
  if(this_scenario$data_scen == 'rich') Nsamp_CAAL = 100 # Nsamp size for CAAL
  # Nsamp_CAAL = 100 # Nsamp size for CAAL
  
  # Order to sort: year, fleet, len bin, age
  to_obsvec = NULL
  to_obsvec_paa = NULL
  for(j in 1:sim_data$n_years_model) {
    for(i in 1:sim_data$n_indices) {
      if(this_scenario$caal_samp == 'random') {
        # Random sampling:
        len_subsam = rmultinom(n = 1, size = Nsamp_CAAL, prob = sim_data$index_pal[i,j,]) 
        len_subsam = as.vector(len_subsam)
      }
      if(this_scenario$caal_samp == 'strat') {
        # Length-stratified sampling:
        len_samp = sim_data$index_pal[i,j,]*sim_data$index_NeffL[j,i]
        len_subsam = numeric(length(len_samp)) # save CAAL Neff
        if(sum(len_samp) == Nsamp_CAAL) {
          len_subsam = len_samp
        } else {
          i_len_samp = len_samp
          for(s in 1:Nsamp_CAAL) {
            n_left_i = length(which(i_len_samp > 0)) 
            n_in_i = sum(len_subsam)
            if((n_left_i+n_in_i) < Nsamp_CAAL) {
              # Sample all lengths in length sample
              pos_samp = which(i_len_samp > 0)
              len_subsam[pos_samp] = len_subsam[pos_samp] + 1
              i_len_samp = len_samp - len_subsam
            } else {
              # Random sample of remaining lengths to complete Nsamp CAAL
              pos_samp = sample(x = which(i_len_samp > 0), size = Nsamp_CAAL-n_in_i, replace = F)
              len_subsam[pos_samp] = len_subsam[pos_samp] + 1
              break
            }
          } # loop
        }
      }# conditional
      
      # Continue code:
      for(k in 1:sim_data$n_lengths) {
        sim_data$index_caal_Neff[j,i,k] = len_subsam[k]
        if(sum(sim_data$pred_IAAL[j,i,k,]) == 0) tmp_caal_sim = matrix(0, ncol = 1, nrow = sim_data$n_ages)
        else tmp_caal_sim = rmultinom(n = 1, size = len_subsam[k], prob = sim_data$pred_IAAL[j,i,k,])
        # Save sim sampling:
        to_obsvec = c(to_obsvec, tmp_caal_sim*sim_data$index_caal_Neff[j,i,k])
        if(sum(tmp_caal_sim) == 0) sim_data$index_caal[i,j,k,] = 0
        else sim_data$index_caal[i,j,k,] = tmp_caal_sim[,1]/sum(tmp_caal_sim[,1])
      } # len bin loop
      # Replace use/not use:
      sim_data$use_index_caal[j,i,] = ifelse(test = len_subsam > 0, yes = 1, no = 0)
      
    } # fleet loop
  } # year loop
  
  # Now replace values CAAL in obsvec vector:
  sim_data$obsvec[sim_data$obs$type == 'indexcaal'] = to_obsvec

  # Age comps calculation:
  # First, calculate average ALK across years to be used later:
  avg_alk = list(sim_data$n_indices)
  for(i in 1:sim_data$n_indices) {
    out_alk = matrix(0, ncol = sim_data$n_ages, nrow = sim_data$n_lengths)
    for(j in 1:sim_data$n_years_model) {
      tmp_alk = sim_data$index_caal[i,j,,] * sim_data$index_caal_Neff[j,i,]
      out_alk = out_alk + tmp_alk
    }
    avg_alk[[i]] = t(apply(t(out_alk), 2, function(i) i/sum(i)))
    avg_alk[[i]][is.nan(avg_alk[[i]])] = 0 # replace nan with zeros
  }
  # now calculate age comps:
  to_obsvec_paa = NULL
  for(j in 1:sim_data$n_years_model) {
    for(i in 1:sim_data$n_indices) {
      if(this_scenario$paa_generation == 'stepwise') {
        tmp_paa = numeric(sim_data$n_ages)
        for(k in 1:sim_data$n_lengths) {
          obs_len = sim_data$index_pal[i,j,k] > 0
          if(obs_len) {
            obs_caal = sum(sim_data$index_caal[i,j,k,]) > 0
            if(obs_caal) { # use year-specific ALK
              tmp_paa = tmp_paa + sim_data$index_pal[i,j,k] * sim_data$index_caal[i,j,k,]
            } else { # use average ALK across years
              tmp_paa = tmp_paa + sim_data$index_pal[i,j,k] * avg_alk[[i]][k,]
            }
          } # obs_len conditional
        } # len loop
        tmp_paa = tmp_paa/sum(tmp_paa) # in case there no len info in CAAL
        sim_data$index_paa[i,j,] = tmp_paa
      }
      if(this_scenario$paa_generation == 'traditional') {
        tmp_paa = sim_data$index_paa[i,j,]
      } # Conditional Traditional
      to_obsvec_paa = c(to_obsvec_paa, tmp_paa*sim_data$index_Neff[j,i]) # Neff of EM
    } # fleet loop
  } # year loop
  
  # Now replace values PAA in obsvec vector:
  sim_data$obsvec[sim_data$obs$type == 'indexpaa'] = to_obsvec_paa
  
  
  # Simulate observed/empirical mean weight-at-age:
  if(this_scenario$method == 'WAA' | this_scenario$method == 'EWAA') {
    to_obsvec = NULL
    for(j in 1:sim_data$n_years_model) {
      for(i in 1:sim_data$n_indices) {
        caal_obs = (sim_data$index_caal_Neff[j,i,]) * sim_data$index_caal[i,j,,] # multiply by 10 to come back to real Neff
        for(a in 1:sim_data$n_ages) {
          ind_wt = rep(x = sim_data$wt_at_len[j,], times = caal_obs[,a])
          if(length(ind_wt) == 0) {
            mean_wt = NA
            cv_wt = 0 # when there is no information in ALK. WHAM will ignore this obs when using WAA approach
          }
          if(length(ind_wt) == 1) {
            mean_wt = mean(ind_wt)
            cv_wt = 0.3 # when there is only one observation
          }
          if(length(ind_wt) > 1 & length(unique(ind_wt)) == 1) {
            mean_wt = mean(ind_wt)
            cv_wt = 0.1 # when there is 2+ observations but they have the same value
          }
          if(length(ind_wt) > 1 & length(unique(ind_wt)) > 1) {
            mean_wt = mean(ind_wt)
            cv_wt = sd(ind_wt)/(sqrt(length(ind_wt))*mean_wt) # when there is 2+ observations
          }
          if(this_scenario$paa_generation == 'stepwise') {
            sim_data$waa[sim_data$waa_pointer_indices[i],j,a] = mean_wt
            sim_data$waa_cv[sim_data$waa_pointer_indices[i],j,a] = cv_wt
          }
        } # ages loop
      } # fleet loop
    } # year loop
    
    # replace NA (waa obs) with average value:
    for(i in 1:sim_data$n_indices) {
      if(this_scenario$paa_generation == 'stepwise') {
        for(a in 1:sim_data$n_ages) {
          avg_waa = mean(sim_data$waa[sim_data$waa_pointer_indices[i],,a], na.rm = TRUE)
          these_na = which(is.na(sim_data$waa[sim_data$waa_pointer_indices[i],,a]))
          sim_data$waa[sim_data$waa_pointer_indices[i],these_na,a] = avg_waa
          if(is.na(avg_waa)) { # this only useful for EWAA. maybe not realistic. also, this will probably never be used.
            this_mean = log(sim_data$pred_waa[sim_data$waa_pointer_indices[i],these_na,a])
            this_sd = sqrt(log(0.2^2 + 1.0)) # CV = 0.2
            this_mean = this_mean - (this_sd^2)*0.5 # corrected mean
            sim_data$waa[sim_data$waa_pointer_indices[i],these_na,a] = exp(mapply(FUN = rnorm, n = 1, mean = this_mean, sd = this_sd))
          }
        }
      } # if paa generation
      to_obsvec = c(to_obsvec, as.vector(t(sim_data$waa[sim_data$waa_pointer_indices[i],,])))
    }
    
    # Now replace values in obsvec WAA vector:
    sim_data$obsvec[sim_data$obs$type == 'indexwaa'] = to_obsvec
  } # waa conditional
    
} # index_caal conditional


# Read EM input data:
EM_input <- em_inputs[[scenj]] 

# -------------------------------------------------------------------------
# Now prepare new sim_data$obs$val, sim_data$obsvec and sim_data$agesvec for matching with EM_input
filter_year = sim_data$obs$year %in% (n_years_burnin + 1):(n_years_base + n_years_burnin)
obsvec_new = sim_data$obsvec[filter_year]
agesvec_new = sim_data$agesvec[filter_year]
obs_new = data.frame(year = sim_data$obs$year[filter_year] - n_years_burnin, # to match year values in EM
                     bin = sim_data$obs$bin[filter_year],
                     fleet = sim_data$obs$fleet[filter_year],
                     type = sim_data$obs$type[filter_year])
obs_new = obs_new %>% dplyr::mutate(ind_match = paste(year, bin, fleet, type, sep = '-'))
# EM ind match:
EM_ind_match = paste(EM_input$data$obs$year, EM_input$data$obs$bin, 
                     EM_input$data$obs$fleet, EM_input$data$obs$type, sep = '-')
# Find matching and replace values in EM from sim_data:
match_rows = obs_new$ind_match %in% EM_ind_match
EM_input$data$obsvec = obsvec_new[match_rows]
EM_input$data$agesvec = agesvec_new[match_rows]
EM_input$data$obs$val = EM_input$data$obsvec # not needed but whatever

# -------------------------------------------------------------------------
# Pass new caal_Neff and use_caal to EM_input:
if(this_scenario$catch_data %in% c('caal')) {
  for(i in 1:n_fisheries) {
    EM_input$data$catch_caal_Neff[,i,] = sim_data$catch_caal_Neff[(n_years_burnin+1):(n_years_burnin+n_years_base),i,]
    EM_input$data$use_catch_caal[,i,] = sim_data$use_catch_caal[(n_years_burnin+1):(n_years_burnin+n_years_base),i,]
  }
}
if(this_scenario$index_data %in% c('caal')) {
  for(i in 1:n_indices) {
    EM_input$data$index_caal_Neff[,i,] = sim_data$index_caal_Neff[(n_years_burnin+1):(n_years_burnin+n_years_base),i,]
    EM_input$data$use_index_caal[,i,] = sim_data$use_index_caal[(n_years_burnin+1):(n_years_burnin+n_years_base),i,]
  }
}

# -------------------------------------------------------------------------
# Pass waa and waa_cv data since it is not in obs data.frame:
if(this_scenario$method %in% c('EWAA', 'WAA')) {
    EM_input$data$waa = sim_data$waa[,(n_years_burnin+1):(n_years_burnin+n_years_base),]
    EM_input$data$waa_cv = sim_data$waa_cv[,(n_years_burnin+1):(n_years_burnin+n_years_base),]
}

# -------------------------------------------------------------------------
# Maturity
if(this_scenario$method %in% c('EWAA', 'WAA')) {
  EM_input$data$mature = sim_data$mat_at_age
}

# -------------------------------------------------------------------------

# Continue code:
truth = sim_data
# Save the version for reproducibility
truth$wham_version = om$wham_version
# Create data.frame saving parameter names:
ompars <- data.frame(par=names(om$par), value=om$par) |> dplyr::filter(par!='F_devs')
ompars$par2 <- sapply(unique(ompars$par), function(x) {
  y <- which(ompars$par==x)
  if(length(y)==1) return(x)
  x <- paste(x, 1:length(y), sep='_')
  return(x)
}) %>% unlist
res <- list(truth = truth, model = model, ompars = ompars)
res$fit <- list()

# Modify initial values for nonparametric approach:
# This is important. Convergence problems might appear if this is not done
if(this_scenario$method == 'WAA') {
  EM_input$par$WAA_a = log(colMeans(EM_input$data$waa[2,,]))
}

# Fix N1 par at true values:
EM_input$par$log_N1_pars = log(sim_data$NAA[n_years_burnin+1,])

#######################################################
# Run WHAM without sdreport first:
fit <- tryCatch(fit_wham(EM_input, do.sdrep=F, do.osa=F, do.retro=F, do.proj=F, MakeADFun.silent=TRUE),
                error = function(e) conditionMessage(e))

# fit$rep[grep('nll',names(fit$rep))] %>% lapply(sum) %>% unlist
# Deal with issues fitting EM to non-matching OM data
# empty elements below can be used to summarize convergence information
if(!'err' %in% names(fit) & class(fit) != "character"){
  res$model$optimized <- TRUE
  res$fit <- fit[c("wham_version", "TMB_version", "opt", "final_gradient", "runtime", "rep")]
  empars <- data.frame(par=names(res$fit$opt$par), value=res$fit$opt$par)%>%
    dplyr::filter(!grepl(x=par,'F_devs|log_NAA'))
  empars$par2 <- sapply(unique(empars$par), function(x) {
    y <- which(empars$par==x)
    if(length(y)==1) return(x)
    x <- paste(x, 1:length(y), sep='_')
    return(x)
  }) %>% unlist
  res$empars <- empars
}

# Save EM results:
rds.fn = file.path(out_dir, paste0("scenario", scenj), paste0("sim", simi, ".RDS"))
saveRDS(res, file = rds.fn)
cat(paste0("END Scenario: ", scenj, " Sim: ", simi, "\n"))
