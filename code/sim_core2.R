#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
simi = as.integer(args[1])
scenj = as.integer(args[2])

# CHANGE THIS IF REQUIRED!
# Main directory:
main_dir = 'C:/USE/GitHub/AKWHAM_sim'

# Load required libraries:
library(wham)
source(file.path(main_dir, "code", "make_om_plots.R"))
source(file.path(main_dir, "code", "config_params.R"))
# Read inputs:
om_inputs <- readRDS(file.path(main_dir, "inputs", "om_inputs.RDS"))
em_inputs <- readRDS(file.path(main_dir, "inputs", "em_inputs.RDS"))
df.scenario <- readRDS(file.path(main_dir, "inputs", "df.scenarios.RDS"))

# Make data.frame summarizing scenario:
this_scenario <- data.frame(df.scenario[scenj, ])
model <- cbind(im = simi, scenario = scenj, optimized=FALSE, sdreport=FALSE, this_scenario)
# Select observations to pass to EM from sim_data:
# DO NOT PASS either 'use_xxx_xxx' or Neff (except for caal, see below)
obs_names <- c("agg_catch", "agg_indices", "catch_paa", "index_paa", "catch_pal", "index_pal", 
               'catch_caal', 'index_caal', 'waa', 'waa_cv', 'Ecov_obs', "obsvec", "agesvec")

#######################################################
# Read seed:
# I don't think we want to use the same (e.g. 1000) seeds for everything.
seeds <- readRDS(file.path(main_dir, "inputs","seeds.RDS"))
# Print scenario name:
cat(paste0("START Scenario: ", scenj, " Sim: ", simi, "\n"))

# -------------------------------------------------------------------------
# Simulate environmental time series:

if(df.scenario$Ecov_sim[scenj] == 'stationary') {
  
  set.seed(seeds[simi])
  ecov_error = rnorm(length(years_base), mean = 0, sd = exp(Ecov_re_sig))
  alpha = 1
  beta = Ecov_trend[1] # trend
  theta = -1 + 2/(1 + exp(-Ecov_re_cor)) # as in WHAM
  sim_ecov = 0
  for(i in 2:length(ecov_error)) sim_ecov[i] = alpha+beta*i+theta*sim_ecov[i-1] + ecov_error[i]
  sim_ecov = scale(sim_ecov)

}

if(df.scenario$Ecov_sim[scenj] == 'trend') {
  
  set.seed(seeds[simi])
  ecov_error = rnorm(length(years_base), mean = 0, sd = exp(Ecov_re_sig))
  alpha = 1
  beta = Ecov_trend[2] # trend
  theta = -1 + 2/(1 + exp(-Ecov_re_cor)) # as in WHAM
  sim_ecov = 0
  for(i in 2:length(ecov_error)) sim_ecov[i] = alpha+beta*i+theta*sim_ecov[i-1] + ecov_error[i]
  sim_ecov = scale(sim_ecov)
  
}

# Now replace the sim_ecov in the OM input:
om_inputs[[scenj]]$par$Ecov_re = sim_ecov

#######################################################
# Run OM:
om <- fit_wham(om_inputs[[scenj]], do.fit = FALSE, MakeADFun.silent = TRUE)
# Define seed and simulate WHAM data:
set.seed(seeds[simi])
sim_data <- om$simulate(complete=TRUE)
if(simi == 1) make_plot_om(sim_data, scenj, main_dir) # Make plot 
if(simi == 1 & scenj <= 4) saveRDS(object = om, file = paste0('inputs/om_sample_', scenj,'.RDS')) # Save OM data to make plots later

# Read EM input data:
EM_input <- em_inputs[[scenj]] 

# CAAL sampling: ----------------------------------------------
# Only works for multinomial 

#  Fishery:
if(df.scenario$catch_data[scenj] == 'caal' | df.scenario$catch_data[scenj] == 'paa') {
  
  if(df.scenario$data_scen[scenj] == 'poor') Nsamp_CAAL = 125 # Nsamp size for CAAL
  if(df.scenario$data_scen[scenj] == 'rich') Nsamp_CAAL = 500 # Nsamp size for CAAL
  
  # Also pass caal_Neff
  if(df.scenario$catch_data[scenj] == 'caal') obs_names = c(obs_names, 'catch_caal_Neff', 'use_catch_caal')
  
  # Order to sort: year, fleet, len bin, age
  to_obsvec = NULL
  for(j in 1:sim_data$n_years_model) {
    for(i in 1:sim_data$n_fleets) {
      if(df.scenario$caal_samp[scenj] == 'random') {
        # Random sampling:
        len_subsam = rmultinom(n = 1, size = Nsamp_CAAL, prob = sim_data$catch_pal[i,j,]) 
        len_subsam = as.vector(len_subsam)
      }
      if(df.scenario$caal_samp[scenj] == 'strat') {
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
        sim_data$catch_caal_Neff[j,i,k] = len_subsam[k]*0.1 # multiplied by 0.1 to mimic GOA pcod
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
      tmp_alk = sim_data$catch_caal[i,j,,] * sim_data$catch_caal_Neff[j,i,]*10
      out_alk = out_alk + tmp_alk
    }
    avg_alk[[i]] = t(apply(t(out_alk), 2, function(i) i/sum(i)))
    avg_alk[[i]][is.nan(avg_alk[[i]])] = 0 # replace nan with zeros
  }
  # now calculate age comps:
  to_obsvec_paa = NULL
  for(j in 1:sim_data$n_years_model) {
    for(i in 1:sim_data$n_fleets) {
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
      sim_data$catch_paa[i,j,] = tmp_paa
      to_obsvec_paa = c(to_obsvec_paa, tmp_paa*EM_input$data$catch_Neff[j,i]) # Neff of EM
    } # fleet loop
  } # year loop
  
  # Now replace values PAA in obsvec vector:
  sim_data$obsvec[sim_data$obs$type == 'catchpaa'] = to_obsvec_paa
  
  # Simulate observed/empirical mean weight-at-age:
  if(df.scenario$method[scenj] == 'WAA' | df.scenario$method[scenj] == 'EWAA') {
    to_obsvec = NULL
    for(j in 1:sim_data$n_years_model) {
      for(i in 1:sim_data$n_fleets) {
        caal_obs = (sim_data$catch_caal_Neff[j,i,]*10) * sim_data$catch_caal[i,j,,] # multiply by 10 to come back to real Neff
        for(a in 1:sim_data$n_ages) {
          ind_wt = rep(x = sim_data$watl[j,], times = caal_obs[,a])
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
            cv_wt = sd(ind_wt)/mean_wt # when there is 2+ observations
          }
          sim_data$waa[sim_data$waa_pointer_fleets[i],j,a] = mean_wt
          sim_data$waa_cv[sim_data$waa_pointer_fleets[i],j,a] = cv_wt
        } # ages loop
      } # fleet loop
    } # year loop
    
    # replace NA (waa obs) with average value:
    for(i in 1:sim_data$n_fleets) {
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
      to_obsvec = c(to_obsvec, as.vector(t(sim_data$waa[sim_data$waa_pointer_fleets[i],,])))
    }
    
    # Now replace values in obsvec WAA vector:
    sim_data$obsvec[sim_data$obs$type == 'catchwaa'] = to_obsvec
  } # waa conditional
  
} # catch_caal conditional


#  Survey:
if(df.scenario$index_data[scenj] == 'caal' | df.scenario$index_data[scenj] == 'paa') {
  
  if(df.scenario$data_scen[scenj] == 'poor') Nsamp_CAAL = 250 # Nsamp size for CAAL
  if(df.scenario$data_scen[scenj] == 'rich') Nsamp_CAAL = 1000 # Nsamp size for CAAL
  
  # Also pass caal_Neff
  if(df.scenario$index_data[scenj] == 'caal') obs_names = c(obs_names, 'index_caal_Neff', 'use_index_caal')
  
  # Order to sort: year, fleet, len bin, age
  to_obsvec = NULL
  to_obsvec_paa = NULL
  for(j in 1:sim_data$n_years_model) {
    for(i in 1:sim_data$n_indices) {
      if(df.scenario$caal_samp[scenj] == 'random') {
        # Random sampling:
        len_subsam = rmultinom(n = 1, size = Nsamp_CAAL, prob = sim_data$index_pal[i,j,]) 
        len_subsam = as.vector(len_subsam)
      }
      if(df.scenario$caal_samp[scenj] == 'strat') {
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
        sim_data$index_caal_Neff[j,i,k] = len_subsam[k]*0.1 # multiplied by 0.1 to mimic GOA pcod
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
      tmp_alk = sim_data$index_caal[i,j,,] * sim_data$index_caal_Neff[j,i,]*10
      out_alk = out_alk + tmp_alk
    }
    avg_alk[[i]] = t(apply(t(out_alk), 2, function(i) i/sum(i)))
    avg_alk[[i]][is.nan(avg_alk[[i]])] = 0 # replace nan with zeros
  }
  # now calculate age comps:
  to_obsvec_paa = NULL
  for(j in 1:sim_data$n_years_model) {
    for(i in 1:sim_data$n_indices) {
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
      sim_data$index_paa[i,j,] = tmp_paa
      to_obsvec_paa = c(to_obsvec_paa, tmp_paa*EM_input$data$index_Neff[j,i]) # Neff of EM
    } # fleet loop
  } # year loop
  
  # Now replace values PAA in obsvec vector:
  sim_data$obsvec[sim_data$obs$type == 'indexpaa'] = to_obsvec_paa
  
  
  # Simulate observed/empirical mean weight-at-age:
  if(df.scenario$method[scenj] == 'WAA' | df.scenario$method[scenj] == 'EWAA') {
    to_obsvec = NULL
    for(j in 1:sim_data$n_years_model) {
      for(i in 1:sim_data$n_indices) {
        caal_obs = (sim_data$index_caal_Neff[j,i,]*10) * sim_data$index_caal[i,j,,] # multiply by 10 to come back to real Neff
        for(a in 1:sim_data$n_ages) {
          ind_wt = rep(x = sim_data$watl[j,], times = caal_obs[,a])
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
            cv_wt = sd(ind_wt)/mean_wt # when there is 2+ observations
          }
          sim_data$waa[sim_data$waa_pointer_indices[i],j,a] = mean_wt
          sim_data$waa_cv[sim_data$waa_pointer_indices[i],j,a] = cv_wt
        } # ages loop
      } # fleet loop
    } # year loop
    
    # replace NA (waa obs) with average value:
    for(i in 1:sim_data$n_indices) {
      for(a in 1:sim_data$n_ages) {
        avg_waa = mean(sim_data$waa[sim_data$waa_pointer_indices[i],,a], na.rm = TRUE)
        these_na = which(is.na(sim_data$waa[sim_data$waa_pointer_indices[i],,a]))
        sim_data$waa[sim_data$waa_pointer_indices[i],these_na,a] = avg_waa
        if(is.na(avg_waa)) { # this only useful for EWAA. maybe not realistic but whatever. also, this will probably never be used.
          this_mean = log(sim_data$pred_waa[sim_data$waa_pointer_indices[i],these_na,a])
          this_sd = sqrt(log(0.2^2 + 1.0)) # CV = 0.2
          this_mean = this_mean - (this_sd^2)*0.5 # corrected mean
          sim_data$waa[sim_data$waa_pointer_indices[i],these_na,a] = exp(mapply(FUN = rnorm, n = 1, mean = this_mean, sd = this_sd))
        }
      }
      to_obsvec = c(to_obsvec, as.vector(t(sim_data$waa[sim_data$waa_pointer_indices[i],,])))
    }
    
    # Now replace values in obsvec WAA vector:
    sim_data$obsvec[sim_data$obs$type == 'indexwaa'] = to_obsvec
  } # waa conditional
    
} # index_caal conditional

# -------------------------------------------------------------------------

# Continue code:
truth <- sim_data
# Save the version for reproducibility
truth$wham_version = om$wham_version
# Put simulated data into EM input:
# it is important to pass keep names since 'obsvec' is being passed and OM simulates data for all categories:
keep_names = names(sim_data)[grep(pattern = 'keep', x = names(sim_data))] 
# Pass names:
EM_input$data[c(obs_names, keep_names)] = sim_data[c(obs_names, keep_names)]
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
if(df.scenario$method[scenj] == 'WAA') {
  EM_input$par$WAA_a = log(colMeans(sim_data$waa[2,,]))
}
if(df.scenario$method[scenj] == 'LAA') {
  EM_input$par$LAA_a = log(colMeans(sim_data$LAA))
}

#######################################################
# Run WHAM without sdreport first:
fit <- tryCatch(fit_wham(EM_input, do.sdrep=F, do.osa=F, do.retro=F, do.proj=F, MakeADFun.silent=TRUE),
                error = function(e) conditionMessage(e))

# fit$rep[grep('nll',names(fit$rep))] %>% lapply(sum) %>% unlist
# Deal with issues fitting EM to non-matching OM data
# empty elements below can be used to summarize convergence information
if(!'err' %in% names(fit) & class(fit) != "character"){
  res$model$optimized <- TRUE
  res$fit <- fit[c("wham_version", "TMB_version", "opt", "final_gradient", "rep")]
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
rds.fn = file.path(main_dir, "results", paste0("scenario", scenj), paste0("sim", simi, ".RDS"))
saveRDS(res, file = rds.fn)
cat(paste0("END Scenario: ", scenj, " Sim: ", simi, "\n"))