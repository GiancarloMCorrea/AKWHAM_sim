#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
simi = as.integer(args[1])
scenj = as.integer(args[2])

# CHANGE THIS IF REQUIRED!
# Main directory:
main_dir = 'C:/Use/GitHub/AKWHAM_sim'

# Load required libraries:
library(wham)
source(file.path(main_dir, "code", "make_om_plots.R"))
# Read inputs:
om_inputs <- readRDS(file.path(main_dir, "inputs", "om_inputs.RDS"))
em_inputs <- readRDS(file.path(main_dir, "inputs", "em_inputs.RDS"))
df.scenario <- readRDS(file.path(main_dir, "inputs", "df.scenarios.RDS"))

# Make data.frame summarizing scenario:
this_scenario <- data.frame(df.scenario[scenj, ])
model <- cbind(im = simi, scenario = scenj, optimized=FALSE, sdreport=FALSE, this_scenario)
# Select observations to pass to EM from sim_data:
# DO NOT PASS 'use_xxx_xxx' (except for caal, see below)
obs_names <- c("agg_catch", "agg_indices", "catch_paa", "index_paa", "catch_pal", "index_pal", 
               'catch_caal', 'index_caal', 'waa', 'Ecov_obs', "obsvec", "agesvec")

#######################################################
# Read seed:
# I don't think we want to use the same (e.g. 1000) seeds for everything.
seeds <- readRDS(file.path(main_dir, "inputs","seeds.RDS"))
# Print scenario name:
cat(paste0("START Scenario: ", scenj, " Sim: ", simi, "\n"))

#######################################################
# Run OM:
om <- fit_wham(om_inputs[[scenj]], do.fit = FALSE, MakeADFun.silent = TRUE)
# Define seed:
# TODO: use same seeds based on OM? 
set.seed(seeds[[scenj]][simi])
# Simulate data:
sim_data <- om$simulate(complete=TRUE)
if(simi == 1) make_plot_om(sim_data, scenj, main_dir) # Make plot 
if(simi == 1 & scenj <= 4) saveRDS(object = om, file = paste0('inputs/om_sample_', scenj,'.RDS')) # Save OM data to make plots later

# CAAL sampling: ----------------------------------------------
# Only works for multinomial 

if(df.scenario$catch_data[scenj] == 'caal') {
  
  if(df.scenario$data_scen[scenj] == 'poor') Nsamp_CAAL = 25 # Nsamp size for CAAL
  if(df.scenario$data_scen[scenj] == 'rich') Nsamp_CAAL = 100 # Nsamp size for CAAL
  
  # Also pass caal_Neff
  obs_names = c(obs_names, 'catch_caal_Neff', 'use_catch_caal')
  
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
        sim_data$catch_caal_Neff[j,i,k] = len_subsam[k]
        tmp_caal_sim = rmultinom(n = 1, size = len_subsam[k], prob = sim_data$pred_CAAL[j,i,k,])
        # Save sim sampling:
        to_obsvec = c(to_obsvec, tmp_caal_sim)
        if(sum(tmp_caal_sim) == 0) sim_data$catch_caal[i,j,k,] = 0
        else sim_data$catch_caal[i,j,k,] = tmp_caal_sim[,1]/sum(tmp_caal_sim[,1])
      } # len bin loop
      # Replace use/not use:
      sim_data$use_catch_caal[j,i,] = ifelse(test = len_subsam > 0, yes = 1, no = 0)
    } # fleet loop
  } # year loop
  # Now replace values in obsvec vector:
  sim_data$obsvec[sim_data$obs$type == 'catchcaal'] = to_obsvec
  
} # catch_caal conditional

if(df.scenario$index_data[scenj] == 'caal') {
  
  if(df.scenario$data_scen[scenj] == 'poor') Nsamp_CAAL = 25 # Nsamp size for CAAL
  if(df.scenario$data_scen[scenj] == 'rich') Nsamp_CAAL = 100 # Nsamp size for CAAL
  
  # Also pass caal_Neff
  obs_names = c(obs_names, 'index_caal_Neff', 'use_index_caal')
  
  # Order to sort: year, fleet, len bin, age
  to_obsvec = NULL
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
        sim_data$index_caal_Neff[j,i,k] = len_subsam[k]
        tmp_caal_sim = rmultinom(n = 1, size = len_subsam[k], prob = sim_data$pred_IAAL[j,i,k,])
        # Save sim sampling:
        to_obsvec = c(to_obsvec, tmp_caal_sim)
        if(sum(tmp_caal_sim) == 0) sim_data$index_caal[i,j,k,] = 0
        else sim_data$index_caal[i,j,k,] = tmp_caal_sim[,1]/sum(tmp_caal_sim[,1])
      } # len bin loop
      # Replace use/not use:
      sim_data$use_index_caal[j,i,] = ifelse(test = len_subsam > 0, yes = 1, no = 0)
    } # fleet loop
  } # year loop
  # Now replace values in obsvec vector:
  sim_data$obsvec[sim_data$obs$type == 'indexcaal'] = to_obsvec
  
} # index_caal conditional

# -------------------------------------------------------------------------

# Continue code:
truth <- sim_data
# Save the version for reproducibility
truth$wham_version = om$wham_version
# Read EM input data:
EM_input <- em_inputs[[scenj]] 
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