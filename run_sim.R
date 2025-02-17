# Make sure you update the WHAM package:
# remotes::install_github(repo = 'GiancarloMCorrea/wham', ref='tuna')

# Clear workspace
rm(list=ls())

# Load required libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(wham)
require(doParallel)
require(foreach)
library(here)
create_inputs = FALSE # want to create OM EM inputs? (just do it once)

if(create_inputs) {
  # Create Scenario and seeds DF (only do it once):
  source(here::here("code", "config_scenarios.R"))
  # Make OM and EM WHAM inputs
  source(here::here("code", "om_setup.R"))
  source(here::here("code", "em_setup.R"))
  source(here::here("code", "order_env_data.R"))
}
  
# -------------------------------------------------------------------------
# Clear workspace
rm(list=ls())
# Read main dir again
out_dir = here::here('results') # folder where all simulations will be saved. preferably out of GitHub folder
main_dir = getwd()

# Read objects to be used in sim_core2.R
df.scenario = readRDS(here::here("inputs", "df.scenarios.RDS"))

# Create folder to save sample data:
dir.create('sample_data', showWarnings = FALSE)
dir.create(here::here('sample_data', 'om_sample'), showWarnings = FALSE)
dir.create(here::here('sample_data', 'LAA_sample'), showWarnings = FALSE)
dir.create(here::here('sample_data', 'LAApar_sample'), showWarnings = FALSE)

# Create folder to save results:
for(k in 1:nrow(df.scenario)) {
	write.dir <- file.path(out_dir, paste0("scenario", k))
	dir.create(write.dir, recursive = T, showWarnings = FALSE)
}

# -------------------------------------------------------------
# Function to run sim:
run_iter <- function(sim, scen){
  cmd <- paste("Rscript --vanilla code/sim_core2.R", sim, scen)
  system(cmd)
}

# -------------------------------------------------------------------------
# Run in parallel several simulations for all scenarios
these_scenarios = c(1:nrow(df.scenario))
snowfall::sfInit(parallel=TRUE, cpus=10) # modify this
snowfall::sfExportAll()
for(sc in these_scenarios){
    snowfall::sfExportAll()
    trash <- snowfall::sfLapply(1:20, function(sim) run_iter(sim, sc))
}
snowfall::sfStop()


# Run in parallel ---------------------------------------------------------

# Specify scenarios and replicates to be run:
# scenj = 75:80
# simi = 1:120
#
# # Combine in DF:
# iter_df = tidyr::crossing(scenj, simi)
# nSim = nrow(iter_df)
#
# # Specify number of cores:
# nCores = 15
# cl = parallel::makeCluster(nCores)
# doParallel::registerDoParallel(cl)
#
# # Run in parallel:
# foreach::foreach(ix = 1:nSim) %dopar% {
#   source(file.path('code', 'sim_core.R'))
#   sim_core(iter_df = iter_df[ix,])
# }
#
# # Stop cluster:
# parallel::stopCluster(cl)
