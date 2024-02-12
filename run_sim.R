# Make sure you update the WHAM package:
# remotes::install_github(repo = 'GiancarloMCorrea/wham', ref='growth')

# Clear workspace
rm(list=ls())

# Load required libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(wham)
require(doParallel)
require(foreach)

# Define WD:
# IMPORTANT: change also this in sim_core2.R
main_dir = 'C:/Users/moroncog/Documents/GitHub/AKWHAM_sim' # local folder
out_dir = 'C:/Users/moroncog/Documents/AKWHAM_sim-simulations' # folder where all simulations will be saved. preferably out of GitHub folder

# Set working directory:
setwd(main_dir)

# Create Scenario and seeds DF (only do it once):
# source(file.path("code", "config_scenarios.R"))
# Make OM and EM WHAM inputs
# source(file.path("code", "om_setup.R"))
# source(file.path("code", "em_setup.R"))
# Clear workspace
# rm(list=ls())
# Read main dir again
# main_dir = getwd()

# Read objects to be used in sim_core2.R
df.scenario = readRDS(file.path("inputs", "df.scenarios.RDS"))

# Create folder to save sample data:
dir.create('sample_data')
dir.create(file.path('sample_data', 'om_sample'))
dir.create(file.path('sample_data', 'LAA_sample'))

# Create folder to save results:
for(k in 1:nrow(df.scenario)) {
	write.dir <- file.path(out_dir, paste0("scenario", k))
	dir.create(write.dir, recursive = T, showWarnings = FALSE)
}

# -------------------------------------------------------------
# Function to run sim:
# WARNING: before running this change main_dir in sim_core2.R
# TODO: use here::here()
run_iter <- function(sim, scen){
  cmd <- paste("Rscript --vanilla code/sim_core2.R", sim, scen)
  system(cmd)
}

# -------------------------------------------------------------------------
# Run in parallel several simulations for all scenarios
# these_scenarios = c(1:16, seq(from = 17, by = 4, length.out = 10), 57:72, seq(from = 73, by = 4, length.out = 10),
#                    113:128, seq(from = 129, by = 4, length.out = 10), 169:184, seq(from = 185, by = 4, length.out = 10))
these_scenarios = 129:224
snowfall::sfInit(parallel=TRUE, cpus=10) # modify this
snowfall::sfExportAll()
for(sc in these_scenarios){
    snowfall::sfExportAll()
    trash <- snowfall::sfLapply(1:125, function(sim) run_iter(sim, sc))
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
