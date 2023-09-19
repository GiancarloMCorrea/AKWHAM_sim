# Make sure you update the WHAM package:
# remotes::install_github(repo = 'GiancarloMCorrea/wham', ref='growth', INSTALL_opts = c("--no-docs", "--no-multiarch", "--no-demo"))

# Clear workspace
rm(list=ls())

# Load required libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(wham)
library(snowfall)
require(doSNOW)
require(parallel)

# Set working directory:
main_dir = "~/GitHub/AKWHAM_sim"
setwd(main_dir)

# Create Scenario DF:
source(file.path("code", "config_scenarios.R"))

# Make OM and EM WHAM inputs
source(file.path("code", "om_setup.R"))
source(file.path("code", "em_setup.R"))

# Clear workspace
rm(list=ls())

# Read main dir again
main_dir = getwd()

# Read OM and EM data frames
df.scenario = readRDS(file.path("inputs", "df.scenarios.RDS"))

# Create folder to save results:
for(k in 1:nrow(df.scenario)) {
	write.dir <- file.path(main_dir, "results", paste0("scenario", k))
	dir.create(write.dir, recursive = T, showWarnings = FALSE)
}

# -------------------------------------------------------------
# Function to run sim:
# WARNING: before running this change main_dir in sim_core.R
# TODO: use here::here()
# run_iter <- function(sim, scen){
#   cmd <- paste("Rscript --vanilla code/sim_core.R", sim, scen)
#   system(cmd)
# }

# -------------------------------------------------------------
# Run Simulation

# Only run 1 replicate for 1 scenarios:
# run_iter(1, 1)

# Run in parallel several simulations only for one scenarios
# sfInit(parallel=TRUE, cpus=10)
# sfExportAll()
# trash <- sfLapply(1:50, function(sim) run_iter(sim, 1))
# sfStop()

# Run in parallel several simulations for all scenarios
# sfInit(parallel=TRUE, cpus=10) # modify this
# sfExportAll()
# for(sc in 1:nrow(df.scenario)){
#     sfExportAll()
#     trash <- sfLapply(1:120, function(sim) run_iter(sim, sc))
# }
# sfStop()


# Run in parallel ---------------------------------------------------------

# Specify scenarios and replicates to be run:
scenj = 41:80
simi = 1:120

# Combine in DF:
iter_df = tidyr::crossing(scenj, simi)
nSim = nrow(iter_df)

# Specify number of cores:
nCores = 18
cl = makeCluster(nCores)
registerDoSNOW(cl)

# Run in parallel:
foreach(ix = 1:nSim) %dopar% {
  source(file.path('code', 'sim_core.R'))
  sim_core(iter_df = iter_df[ix,])
}

# Stop cluster:
stopCluster(cl)