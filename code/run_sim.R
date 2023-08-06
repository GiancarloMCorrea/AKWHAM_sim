
# Clear workspace
rm(list=ls())

# Load required libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(wham)
library(snowfall)

# Set working directory:
setwd("~/GitHub/AKWHAM_sim")

# Create Scenario DF:
source(file.path("code", "config_scenarios.R"))

## Make OM and EM WHAM inputs
source(file.path("code", "om_setup.R"))
source(file.path("code", "em_setup.R"))

# Clear workspace
rm(list=ls())

# Read OM and EM data frames
df.scenario = readRDS(file.path("inputs", "df.scenarios.RDS"))

# Function to run sim:
# WARNING: before running this change main_dir in sim_core.R
# TODO: use here::here()
run_iter <- function(sim, scen){
  cmd <- paste("Rscript --vanilla code/sim_core.R", sim, scen)
  system(cmd)
}

# Only run 1 replicate for 1 OM and 1 EM:
# run_iter(1, 1)

# Run in parallel several simulations only for one OM and one EM
# sfInit(parallel=TRUE, cpus=10)
# sfExportAll()
# trash <- sfLapply(1:50, function(sim) run_iter(sim, 1))
# sfStop()

# Run in parallel several simulations for all EM and OM
sfInit(parallel=TRUE, cpus=10)
sfExportAll()
for(sc in 1:nrow(df.scenario)){
    sfExportAll()
    trash <- sfLapply(1:20, function(sim) run_iter(sim, sc))
}
sfStop()
