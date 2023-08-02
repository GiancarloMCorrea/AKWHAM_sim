library(snowfall)

# Clear workspace
rm(list=ls())

# Set working directory:
setwd("~/GitHub/AKWHAM_sim")

## Make OM and EM inputs
source(file.path("code", "om_setup.R"))
source(file.path("code", "em_setup.R"))

# Clear workspace
rm(list=ls())

# Read OM and EM data frames
df.ems = readRDS(file.path("inputs", "df.ems.RDS"))
df.oms = readRDS(file.path("inputs", "df.oms.RDS"))

# Function to run sim:
# WARNING: before running this change main_dir in sim_core.R
# TODO: use here::here()
run_iter <- function(sim, om, em){
  cmd <- paste("Rscript --vanilla code/sim_core.R", sim, om, em)
  system(cmd)
}

# Only run 1 replicate for 1 OM and 1 EM:
run_iter(1, 1, 1)

# Run in parallel several simulations only for one OM and one EM
# sfInit(parallel=TRUE, cpus=10)
# sfExportAll()
# trash <- sfLapply(1:50, function(sim) run_iter(sim, 1, 1))
# sfStop()

# Run in parallel several simulations for all EM and OM
# sfInit(parallel=TRUE, cpus=10)
# sfExportAll()
# for(om in 1:nrow(df.oms)){
#   for(em in 1:nrow(df.ems)){
#     sfExportAll()
#     trash <- sfLapply(1:50, function(sim) run_iter(sim,om,em))
#   }
# }
# sfStop()
