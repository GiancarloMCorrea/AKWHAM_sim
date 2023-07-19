library(snowfall)

# Clear workspace
rm(list=ls())

# Set working directory:
setwd("C:/Use/GitHub/AKWHAM_sim")

# Make OM figures? 
make_OM_figures = TRUE

## Make OM and EM inputs
source(file.path("code", "om_setup.R"))
source(file.path("code", "em_setup.R"))

# Clear workspace
rm(list=ls())

# Read OM and EM data frames
df.ems = readRDS(file.path("inputs", "df.ems.RDS"))
df.oms = readRDS(file.path("inputs", "df.oms.RDS"))

run_iter <- function(sim, om, em){
  cmd <- paste("Rscript --vanilla sim_core.R", sim, om, em)
  system(cmd)
}

#sfInit(parallel=TRUE, cpus=6)
#sfExportAll()
run_iter(sim = 1, om = 1, em = 1)
trash <- sfLapply(1:30, function(sim) run_iter(sim,2,1))

# for(om in 1:nrow(df.oms)){
#   for(em in 1:nrow(df.ems)){
#     sfExportAll()
#     trash <- sfLapply(1:30, function(sim) run_iter(sim,om,em))
#   }
# }
# 
# sfStop()
