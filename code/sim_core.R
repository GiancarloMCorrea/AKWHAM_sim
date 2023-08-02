#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
simi = as.integer(args[1])
omj = as.integer(args[2])
emk = as.integer(args[3])

# Main directory:
main_dir = 'C:/Users/moroncog/Documents/GitHub/AKWHAM_sim'

# Load required libraries:
library(wham)
source(file.path(main_dir, "code", "make_om_plots.R"))
# Read inputs:
om_inputs <- readRDS(file.path(main_dir, "inputs", "om_inputs.RDS"))
em_inputs <- readRDS(file.path(main_dir, "inputs", "em_inputs.RDS"))
df.ems <- readRDS(file.path(main_dir, "inputs", "df.ems.RDS"))
df.oms <- readRDS(file.path(main_dir, "inputs", "df.oms.RDS"))
# Make data.frame summarizing scenario:
x <- data.frame(df.ems[emk,])
names(x) <- paste0('em_',names(x))
y <- data.frame(df.oms[omj,])
names(y) <- paste0('om_',names(y))
model <- cbind(im=simi, om=omj, em=emk, optimized=FALSE, sdreport=FALSE, y,x)
# Select observations to pass to EM from sim_data:
# DO NOT PASS 'use_xxx_xxx'
obs_names <- c("agg_catch", "agg_indices", "catch_paa", "index_paa", "catch_pal", "index_pal", 
               'catch_caal', 'index_caal', 'waa', "obsvec", "agesvec")
# TODO: add Ecov information when ready

#######################################################
# Read seed:
# I don't think we want to use the same (e.g. 1000) seeds for everything.
seeds <- readRDS(file.path(main_dir, "inputs","seeds.RDS"))
# Print scenario name:
cat(paste0("START OM: ", omj, " Sim: ", simi, " EM: ", emk, "\n"))
# Create folder to save results:
write.dir <- file.path(main_dir, "results", paste0("om", omj))
dir.create(write.dir, recursive = T, showWarnings = FALSE)

#######################################################
# Run OM:
om <- fit_wham(om_inputs[[omj]], do.fit = FALSE, MakeADFun.silent = TRUE)
# Define seed:
set.seed(seeds[[omj]][simi])
# Simulate data:
sim_data <- om$simulate(complete=TRUE)
if(simi == 1) make_plot_om(sim_data, omj, main_dir) # Make plot 
truth <- sim_data
# Save the version for reproducibility
truth$wham_version = om$wham_version
# Read EM input data:
EM_input <- em_inputs[[emk]] 
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
rds.fn = file.path(main_dir, "results", paste0("om", omj), paste0("sim", simi, "_em", emk, ".RDS"))
saveRDS(res, file = rds.fn)
cat(paste0("END OM: ", omj, " Sim: ", simi, " EM: ", emk, "\n"))