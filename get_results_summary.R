# Code to summarise results
library(ggplot2)
library(dplyr)
library(tidyr)
theme_set(theme_bw())

# Clean workspace
rm(list = ls())

# Aux function ------------------------------------------------------------
source('aux_functions.R')
source(file.path('code', 'config_params.R'))

# Folder where simulations are saved:
out_dir = 'results'

# Some important parameters:
waapos = 2 # Only survey waa
output_folder = 'outputs'
dir.create(output_folder)

# Create objects
scenario_names = list.files(path = out_dir)
ts_results = list()
par_results = list()
waa_results = list()
catch_paa_results = list()
index_paa_results = list()
sel_results = list()
waare_results = list()
countList = 1
for(k in seq_along(scenario_names)) {
  
  replicates = list.files(path = file.path(out_dir, scenario_names[k]))
  # Only run loop for scenarios with RDS
  if(length(replicates) > 0) { 
    
    for(j in seq_along(replicates)) { # loop over replicates
      rep_i = readRDS(file = file.path(out_dir, scenario_names[k], replicates[j]))
      rep_i$model = rep_i$model[, !duplicated(colnames(rep_i$model))] # remove duplicated scenario
      
	    ts_df = NULL
      par_df = NULL
      waa_df = NULL
      catch_paa_df = NULL
      catch_paa_df = NULL
      sel_df = NULL
      waare_df = NULL
      if(rep_i$model$optimized) {
        nyears = rep_i$truth$n_years_model - n_years_burnin # Only main years (exclude burnin)
        nages = rep_i$truth$n_ages
        # TIME SERIES -------------------------------
        ssb <- data.frame(par = 'SSB',
                          year = 1:nyears,
                          est = rep_i$fit$rep$SSB,
                          truth = tail(rep_i$truth$SSB, n = n_years_base))
        recruits <- data.frame(par = 'Rec',
                               year = 1:nyears,
                               est = rep_i$fit$rep$NAA[,1],
                               truth = rep_i$truth$NAA[(n_years_burnin+1):(n_years_base+n_years_burnin),1])
        f <- data.frame(par = 'F',
                        year = 1:nyears,
                        est = rep_i$fit$rep$Fbar,
                        truth = tail(rep_i$truth$Fbar, n = n_years_base))
        ts_df <- bind_rows(ssb, f, recruits) %>% bind_cols(rep_i$model) %>%
                          mutate(rel_error = (est-truth)/truth, abs_error = est-truth,
                          sim = as.factor(im), maxgrad = get_maxgrad(rep_i))
        # PARAMETERS ----------------------------------
        # 1) Main parameters:
        pars <- merge(rep_i$ompars, rep_i$empars, by='par2') %>%
                        filter(grepl(x=par.y, "mean_rec_pars|logit_q"))
        # Exp() parameters in log-scale
        pars = pars %>% mutate(value.x = if_else(grepl(x = par.x, "mean_rec_pars"), 
                                                 exp(value.x), value.x),
                               value.y = if_else(grepl(x = par.y, "mean_rec_pars"), 
                                                 exp(value.y), value.y))
        # Now for Q:
        pars = pars %>% dplyr::mutate(value.x = if_else(grepl(x = par.x, "logit_q"), 
                                                 10*exp(value.x)/(1+exp(value.x)), value.x),
                               value.y = if_else(grepl(x = par.y, "logit_q"), 
                                                 10*exp(value.y)/(1+exp(value.y)), value.y))
        pars = pars %>% dplyr::select('par2', 'value.y', 'value.x') %>% 
          dplyr::rename('par' = 'par2', 'est' = 'value.y', 'truth' = 'value.x')
        # # Growth parameters:
        # grw1 <- data.frame(par = c('k', 'Linf', 'L1'),
        #                  est = exp(rep_i$fit$rep$growth_a[1:3,1]),
        #                  truth = exp(rep_i$truth$growth_a[1:3,1]))
        # grw2 <- data.frame(par = c('SD1', 'SDA'),
        #                  est = rep_i$fit$rep$SD_len,
        #                  truth = rep_i$truth$SD_len)
        # # Ecov parameters:
        # ecov1 <- data.frame(par = c('meanEcov', 'sigma', 'rho'),
        #                     est = rep_i$fit$rep$Ecov_process_pars[,1],
        #                     truth = rep_i$truth$Ecov_process_pars[,1])
        # # Exp sigma parameter:
        # ecov1 = ecov1 %>% mutate(est = if_else(grepl(x = par, "sigma"),  exp(est), est),
        #                          truth = if_else(grepl(x = par, "sigma"), exp(truth), truth))
        # ecov2 <- data.frame(par = c('EcovBeta'),
        #                    est = rep_i$fit$rep$Ecov_beta[4,1,1,1],
        #                    truth = rep_i$truth$Ecov_beta[4,1,1,1])
        # # Merge all parameters:
        par_df <- pars %>% bind_cols(rep_i$model) %>%
                      mutate(rel_error = (est-truth)/truth, abs_error = est-truth,
                              sim = as.factor(im),  maxgrad = get_maxgrad(rep_i))

        # WAA TIME SERIES
        waa <- list()
        for(year in 1:nyears) { 
          waa[[year]] <- data.frame(par='WAA', age=1:nages, year=year,
                                    est=rep_i$fit$rep$pred_waa[waapos,year,],
                                    truth=rep_i$truth$pred_waa[waapos,n_years_burnin+year,]) %>%
            bind_cols(rep_i$model)
        }
        waa_df <- waa %>% bind_rows() %>%
          mutate(rel_error=(est-truth)/truth, abs_error=est-truth,
                 sim=as.factor(im),  maxgrad=get_maxgrad(rep_i))

        # Pred catch paa TIME SERIES
        catch_paa <- list()
        for(year in 1:nyears) { 
          catch_paa[[year]] <- data.frame(par='pred_catch_paa', age=1:nages, year=year,
                                    est=rep_i$fit$rep$pred_catch_paa[year,1,],
                                    truth=rep_i$truth$pred_catch_paa[n_years_burnin+year,1,]) %>%
            bind_cols(rep_i$model)
        }
        catch_paa_df <- catch_paa %>% bind_rows() %>%
          mutate(rel_error=(est-truth)/truth, abs_error=est-truth,
                 sim=as.factor(im),  maxgrad=get_maxgrad(rep_i))
        
        # Pred index paa TIME SERIES
        index_paa <- list()
        for(year in 1:nyears) { 
          index_paa[[year]] <- data.frame(par='pred_index_paa', age=1:nages, year=year,
                                          est=rep_i$fit$rep$pred_index_paa[year,1,],
                                          truth=rep_i$truth$pred_index_paa[n_years_burnin+year,1,]) %>%
            bind_cols(rep_i$model)
        }
        index_paa_df <- index_paa %>% bind_rows() %>%
          mutate(rel_error=(est-truth)/truth, abs_error=est-truth,
                 sim=as.factor(im),  maxgrad=get_maxgrad(rep_i))
        
        # Selectivity values (only EM estimates)
        sel_df = data.frame(par1 = c(rep_i$fit$rep$selpars[[1]][,1], rep_i$fit$rep$selpars[[2]][,1]),
                            fleet = rep(c(1,2), each = nrow(rep_i$fit$rep$selpars[[1]])),
                            year = rep(1:nyears, times = 2)) %>% bind_cols(rep_i$model) %>%
                  mutate(sim=as.factor(im), maxgrad=get_maxgrad(rep_i))
        
        # RE effects
        waare_df = data.frame(est = as.vector(rep_i$fit$rep$WAA_repars[1,]), 
                              par = c('par1','par2','par3','par4')) %>% bind_cols(rep_i$model) %>%
                      mutate(sim=as.factor(im), maxgrad=get_maxgrad(rep_i))
        
      } # conditional if optimized 
      
      ts_results[[countList]] = ts_df
      par_results[[countList]] = par_df
      waa_results[[countList]] = waa_df
      catch_paa_results[[countList]] = catch_paa_df
      index_paa_results[[countList]] = index_paa_df
      sel_results[[countList]] = sel_df
      waare_results[[countList]] = waare_df
      countList = countList + 1
      
    } # rep loop
    
    cat(scenario_names[k], ' done', "\n")
    
  } # conditional if rds present

} # scenarios loop

ts_results = dplyr::bind_rows(ts_results)
par_results = dplyr::bind_rows(par_results)
waa_results = dplyr::bind_rows(waa_results)
catch_paa_results = dplyr::bind_rows(catch_paa_results)
index_paa_results = dplyr::bind_rows(index_paa_results)
sel_results = dplyr::bind_rows(sel_results)
waare_results = dplyr::bind_rows(waare_results)

# Save results
saveRDS(ts_results, file.path(output_folder, 'ts_results.RDS'))
saveRDS(par_results, file.path(output_folder, 'par_results.RDS'))
saveRDS(waa_results, file.path(output_folder, 'waa_results.RDS'))
saveRDS(catch_paa_results, file.path(output_folder, 'catch_paa_results.RDS'))
saveRDS(index_paa_results, file.path(output_folder, 'index_paa_results.RDS'))
saveRDS(sel_results, file.path(output_folder, 'sel_results.RDS'))
saveRDS(waare_results, file.path(output_folder, 'waare_results.RDS'))
