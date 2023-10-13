# Code to summarise results

library(ggplot2)
library(dplyr)
library(tidyr)
theme_set(theme_bw())

# Clean workspace
rm(list = ls())

# Aux function ------------------------------------------------------------
get_maxgrad <- function(fit){
  if(!fit$model$optimized){
    return(NULL)
  }
  maxgrad <- max(abs(fit$fit$final_gradient))
  return(maxgrad)
}
# -------------------------------------------------------------------------


# Read RDS files and save them:
scenario_names = list.files(path = 'results')
ts_results = list()
par_results = list()
laa_results = list()
waa_results = list()
sel_results = list()
countList = 1
for(k in seq_along(scenario_names)) {
  
  replicates = list.files(path = file.path('results', scenario_names[k]))
  # Only run loop for scenarios with RDS
  if(length(replicates) > 0) { 
    
    for(j in seq_along(replicates)) { # loop over replicates
      rep_i = readRDS(file = file.path('results', scenario_names[k], replicates[j]))
      
	    ts_df = NULL
      par_df = NULL
      laa_df = NULL
      waa_df = NULL
      sel_df = NULL
      if(rep_i$model$optimized) {
        nyears = rep_i$truth$n_years_model
        nages = rep_i$truth$n_ages
        # TIME SERIES -------------------------------
        ssb <- data.frame(par = 'SSB',
                          year = 1:nyears,
                          est = rep_i$fit$rep$SSB,
                          truth = rep_i$truth$SSB)
        recruits <- data.frame(par = 'Rec',
                               year = 1:nyears,
                               est = rep_i$fit$rep$NAA[,1],
                               truth = rep_i$truth$NAA[,1])
        f <- data.frame(par = 'F',
                        year = 1:nyears,
                        est = rep_i$fit$rep$Fbar,
                        truth = rep_i$truth$Fbar)
        ts_df <- bind_rows(ssb, f, recruits) %>% bind_cols(rep_i$model) %>%
                          mutate(rel_error = (est-truth)/truth, abs_error = est-truth,
                          sim = as.factor(im), maxgrad = get_maxgrad(rep_i))
        # PARAMETERS ----------------------------------
        # 1) Main parameters:
        pars <- merge(rep_i$ompars, rep_i$empars, by='par2') %>%
                        filter(grepl(x=par.y, "mean_rec_pars|logit_q|log_F1|log_N1_pars"))
        # Exp() parameters in log-scale
        pars = pars %>% mutate(value.x = if_else(grepl(x = par.x, "mean_rec_pars|log_F1|log_N1_pars"), 
                                                 exp(value.x), value.x),
                               value.y = if_else(grepl(x = par.y, "mean_rec_pars|log_F1|log_N1_pars"), 
                                                 exp(value.y), value.y))
        # Now for Q:
        pars = pars %>% mutate(value.x = if_else(grepl(x = par.x, "logit_q"), 
                                                 10*exp(value.x)/(1+exp(value.x)), value.x),
                               value.y = if_else(grepl(x = par.y, "logit_q"), 
                                                 10*exp(value.y)/(1+exp(value.y)), value.y))
        pars = pars %>% select('par2', 'value.y', 'value.x') %>% rename('par' = 'par2', 'est' = 'value.y', 'truth' = 'value.x')
        # Growth parameters:
        grw1 <- data.frame(par = c('k', 'Linf', 'L1'),
                         est = exp(rep_i$fit$rep$growth_a[1:3,1]),
                         truth = exp(rep_i$truth$growth_a[1:3,1]))
        grw2 <- data.frame(par = c('SD1', 'SDA'),
                         est = rep_i$fit$rep$SD_len,
                         truth = rep_i$truth$SD_len)
        # Ecov parameters:
        ecov1 <- data.frame(par = c('meanEcov', 'sigma', 'rho'),
                            est = rep_i$fit$rep$Ecov_process_pars[,1],
                            truth = rep_i$truth$Ecov_process_pars[,1])
        # Exp sigma parameter:
        ecov1 = ecov1 %>% mutate(est = if_else(grepl(x = par, "sigma"),  exp(est), est),
                                 truth = if_else(grepl(x = par, "sigma"), exp(truth), truth))
        ecov2 <- data.frame(par = c('EcovBeta'),
                           est = rep_i$fit$rep$Ecov_beta[4,1,1,1],
                           truth = rep_i$truth$Ecov_beta[4,1,1,1])
        # Merge all parameters:
        par_df <- bind_rows(pars, grw1, grw2, ecov1, ecov2) %>% bind_cols(rep_i$model) %>%
                      mutate(rel_error = (est-truth)/truth, abs_error = est-truth,
                              sim = as.factor(im),  maxgrad = get_maxgrad(rep_i))
        # LAA TIME SERIES
        laa <- list()
        for(year in 1:nyears){
          laa[[year]] <- data.frame(par='LAA', age=1:nages, year=year,
                                    est=rep_i$fit$rep$LAA[year,],
                                    truth=rep_i$truth$LAA[year,]) %>%
            bind_cols(rep_i$model)
        }
        laa_df <- laa %>% bind_rows() %>%
          mutate(rel_error=(est-truth)/truth, abs_error=est-truth,
                 sim=as.factor(im),  maxgrad=get_maxgrad(rep_i))
        
        # WAA TIME SERIES
        waa <- list()
        waapos = 2 # Only survey waa
        for(year in 1:nyears) { 
          waa[[year]] <- data.frame(par='WAA', age=1:nages, year=year,
                                    est=rep_i$fit$rep$pred_waa[waapos,year,],
                                    truth=rep_i$truth$pred_waa[waapos,year,]) %>%
            bind_cols(rep_i$model)
        }
        waa_df <- waa %>% bind_rows() %>%
          mutate(rel_error=(est-truth)/truth, abs_error=est-truth,
                 sim=as.factor(im),  maxgrad=get_maxgrad(rep_i))
        
        # Selectivity values (only when OM and EM uses size-based selex)
        my_lens = rep_i$truth$lengths
        selFish = data.frame(par='selLL', len = my_lens, type = 'fishery',
                              est=rep_i$fit$rep$selLL[[1]][1,],
                              truth=rep_i$truth$selLL[[1]][1,])
        selSurv = data.frame(par='selLL', len = my_lens, type = 'survey',
                             est=rep_i$fit$rep$selLL[[2]][1,],
                             truth=rep_i$truth$selLL[[2]][1,])
        sel = rbind(selFish, selSurv) %>% bind_cols(rep_i$model)
        sel_df = sel %>% bind_rows() %>%
                  mutate(rel_error=(est-truth)/truth, abs_error=est-truth,
                         sim=as.factor(im),  maxgrad=get_maxgrad(rep_i))
        
      } # conditional if optimized
      
      ts_results[[countList]] = ts_df
      par_results[[countList]] = par_df
      laa_results[[countList]] = laa_df
      waa_results[[countList]] = waa_df
      sel_results[[countList]] = sel_df
      countList = countList + 1
      
    } # rep loop
    
    cat(scenario_names[k], ' done', "\n")
    
  } # conditional if rds present

} # scenarios loop

ts_results = dplyr::bind_rows(ts_results)
par_results = dplyr::bind_rows(par_results)
laa_results = dplyr::bind_rows(laa_results)
waa_results = dplyr::bind_rows(waa_results)
sel_results = dplyr::bind_rows(sel_results)

# Save results
saveRDS(ts_results, 'results/ts_results.RDS')
saveRDS(par_results, 'results/par_results.RDS')
saveRDS(laa_results, 'results/laa_results.RDS')
saveRDS(waa_results, 'results/waa_results.RDS')
saveRDS(sel_results, 'results/sel_results.RDS')
