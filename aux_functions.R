# Auxiliary function for plotting
# Cool functions, Cole

get_maxgrad <- function(fit){
  if(!fit$model$optimized){
    return(NULL)
  }
  maxgrad <- max(abs(fit$fit$final_gradient))
  return(maxgrad)
}

add_ci <- function(g, ci, alpha, showMedian = FALSE, medianCol = '#000000', ...){
  stopifnot(length(ci)==length(alpha))
  for(ii in 1:length(ci))
    g <- g+stat_summary(fun.data = median_hilow, fun.args = list(conf.int = ci[ii]),
                        geom = 'ribbon', alpha = alpha[ii],...)
  if(showMedian) g = g + stat_summary(fun = median, geom = "line", linewidth = 0.5, colour = medianCol)
  return(g)
}


# Get time series:
get_ts <- function(fits, nyears = 52){
  ff <- function(fit){
    if(!fit$model$optimized){
      ##  print(fit$model[1,1:3])
      return(NULL)
    }
    ssb <- data.frame(par='SSB',
                      year=1:nyears,
                      est=fit$fit$rep$SSB,
                      truth=fit$truth$SSB)
    ## if(fit$model$sdreport) ssb$se <- fit$sdrep$SE_rep$log_SSB
    recruits <- data.frame(par='recruits',
                           year=1:nyears,
                           est=fit$fit$rep$NAA[,1],
                           truth=fit$truth$NAA[,1])
    ## if(fit$model$sdreport) ssb$se <- fit$sdrep$SE_rep$log_SSB
    ## this fails w/ cbind for some reason??
    f <- data.frame(par='F',
                    year=1:nyears,
                    est=fit$fit$rep$F,
                    truth=fit$truth$F)
    ## if(fit$model$sdreport) f$se <- fit$sdrep$SE_rep$F
    ## this fails w/ cbind for some reason??
    # ecov <- data.frame(par='Ecov_out',
    #                    year=1:nyears,
    #                    ## take first one -- all the same
    #                    est=fit$fit$rep$Ecov_out[,1,1],
    #                    truth=fit$truth$Ecov_out[,1,1])
    # ts <- bind_rows(ssb, f, recruits, ecov) %>% bind_cols(fit$model) %>%
    #   mutate(rel_error=(est-truth)/truth, abs_error=est-truth,
    #          sim=as.factor(im), maxgrad=get_maxgrad(fit))
    ts <- bind_rows(ssb, f, recruits) %>% bind_cols(fit$model) %>%
      mutate(rel_error=(est-truth)/truth, abs_error=est-truth,
             sim=as.factor(im), maxgrad=get_maxgrad(fit))
    return(ts)
  }
  lapply(fits, function(i) ff(i)) %>% bind_rows() 
}

get_selex <- function(fits){
  ff <- function(fit){
    if(!fit$model$optimized){
      ##  print(fit$model)
      return(NULL)
    }
    s1 <- data.frame(par='selex_fishery',
                     age=1:10,
                     est=fit$fit$rep$selAA[[1]][1,],
                     truth=fit$truth$selAA[[1]][1,])
    s2 <- data.frame(par='selex_survey1',
                     age=1:10,
                     est=fit$fit$rep$selAA[[2]][1,],
                     truth=fit$truth$selAA[[2]][1,])
    s3 <- data.frame(par='selex_survey2',
                     age=1:50, # not really age but whatever
                     est=fit$fit$rep$selAL[[3]][1,],
                     truth=fit$truth$selAL[[3]][1,])
    selex <- bind_rows(s1,s2,s3) %>% bind_cols(fit$model) %>%
      mutate(rel_error=(est-truth)/truth, abs_error=est-truth,
             sim=as.factor(im), maxgrad=get_maxgrad(fit))
    return(selex)
  }
  lapply(fits, function(i) ff(i)) %>% bind_rows()%>% add_labels
}

get_laa <- function(fits){
  ff <- function(fit){
    if(!fit$model$optimized){
      ## print(fit$model)
      return(NULL)
    }
    laa <- list()
    for(year in seq_len(nrow(fit$fit$rep$LAA))){
      laa[[year]] <- data.frame(par='laa', age=1:10, year=year,
                                est=fit$fit$rep$LAA[year,],
                                truth=fit$truth$LAA[year,]) %>%
        bind_cols(fit$model)
    }
    laa <- laa %>% bind_rows() %>%
      mutate(rel_error=(est-truth)/truth, abs_error=est-truth,
             sim=as.factor(im),  maxgrad=get_maxgrad(fit))
    return(laa)
  }
  lapply(fits, function(i) ff(i)) %>% bind_rows() %>% add_labels
}

get_waa <- function(fits, nages = 10, waapos = 2){
  ff <- function(fit){
    if(!fit$model$optimized){
      ## print(fit$model)
      return(NULL)
    }
    waa <- list()
    for(year in seq_len(nrow(fit$fit$rep$pred_waa[waapos,,]))){ # plot survey waa
      waa[[year]] <- data.frame(par='waa', age=1:nages, year=year,
                                est=fit$fit$rep$pred_waa[waapos,year,],
                                truth=fit$truth$pred_waa[waapos,year,]) %>%
        bind_cols(fit$model)
    }
    waa <- waa %>% bind_rows() %>%
      mutate(rel_error=(est-truth)/truth, abs_error=est-truth,
             sim=as.factor(im),  maxgrad=get_maxgrad(fit))
    return(waa)
  }
  #lapply(fits, function(i) ff(i)) %>% bind_rows() %>% add_labels
  lapply(fits, function(i) ff(i)) %>% bind_rows()
}

get_pars <- function(fits){
  ff <- function(fit){
    if(!fit$model$optimized){
      ## print(fit$model)
      return(NULL)
    }
    #fit$empars$par2[fit$empars$par2 == 'log_N1_pars'] = 'log_N1_pars_1' # IMPORTANT to merge OM and EM dfs.
    pars <- merge(fit$ompars, fit$empars, by='par2') %>%
      filter(grepl(x=par.y, "Ecov|growth_a|SD_par|mean_rec_pars|logit_q|log_F1|log_N1_pars|logit_selpars"))
    # Transform to original scale (TODO: do this for other variables logit)
    pars$value.x = ifelse(test = grepl(x = par.x, "growth_a|SD_par|mean_rec_pars|log_F1|log_N1_pars"), 
                          yes = exp(pars$value.x), no = pars$value.x)
    pars$value.y = ifelse(test = grepl(x = par.y, "growth_a|SD_par|mean_rec_pars|log_F1|log_N1_pars"), 
                          yes = exp(pars$value.y), no = pars$value.y)
    pars <- pars %>% select(par=par.x, par2, truth=value.x, est=value.y) %>%
      bind_cols(fit$model) %>%
      mutate(rel_error=(est-truth)/truth, abs_error=est-truth,
             sim=as.factor(im),  maxgrad=get_maxgrad(fit))
    return(pars)
  }
  lapply(fits, function(i) ff(i)) %>% bind_rows() 
}

get_growth <- function(fits){
  ff <- function(fit){
    if(!fit$model$optimized){
      ##  print(fit$model)
      return(NULL)
    }
    ## !!will need to update this if the growth estimation changes!!
    p1 <- data.frame(par=c('k', 'L1', 'Linf'),
                     est=exp(fit$fit$rep$growth_a[1:3,1]),
                     truth=exp(fit$truth$growth_a[1:3,1]))
    ## p2 <- data.frame(par=c('SDold'),
    ##                  est=fit$empars$value[fit$empars$par=='SDgrowth_par'],
    ##                  truth=fit$ompars$value[fit$ompars$par=='SDgrowth_par'][2])
    ## p2 <- data.frame(par=c('SD1', 'SD2'),
    ##                  est=fit$fit$rep$expSD,
    ##                  truth=fit$truth$expSD)
    p2 <- NULL
    growth <- bind_rows(p1,p2) %>% bind_cols(fit$model) %>%
      mutate(rel_error=(est-truth)/truth, abs_error=est-truth,
             sim=as.factor(im),  maxgrad=get_maxgrad(fit))
    return(growth)
  }
  warning("need to fix growth pars")
  lapply(fits, function(i) ff(i)) %>% bind_rows() %>% add_labels
}

