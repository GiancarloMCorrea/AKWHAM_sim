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
  # lapply(fits, function(i) ff(i)) %>% bind_rows()%>% add_labels
  lapply(fits, function(i) ff(i)) %>% bind_rows() 
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
  # lapply(fits, function(i) ff(i)) %>% bind_rows() %>% add_labels
  lapply(fits, function(i) ff(i)) %>% bind_rows() 
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
      filter(grepl(x=par.y, "Ecov|growth_a|SD_par|mean_rec_pars|logit_q|log_F1|log_N1_pars|M_a"))
    # Transform to original scale (TODO: do this for other variables logit)
	pars = pars %>% mutate(value.x = if_else(grepl(x = par.x, "growth_a|SD_par|mean_rec_pars|log_F1|log_N1_pars|M_a"), 
											exp(value.x), value.x),
						   value.y = if_else(grepl(x = par.y, "growth_a|SD_par|mean_rec_pars|log_F1|log_N1_pars|M_a"), 
											exp(value.y), value.y))
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
    p2 <- data.frame(par=c('SD1', 'SDA'),
                     est=fit$fit$rep$SD_len,
                     truth=fit$truth$SD_len)
    growth <- bind_rows(p1,p2) %>% bind_cols(fit$model) %>%
      mutate(rel_error=(est-truth)/truth, abs_error=est-truth,
             sim=as.factor(im),  maxgrad=get_maxgrad(fit))
    return(growth)
  }
  # warning("need to fix growth pars")
  # lapply(fits, function(i) ff(i)) %>% bind_rows() %>% add_labels
  lapply(fits, function(i) ff(i)) %>% bind_rows()
}

# Aux function to plot:
my_label_parsed <- function (variable, value) {
  if (variable == "impact") {
    return(as.character(value))
  } else {
    llply(as.character(value), function(x) parse(text = x))    
  }
}

# Set EM and OM labels in df to plot:
set_labels = function(df, selex_type = 'fixed', caal_type = 'random') {
  
  temp = df %>% filter(maxgrad < 1) # convergent replicates
  temp = temp %>% filter(age_selex %in% selex_type)
  temp = temp %>% filter(caal_samp %in% caal_type)
  temp = temp %>% mutate(method = case_when(method == 'EWAA' ~ 'WEm', 
                                            method == 'WAA' ~ 'WNP'))
  temp = temp %>% mutate(re_method = case_when(re_method == '2dar1' ~ '2D', 
                                               re_method == '3dgmrf' ~ '3D',
                                               .default = re_method))
  temp = temp %>% mutate(em_method = case_when(method == 'WEm' ~ 'WEm', 
                                               method == 'WNP' ~ paste(method, re_method, sep = '-')))
  temp = temp %>% mutate(em_method = factor(em_method, levels = c('WEm', 'WNP-iid', 'WNP-2D', 'WNP-3D')))
  temp$caal_samp[temp$caal_samp == 'random'] = 'Rand'
  temp$caal_samp[temp$caal_samp == 'strat'] = 'Strat'
  temp$age_selex[temp$age_selex == 'fixed'] = 'Fixed'
  temp$age_selex[temp$age_selex == 'varying'] = 'Vary'
  # EM label:
  temp = temp %>% mutate(em_label = em_method)
  temp = temp %>% mutate(data_scen = factor(data_scen, levels = c('rich','poor')))
  temp = temp %>% mutate(om_label = factor(growth_var, levels = 0:2,
                                           labels = c('Time~invariant', Variability~"in"~k~"/"~L[infinity], 
                                                      expression(Variability~"in"~L[1]))))
  # temp = temp %>% mutate(Ecov_sim = factor(Ecov_sim, levels = c('stationary', 'trend'),
  #                                        labels = c('Stationary', 'Trend')))

  return(temp)
  
}

filter_iter = function(df, first_sims = 100) {
  
  filter1 = tapply(df$im, df$scenario, unique)
  for(i in seq_along(filter1)) {
    filter1[[i]] = sort(filter1[[i]])
    filter1[[i]] = filter1[[i]][1:first_sims] # first first_sims replicates
    filter1[[i]] = data.frame(scenario = names(filter1)[i], im = filter1[[i]])
  }

  sel_iter = do.call(rbind.data.frame, filter1)
  sel_iter = sel_iter %>% mutate(scen_im = paste(scenario, im, sep = '-'))

  df = df %>% mutate(scen_im = paste(scenario, im, sep = '-'))
  df2 = df[df$scen_im %in% sel_iter$scen_im, ]

  return(df2)

}

# To make plot by parameter and rel_error (data_scen by color)
make_plot_1 = function(df, this_factor, col_vals, y_break = 0.4, violin_sep = 0.4,
                       leg_pos = 'none', leg_title = NULL, alpha_level = 0.6) {

  my_plot =  ggplot(df, aes(x=em_label, y=rel_error, fill={{this_factor}})) +
      geom_violin(position=position_dodge(violin_sep), alpha = alpha_level, color = NA) +
      scale_fill_manual(values = col_vals) +
      coord_cartesian(ylim = y_break*c(-1, 1)) +
      geom_hline(yintercept=0, color=1, linetype='dashed') +
      theme(legend.position = leg_pos,
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 9.3),
            strip.text = element_text(size = 10),
            strip.background = element_rect(fill="white")) +
      scale_y_continuous(breaks=c(-1*y_break, 0, 1*y_break)) +
      xlab(NULL) + ylab('Relative error') +
      #facet_nested(par2+Ecov_sim ~ om_label, labeller = 'label_parsed')
      facet_grid(par2 ~ om_label, labeller = 'label_parsed')
  
  if(!is.null(leg_title)) my_plot = my_plot + guides(fill=guide_legend(title=leg_title))

  return(my_plot)

}

# To make plot by parameter and var_values (data_scen by color)
make_plot_3 = function(df, this_var, this_factor, col_vals, violin_sep = 0.4,
                       leg_pos = 'none', leg_title = NULL, alpha_level = 0.6,
                       var_name = 'Variable name') {
  
  my_plot =  ggplot(df, aes(x=em_label, y={{this_var}}, fill={{this_factor}})) +
    geom_violin(position=position_dodge(violin_sep), alpha = alpha_level, color = NA) +
    scale_fill_manual(values = col_vals) +
    theme(legend.position = leg_pos,
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 9.3),
          strip.text = element_text(size = 10),
          strip.background = element_rect(fill="white")) +
    xlab(NULL) + ylab(var_name) +
    facet_grid(par2 ~ om_label, labeller = 'label_parsed', scales = 'free_y')
  
  if(!is.null(leg_title)) my_plot = my_plot + guides(fill=guide_legend(title=leg_title))
  
  return(my_plot)
  
}
