# Code to make figures

library(ggplot2)
library(dplyr)
library(plyr)
library(tidyr)
theme_set(theme_bw())

# Clean workspace
rm(list = ls())

# Call aux functions
source('aux_functions.R')

# Save folder:
save_folder = 'plots'

# Read scenarios df
df.scenario = readRDS('inputs/df.scenarios.RDS')

# Order EM labels:
EM_order = c("WEm-paa-paa", "WNP-paa-paa", "LP-pal-pal", "LP-pal-paa", "LP-pal-pal/caal(r)", "LP-pal-pal/caal(s)",  
             "LEc-pal-pal", "LEc-pal-paa", "LEc-pal-pal/caal(r)", "LEc-pal-pal/caal(s)")


# -------------------------------------------------------------------------
# Read output files -------------------------------------------------------

# TS data:
ts_df1 = readRDS(file = 'outputs/ts_results_1.RDS')
ts_df2 = readRDS(file = 'outputs/ts_results_2.RDS')
ts_df = rbind(ts_df1, ts_df2)

# par data:
par_df1 = readRDS(file = 'outputs/par_results_1.RDS')
par_df2 = readRDS(file = 'outputs/par_results_2.RDS')
par_df = rbind(par_df1, par_df2)

# LAA data:
laa_df1 = readRDS(file = 'outputs/laa_results_1.RDS')
laa_df2 = readRDS(file = 'outputs/laa_results_2.RDS')
laa_df = rbind(laa_df1, laa_df2)

# WAA data:
waa_df1 = readRDS(file = 'outputs/waa_results_1.RDS')
waa_df2 = readRDS(file = 'outputs/waa_results_2.RDS')
waa_df = rbind(waa_df1, waa_df2)


# -------------------------------------------------------------------------
# PAR plot (for ALL scenarios):
temp = par_df %>% filter(par %in% c('log_F1', 'log_N1_pars', 'logit_q', 'mean_rec_pars'))
temp = temp %>% filter(maxgrad < 1)
temp = temp %>% mutate(method = factor(method, levels = c('EWAA', 'WAA', 'growth', 'Ecov'),
                                     labels = c('WEm', 'WNP', 'LP', 'LEc')))
temp$caal_samp[temp$caal_samp == 'random'] = '(r)'
temp$caal_samp[temp$caal_samp == 'strat'] = '(s)'
temp$index_data[temp$index_data == 'caal'] = 'pal/caal'
temp = temp %>% mutate(em_label = if_else(condition = index_data == 'pal/caal', 
                                                true = paste0(method,'-',catch_data,'-',index_data,caal_samp),
                                                false = paste(method,catch_data,index_data, sep = '-')))
temp = temp %>% mutate(em_label = factor(em_label, levels = EM_order))
temp = temp %>% mutate(par2 = factor(par, levels = c('mean_rec_pars', 'logit_q', 'log_N1_pars', 'log_F1'),
                                     labels = c(expression(bar(R)), 'Q', expression(N["1,1"]), 'F[1]')))
temp = temp %>% mutate(om_label = factor(growth_par, levels = 0:3,
                                     labels = c('Time~invariant', Variability~"in"~k, expression(Variability~"in"~L[infinity]), expression(Variability~"in"~L[1]))))

# Make plot:
p1 = ggplot(temp, aes(x=em_label, y=rel_error, fill=data_scen, color = data_scen)) +
        geom_violin(position=position_dodge(0.6), alpha = 0.75) +
        theme_bw() +
        coord_cartesian(ylim = c(-0.5, 0.5)) +
        geom_hline(yintercept=0, color=1, linetype='dashed') +
        theme(legend.position = 'none',
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
              strip.text = element_text(size = 10)) +
        scale_y_continuous(breaks=c(-0.5, 0, 0.5)) +
        xlab(NULL) + ylab('Relative error') +
        facet_grid(par2 ~ om_label, labeller = my_label_parsed) 
ggsave(filename = file.path(save_folder, 'par.jpg'), plot = p1, 
       width = 190 , height = 220, units = 'mm', dpi = 500)


# -------------------------------------------------------------------------
# TS plot (median over years, for ALL scenarios):
temp = ts_df %>% filter(maxgrad < 1)
temp = temp %>% dplyr::group_by(par, data_scen, catch_data, index_data, caal_samp, method, growth_par, im) %>% 
            dplyr::summarise(rel_error = median(rel_error))
temp = temp %>% mutate(method = factor(method, levels = c('EWAA', 'WAA', 'growth', 'Ecov'),
                                       labels = c('WEm', 'WNP', 'LP', 'LEc')))
temp$caal_samp[temp$caal_samp == 'random'] = '(r)'
temp$caal_samp[temp$caal_samp == 'strat'] = '(s)'
temp$index_data[temp$index_data == 'caal'] = 'pal/caal'
temp = temp %>% mutate(em_label = if_else(condition = index_data == 'pal/caal', 
                                          true = paste0(method,'-',catch_data,'-',index_data,caal_samp),
                                          false = paste(method,catch_data,index_data, sep = '-')))
temp = temp %>% mutate(em_label = factor(em_label, levels = EM_order))
temp = temp %>% mutate(par2 = factor(par, levels = c('SSB', 'Rec', 'F'),
                                     labels = c('SSB', 'R', 'F')))
temp = temp %>% mutate(om_label = factor(growth_par, levels = 0:3,
                                         labels = c('Time~invariant', Variability~"in"~k, expression(Variability~"in"~L[infinity]), expression(Variability~"in"~L[1]))))

# Make plot:
p2 = ggplot(temp, aes(x=em_label, y=rel_error, fill=data_scen, color = data_scen)) +
  geom_violin(position=position_dodge(0.6), alpha = 0.75) +
  theme_bw() +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  geom_hline(yintercept=0, color=1, linetype='dashed') +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text = element_text(size = 10)) +
  scale_y_continuous(breaks=c(-0.3, 0, 0.3)) +
  xlab(NULL) + ylab('Relative error') +
  facet_grid(par2 ~ om_label, labeller = my_label_parsed) 
ggsave(filename = file.path(save_folder, 'ts.jpg'), plot = p2, 
       width = 190 , height = 180, units = 'mm', dpi = 500)

# -------------------------------------------------------------------------
# TS plot (by year, for ALL scenarios):

# TODO


# -------------------------------------------------------------------------
# Growth parameters (only for growth, Ecov, and SemiP scenarios):
temp = par_df %>% filter(par %in% c('k', 'Linf', 'L1', 'SD1', 'SDA'))
temp = temp %>% filter(maxgrad < 1, scenario %in% c(9:40, 49:80)) # select only relevant scenarios
temp = temp %>% mutate(method = factor(method, levels = c('growth', 'Ecov'),
                                       labels = c('LP', 'LEc')))
temp$caal_samp[temp$caal_samp == 'random'] = '(r)'
temp$caal_samp[temp$caal_samp == 'strat'] = '(s)'
temp$index_data[temp$index_data == 'caal'] = 'pal/caal'
temp = temp %>% mutate(em_label = if_else(condition = index_data == 'pal/caal', 
                                          true = paste0(method,'-',catch_data,'-',index_data,caal_samp),
                                          false = paste(method,catch_data,index_data, sep = '-')))
temp = temp %>% mutate(em_label = factor(em_label, levels = EM_order))
temp = temp %>% mutate(par2 = factor(par, levels = c('k', 'Linf', 'L1', 'SD1', 'SDA'),
                                     labels = c('k', expression(L[infinity]), expression(L[1]), expression(SD[1]), expression(SD[A]))))
temp = temp %>% mutate(om_label = factor(growth_par, levels = 0:3,
                                         labels = c('Time~invariant', Variability~"in"~k, expression(Variability~"in"~L[infinity]), expression(Variability~"in"~L[1]))))

# Make plot:
p4 = ggplot(temp, aes(x=em_label, y=rel_error, fill=data_scen, color = data_scen)) +
  geom_violin(position=position_dodge(0.6), alpha = 0.75) +
  theme_bw() +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  geom_hline(yintercept=0, color=1, linetype='dashed') +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text = element_text(size = 10)) +
  scale_y_continuous(breaks=c(-0.5, 0, 0.5)) +
  xlab(NULL) + ylab('Relative error') +
  facet_grid(par2 ~ om_label, labeller = my_label_parsed) 
ggsave(filename = file.path(save_folder, 'growth.jpg'), plot = p4, 
       width = 190 , height = 240, units = 'mm', dpi = 500)

# -------------------------------------------------------------------------
# Ecov parameters (only for Ecov scenarios):
temp = par_df %>% filter(par %in% c('sigma', 'rho', 'EcovBeta')) # meanEcov too? 
temp = temp %>% filter(maxgrad < 1, scenario %in% c(25:40, 65:80)) # select only relevant scenarios
temp = temp %>% mutate(method = factor(method, levels = c('Ecov'),
                                       labels = c('LEc')))
temp$caal_samp[temp$caal_samp == 'random'] = '(r)'
temp$caal_samp[temp$caal_samp == 'strat'] = '(s)'
temp$index_data[temp$index_data == 'caal'] = 'pal/caal'
temp = temp %>% mutate(em_label = if_else(condition = index_data == 'pal/caal', 
                                          true = paste0(method,'-',catch_data,'-',index_data,caal_samp),
                                          false = paste(method,catch_data,index_data, sep = '-')))
temp = temp %>% mutate(em_label = factor(em_label, levels = EM_order))
temp = temp %>% mutate(par2 = factor(par, levels = c('sigma', 'rho', 'EcovBeta'),
                                     labels = c(expression(sigma[X]^2), expression(rho[X]), expression(beta))))
temp = temp %>% mutate(om_label = factor(growth_par, levels = 0:3,
                                         labels = c('Time~invariant', Variability~"in"~k, expression(Variability~"in"~L[infinity]), expression(Variability~"in"~L[1]))))

# Make plot:
p5 = ggplot(temp, aes(x=em_label, y=rel_error, fill=data_scen, color = data_scen)) +
  geom_violin(position=position_dodge(0.6), alpha = 0.75) +
  theme_bw() +
  coord_cartesian(ylim = c(-1, 1)) +
  geom_hline(yintercept=0, color=1, linetype='dashed') +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text = element_text(size = 10)) +
  scale_y_continuous(breaks=c(-1, -0.5, 0, 0.5, 1)) +
  xlab(NULL) + ylab('Relative error') +
  facet_grid(par2 ~ om_label, labeller = my_label_parsed) 
ggsave(filename = file.path(save_folder, 'ecov.jpg'), plot = p5, 
       width = 190 , height = 220, units = 'mm', dpi = 500)

# -------------------------------------------------------------------------
# WAA info (median over years, only for WAA and Ewaa scenarios):
temp = waa_df
temp = temp %>% filter(maxgrad < 1, scenario %in% c(1:8, 41:48)) # select only relevant scenarios
temp = temp %>% dplyr::group_by(age, data_scen, catch_data, index_data, caal_samp, method, growth_par, im) %>% 
            dplyr::summarise(rel_error = median(rel_error))
temp = temp %>% mutate(method = factor(method, levels = c('EWAA', 'WAA'),
                                       labels = c('WEm', 'WNP')))
temp$caal_samp[temp$caal_samp == 'random'] = '(r)'
temp$caal_samp[temp$caal_samp == 'strat'] = '(s)'
temp$index_data[temp$index_data == 'caal'] = 'pal/caal'
temp = temp %>% mutate(em_label = if_else(condition = index_data == 'pal/caal', 
                                          true = paste0(method,'-',catch_data,'-',index_data,caal_samp),
                                          false = paste(method,catch_data,index_data, sep = '-')))
temp = temp %>% mutate(em_label = factor(em_label, levels = EM_order))
temp = temp %>% mutate(par2 = factor(age, levels = 1:10,
                                     labels = 1:10))
temp = temp %>% mutate(om_label = factor(growth_par, levels = 0:3,
                                         labels = c('Time~invariant', Variability~"in"~k, expression(Variability~"in"~L[infinity]), expression(Variability~"in"~L[1]))))

# Make plot:
p6 = ggplot(temp, aes(x=em_label, y=rel_error, fill=data_scen, color = data_scen)) +
  geom_violin(position=position_dodge(0.6), alpha = 0.75) +
  theme_bw() +
  coord_cartesian(ylim = c(-0.2, 0.2)) +
  geom_hline(yintercept=0, color=1, linetype='dashed') +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text = element_text(size = 10)) +
  scale_y_continuous(breaks=c(-0.1, 0, 0.1)) +
  xlab(NULL) + ylab('Relative error') +
  facet_grid(par2 ~ om_label, labeller = my_label_parsed) 
ggsave(filename = file.path(save_folder, 'waa.jpg'), plot = p6, 
       width = 190 , height = 240, units = 'mm', dpi = 500)


# -------------------------------------------------------------------------
# LAA info (median over years, only for growth Ecov SemiG and LAA scenarios):
temp = laa_df
temp = temp %>% filter(maxgrad < 1, scenario %in% c(9:40, 49:80)) # select only relevant scenarios
temp = temp %>% dplyr::group_by(age, data_scen, catch_data, index_data, caal_samp, method, growth_par, im) %>% 
  dplyr::summarise(rel_error = median(rel_error))
temp = temp %>% mutate(method = factor(method, levels = c('growth', 'Ecov'),
                                       labels = c('LP', 'LEc')))
temp$caal_samp[temp$caal_samp == 'random'] = '(r)'
temp$caal_samp[temp$caal_samp == 'strat'] = '(s)'
temp$index_data[temp$index_data == 'caal'] = 'pal/caal'
temp = temp %>% mutate(em_label = if_else(condition = index_data == 'pal/caal', 
                                          true = paste0(method,'-',catch_data,'-',index_data,caal_samp),
                                          false = paste(method,catch_data,index_data, sep = '-')))
temp = temp %>% mutate(em_label = factor(em_label, levels = EM_order))
temp = temp %>% mutate(par2 = factor(age, levels = 1:10,
                                     labels = 1:10))
temp = temp %>% mutate(om_label = factor(growth_par, levels = 0:3,
                                         labels = c('Time~invariant', Variability~"in"~k, expression(Variability~"in"~L[infinity]), expression(Variability~"in"~L[1]))))

# Make plot:
p7 = ggplot(temp, aes(x=em_label, y=rel_error, fill=data_scen, color = data_scen)) +
  geom_violin(position=position_dodge(0.6), alpha = 0.75) +
  theme_bw() +
  coord_cartesian(ylim = c(-0.2, 0.2)) +
  geom_hline(yintercept=0, color=1, linetype='dashed') +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text = element_text(size = 10)) +
  scale_y_continuous(breaks=c(-0.1, 0, 0.1)) +
  xlab(NULL) + ylab('Relative error') +
  facet_grid(par2 ~ om_label, labeller = my_label_parsed) 
ggsave(filename = file.path(save_folder, 'laa.jpg'), plot = p7, 
       width = 190 , height = 240, units = 'mm', dpi = 500)


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# 
# # -------------------------------------------------------------------------
# # Part I: Only age data ---------------------------------------------------
# 
# sc_code = 'age_age'
# 
# # Read outputs rds and save them:
# # WARNING: this could take up to hours depending on the number of scenarios and replicates
# scenario_names = paste0('scenario', 1:6) # Here select what scenarios to analyze
# fits = list()
# countList = 1
# for(k in seq_along(scenario_names)) {
#   replicates = list.files(path = file.path('results', scenario_names[k]))
#   for(j in seq_along(replicates)) {
#     fits[[countList]] = readRDS(file = file.path('results', scenario_names[k], replicates[j]))
#     countList = countList + 1
#   }
# }
# # saveRDS(fits, 'results/fits_age.RDS')
# 
# # Read merged output:
# # fits = readRDS("results/fits_age.RDS")
# 
# # check convergence stats
# models <- lapply(fits, function(x) x$model) %>% bind_rows 
# group_by(models, Scenario) %>%
#   summarize(pct.converged=mean(optimized), n.converged=sum(optimized), n.run=length(optimized))
# 
# # Quick exploration via plots ---------------------------------------------
# 
# # Time series: SSB, F, and Rec:
# ts <- get_ts(fits, nyears = 40) %>% filter(abs(maxgrad)<1)
# ts$par = factor(ts$par, levels = c('SSB', 'recruits', 'F'), labels = c('SSB', 'F', 'Recruitment'))
# ts$scenario = factor(ts$scenario, levels = as.character(1:6), labels = c('OM:k-EM:EWAA', 'OM:Li-EM:EWAA', 'OM:L1-EM:EWAA',
#                                                                            'OM:k-EM:WAAre','OM:Li-EM:WAAre','OM:L1-EM:WAAre'))
# 
# g = ggplot(ts, aes(year,y = rel_error)) + 
#   facet_grid(par ~ scenario)+ 
#   geom_hline(yintercept=0, color=2) +
#   xlab('Year') + ylab('Relative error') +
#   coord_cartesian(ylim = 1*c(-1,1)) +
#   theme(strip.background = element_blank())
# g2 = add_ci(g, ci=c(.5,.95), alpha=c(.4,.4), fill = '#606060', showMedian = TRUE)
# ggsave(file.path(save_folder, paste0(sc_code, '_ts.jpg')), g2, width = 180 , height = 130, units = 'mm', dpi = 500)
# 
# 
# # Parameters: Q, M, log_F1, and mean_rec
# pars <- get_pars(fits) %>% filter(abs(maxgrad)<1)
# pars$par2 = factor(pars$par2, levels = c('mean_rec_pars', 'log_N1_pars', 'log_F1', 'M_a', 'logit_q'))
# pars$scenario = factor(pars$scenario, levels = as.character(1:6), labels = c('OM:k-EM:EWAA', 'OM:Li-EM:EWAA', 'OM:L1-EM:EWAA',
#                                                                              'OM:k-EM:WAAre','OM:Li-EM:WAAre','OM:L1-EM:WAAre'))
# 
# g <- ggplot(pars, aes(par2, rel_error)) +
#   geom_hline(yintercept=0, col=2, lwd=1) + #ylim(-3,3)+
#   geom_boxplot(fill = '#606060', alpha = 0.5) +
#   scale_x_discrete(labels = c('mean_rec_pars'   = expression(bar(R)),
#                               'log_N1_pars' = expression(N[1*","*1]),
#                               'log_F1' = expression(F[1]),
#                               'M_a' = expression(M),
#                               'logit_q' = expression(Q))) +
#   coord_cartesian(ylim=c(-1,1))+
#   ylab('Relative error') +
#   facet_wrap(~scenario, scales='free', nrow = 2) +
#   theme(axis.text.x = element_text(angle = 90)) + labs(x=NULL) +
#   theme(panel.spacing = unit(0, "cm"), strip.background = element_blank())
# ggsave(file.path(save_folder, paste0(sc_code, '_par.jpg')), g, width = 180 , height = 160, units = 'mm', dpi = 500)
# 
# # Mean weight-at-age:
# 
# waa <- get_waa(fits) %>% filter(abs(maxgrad)<1)
# tmp <- filter(waa, year==40) # terminal year
# tmp$scenario = factor(tmp$scenario, levels = as.character(1:6), labels = c('OM:k-EM:EWAA', 'OM:Li-EM:EWAA', 'OM:L1-EM:EWAA',
#                                                                            'OM:k-EM:WAAre','OM:Li-EM:WAAre','OM:L1-EM:WAAre'))
# g <- ggplot(tmp, aes(age, rel_error)) + 
#   geom_hline(yintercept=0, color=2) +
#   facet_wrap(~scenario, nrow = 2)+
#   geom_hline(yintercept=0, col=2) + labs(y='Relative error', x = 'Age') +
#   scale_x_continuous(breaks = 1:10, labels = 1:10) +
#   theme(strip.background = element_blank())
# g2 = add_ci(g, ci=c(.5,.95), alpha=c(.4,.4), fill = '#606060', showMedian = TRUE)
# ggsave(file.path(save_folder, paste0(sc_code, '_waa_termy.jpg')), g2, width = 180 , height = 160, units = 'mm', dpi = 500)
# 
# # Median RE by year and age:
# tmp <- waa %>% group_by(scenario, age, year) %>% summarize(n=n(), mre=median(rel_error))
# tmp$scenario = factor(tmp$scenario, levels = as.character(1:6), labels = c('OM:k-EM:EWAA', 'OM:Li-EM:EWAA', 'OM:L1-EM:EWAA',
#                                                                            'OM:k-EM:WAAre','OM:Li-EM:WAAre','OM:L1-EM:WAAre'))
# 
# g <- ggplot(tmp, aes(year, age, size=abs(mre), color=mre>0)) +
#   geom_point(alpha=.5) +
#   scale_y_continuous(breaks = 1:10, labels = 1:10) +
#   labs(y='Age', x = 'Year', size = 'RE', color = 'Bias') +
#   facet_wrap(~scenario, nrow = 2) +
#   theme(strip.background = element_blank(),
#         legend.position = 'bottom')
# ggsave(file.path(save_folder, paste0(sc_code, '_waa_ya.jpg')), g, width = 180 , height = 160, units = 'mm', dpi = 500)
# 
# 
# 
# 
# # -------------------------------------------------------------------------
# # Part I: age and length data ---------------------------------------------------
# 
# sc_code = 'age_len'
# 
# # Read outputs rds and save them:
# # WARNING: this could take up to hours depending on the number of scenarios and replicates
# scenario_names = paste0('scenario', 7:12) # Here select what scenarios to analyze
# fits = list()
# countList = 1
# for(k in seq_along(scenario_names)) {
#   replicates = list.files(path = file.path('results', scenario_names[k]))
#   for(j in seq_along(replicates)) {
#     fits[[countList]] = readRDS(file = file.path('results', scenario_names[k], replicates[j]))
#     countList = countList + 1
#   }
# }
# # saveRDS(fits, 'results/fits_age.RDS')
# 
# # Read merged output:
# # fits = readRDS("results/fits_age.RDS")
# 
# # check convergence stats
# models <- lapply(fits, function(x) x$model) %>% bind_rows 
# group_by(models, Scenario) %>%
#   summarize(pct.converged=mean(optimized), n.converged=sum(optimized), n.run=length(optimized))
# 
# # Time series: SSB, F, and Rec:
# ts <- get_ts(fits, nyears = 45) %>% filter(abs(maxgrad)<1)
# ts$par = factor(ts$par, levels = c('SSB', 'recruits', 'F'), labels = c('SSB', 'F', 'Recruitment'))
# ts$scenario = factor(ts$scenario, levels = as.character(7:12), labels = c('OM:k-EM:kar1(AL)', 'OM:Li-EM:Liar1(AL)', 'OM:L1-EM:L1ar1(AL)',
#                                                                          'OM:k-EM:kar1(LA)','OM:Li-EM:Liar1(LA)','OM:L1-EM:L1ar1(LA)'))
# 
# g = ggplot(ts, aes(year,y = rel_error)) + 
#   facet_grid(par ~ scenario)+ 
#   geom_hline(yintercept=0, color=2) +
#   xlab('Year') + ylab('Relative error') +
#   coord_cartesian(ylim = 1*c(-1,1)) +
#   theme(strip.background = element_blank())
# g2 = add_ci(g, ci=c(.5,.95), alpha=c(.4,.4), fill = '#606060', showMedian = TRUE)
# ggsave(file.path(save_folder, paste0(sc_code, '_ts.jpg')), g2, width = 180 , height = 130, units = 'mm', dpi = 500)
# 
# 
# # Parameters: Q, M, log_F1, and mean_rec
# pars <- get_pars(fits) %>% filter(abs(maxgrad)<1)
# pars$par2 = factor(pars$par2, levels = c('mean_rec_pars', 'log_N1_pars', 'log_F1', 'M_a', 'logit_q'))
# pars$scenario = factor(pars$scenario, levels = as.character(7:12), labels = c('OM:k-EM:kar1(AL)', 'OM:Li-EM:Liar1(AL)', 'OM:L1-EM:L1ar1(AL)',
#                                                                               'OM:k-EM:kar1(LA)','OM:Li-EM:Liar1(LA)','OM:L1-EM:L1ar1(LA)'))
# 
# g <- ggplot(pars, aes(par2, rel_error)) +
#   geom_hline(yintercept=0, col=2, lwd=1) + #ylim(-3,3)+
#   geom_boxplot(fill = '#606060', alpha = 0.5) +
#   scale_x_discrete(labels = c('mean_rec_pars'   = expression(bar(R)),
#                               'log_N1_pars' = expression(N[1*","*1]),
#                               'log_F1' = expression(F[1]),
#                               'M_a' = expression(M),
#                               'logit_q' = expression(Q))) +
#   coord_cartesian(ylim=c(-1,1))+
#   ylab('Relative error') +
#   facet_wrap(~scenario, scales='free', nrow = 2) +
#   theme(axis.text.x = element_text(angle = 90)) + labs(x=NULL) +
#   theme(panel.spacing = unit(0, "cm"), strip.background = element_blank())
# ggsave(file.path(save_folder, paste0(sc_code, '_par.jpg')), g, width = 180 , height = 160, units = 'mm', dpi = 500)
# 
# # Growth parameters:
# growth <- get_growth(fits) %>% filter(abs(maxgrad)<1)
# growth$par = factor(growth$par, levels = c('k', 'Linf', 'L1', 'SD1', 'SDA'))
# growth$scenario = factor(growth$scenario, levels = as.character(7:12), labels = c('OM:k-EM:kar1(AL)', 'OM:Li-EM:Liar1(AL)', 'OM:L1-EM:L1ar1(AL)',
#                                                                                   'OM:k-EM:kar1(LA)','OM:Li-EM:Liar1(LA)','OM:L1-EM:L1ar1(LA)'))
# 
# g <- ggplot(growth, aes(par, rel_error)) +
#   geom_hline(yintercept=0, col=2, lwd=1) + #ylim(-3,3)+
#   geom_boxplot(fill = '#606060', alpha = 0.5) +
#   scale_x_discrete(labels = c('K' = expression(k),
#                               'Linf'   = expression(L[inf]),
#                               'L1' = expression(L[1]),
#                               'SD1' = expression(SD[1]),
#                               'SDA' = expression(SD[A]))) +
#   coord_cartesian(ylim=c(-1,1))+
#   ylab('Relative error') +
#   facet_wrap(~scenario, scales='free', nrow = 2) +
#   theme(axis.text.x = element_text(angle = 90)) + labs(x=NULL) +
#   theme(panel.spacing = unit(0, "cm"), strip.background = element_blank())
# ggsave(file.path(save_folder, paste0(sc_code, '_growth.jpg')), g, width = 180 , height = 160, units = 'mm', dpi = 500)
# 
# # Plot mean length at age:
# laa <- get_laa(fits) %>% filter(abs(maxgrad)<1)
# tmp <- filter(laa, year==40) # terminal year
# tmp$scenario = factor(tmp$scenario, levels = as.character(7:12), labels = c('OM:k-EM:kar1(AL)', 'OM:Li-EM:Liar1(AL)', 'OM:L1-EM:L1ar1(AL)',
#                                                                               'OM:k-EM:kar1(LA)','OM:Li-EM:Liar1(LA)','OM:L1-EM:L1ar1(LA)'))
# 
# g <- ggplot(tmp, aes(age, rel_error)) + 
#   geom_hline(yintercept=0, color=2) +
#   facet_wrap(~scenario, nrow = 2)+
#   geom_hline(yintercept=0, col=2) + labs(y='Relative error', x = 'Age') +
#   scale_x_continuous(breaks = 1:10, labels = 1:10) +
#   theme(strip.background = element_blank())
# g2 = add_ci(g, ci=c(.5,.95), alpha=c(.4,.4), fill = '#606060', showMedian = TRUE)
# ggsave(file.path(save_folder, paste0(sc_code, '_waa_termy.jpg')), g2, width = 180 , height = 160, units = 'mm', dpi = 500)
# 
# # Median RE by year and age:
# tmp <- laa %>% group_by(scenario, age, year) %>% summarize(n=n(), mre=median(rel_error))
# tmp$scenario = factor(tmp$scenario, levels = as.character(7:12), labels = c('OM:k-EM:kar1(AL)', 'OM:Li-EM:Liar1(AL)', 'OM:L1-EM:L1ar1(AL)',
#                                                                             'OM:k-EM:kar1(LA)','OM:Li-EM:Liar1(LA)','OM:L1-EM:L1ar1(LA)'))
# 
# g <- ggplot(tmp, aes(year, age, size=abs(mre), color=mre>0)) +
#   geom_point(alpha=.5) +
#   scale_y_continuous(breaks = 1:10, labels = 1:10) +
#   labs(y='Age', x = 'Year', size = 'RE', color = 'Bias') +
#   facet_wrap(~scenario, nrow = 2) +
#   theme(strip.background = element_blank(),
#         legend.position = 'bottom')
# ggsave(file.path(save_folder, paste0(sc_code, '_laa_ya.jpg')), g, width = 180 , height = 160, units = 'mm', dpi = 500)
# 
# 
# 
# # -------------------------------------------------------------------------
# 
# 
# 
# # ecov <- filter(pars, grepl(x=par, pattern='Ecov'))
# # g <- ggplot(ecov, aes(emf, rel_error)) +
# #   geom_hline(yintercept=0, col=2, lwd=1) + #ylim(-3,3)+
# #   geom_violin() +
# #   geom_jitter(width=.3, height=0, alpha=.3) +
# #   coord_cartesian(ylim=c(-1,1))+
# #   facet_grid(omf~par2, scales='free')+
# #   theme(axis.text.x = element_text(angle = 90)) + labs(x=NULL)
# # ## g <- ggplot(ecov, aes(par2, rel_error)) +
# # ##   geom_hline(yintercept=0, col=2, lwd=1) + #ylim(-3,3)+
# # ##   geom_violin() +
# # ##   coord_cartesian(ylim=c(-1,1))+
# # ##   facet_grid(omf+emf~par, scales='free')
# # ggsave("../plots/relerror_pars_ecov.png", g, width=6 , height=4)
# 
# # growthpars <- filter(pars, grepl(x=par, pattern='growth_a'))
# # g <- ggplot(growthpars, aes(emf, rel_error)) +
# #   geom_hline(yintercept=0, col=2, lwd=1) + #ylim(-3,3)+
# #   geom_violin() +
# #   geom_jitter(width=.3, height=0, alpha=.3) +
# #   coord_cartesian(ylim=c(-1,1))+
# #   facet_grid(omf~par2, scales='free')+
# #   theme(axis.text.x = element_text(angle = 90)) + labs(x=NULL)
# # ggsave("../plots/relerror_pars_growth.png", g, width=5 , height=4)
# ## test <- filter(ts, par=='SSB' & sim==1 & em==2)
# ## plot(test$year, test$truth)
# ## lines(test$year, test$est)
# 
# 
# # selex <- get_selex(fits) %>% filter(abs(maxgrad)<1)
# # ggplot(selex, aes(age, est, group=sim)) +
# #   geom_line() + facet_grid(par~emf+om)
# # g <- ggplot(selex, aes(age, abs_error, group=sim)) + geom_line(alpha=.5) +
# #   geom_hline(yintercept=0, col=2, lwd=1) +
# #   facet_grid(par~omf+emf)
# # ggsave("../plots/abserror_selex.png", g, width=12 , height=5)
# 
# 
# 
# 
# 
