# Code to make figures
library(ggplot2)
library(dplyr)
library(plyr)
library(tidyr)
library(ggh4x)
library(RColorBrewer)
theme_set(theme_bw())

# Clean workspace
rm(list = ls())

# Call aux functions
source('aux_functions.R')

# Save plot folder:
save_folder = 'plots'

# Output folder:
output_folder = 'outputs'

# Read scenarios df
df.scenario = readRDS('inputs/df.scenarios.RDS')

# Define figure type:
fig_type = '.png'

# Color palettes
colpal1 = brewer.pal(n = 8, name = 'Set1')[1:2] # for data poor rich plots
colpal2 = brewer.pal(n = 8, name = 'Paired')[c(3:4, 7:8)]

# Order EM labels:
# EM_order = c("WEm:paa(r)/paa(r)", "WEm:paa(s)/paa(s)", "WNP:paa(r)/paa(r)", "WNP:paa(s)/paa(s)")

# -------------------------------------------------------------------------
# Read output files -------------------------------------------------------

# TS data:
ts_df = readRDS(file = file.path(output_folder, 'ts_results.RDS'))
# par data:
par_df = readRDS(file = file.path(output_folder, 'par_results.RDS'))
# WAA data:
waa_df = readRDS(file = file.path(output_folder, 'waa_results.RDS'))
# Catch pred paa
catch_paa_df = readRDS(file = file.path(output_folder, 'catch_paa_results.RDS'))
# Index pred paa
index_paa_df = readRDS(file = file.path(output_folder, 'index_paa_results.RDS'))
# Selex
selex_df = readRDS(file = file.path(output_folder, 'sel_results.RDS'))
# WAA re
waare_df = readRDS(file = file.path(output_folder, 'waare_results.RDS'))


# -------------------------------------------------------------------------
# Traditional approach ----------------------------------------------------
paa_gen_approach = 'traditional'

# # Convergence rates:
# n_sim = 50 # number of iterations run per scenario.
# 
# # Set EM and OM labels:
# temp = set_labels(par_df)
# temp$data_scen = factor(temp$data_scen, labels = c('Data-rich', 'Data-poor'))
# conv_df = temp %>% group_by(em_label, om_label, Ecov_sim, data_scen, caal_samp) %>% 
#             dplyr::summarise(n_conv = length(unique(maxgrad) < 1)) %>%
#             dplyr::mutate(n_tot = n_sim) %>%
#             dplyr::mutate(conv_rate = (n_conv/n_tot)*100)
# # OUTPUT TABLE WITH SCENARIO LABELS
# # TODO: add missing scenarios due to 0 convergence rate

# -------------------------------------------------------------------------
# PAR plot (for ALL scenarios):
temp = par_df %>% filter(par %in% c('logit_q', 'mean_rec_pars'), # 'log_F1', 'log_N1_pars'
                         paa_generation == paa_gen_approach) 
# Set EM and OM labels:
temp = set_labels(temp)
# Filter first X reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(par, levels = c('mean_rec_pars', 'logit_q'),
                                     labels = c(expression(bar(R)), 'Q'))) # expression(N["1,1"]) 'F[1]'

# WEm and WNP results:
p1 = make_plot_1(temp)
ggsave(filename = file.path(save_folder, paste0(paa_gen_approach, '_par', fig_type)), plot = p1,
       width = 190 , height = 140, units = 'mm', dpi = 500)

# -------------------------------------------------------------------------
# TS plot (median over years, for ALL scenarios):
temp = ts_df %>% filter(paa_generation == paa_gen_approach) %>%
        dplyr::group_by(scenario, par, data_scen, caal_samp, age_selex, re_method, 
                        method, growth_var, im) %>% 
        dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad))
# Set EM and OM labels:
temp = set_labels(temp)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(par, levels = c('SSB', 'Rec', 'F'),
                                     labels = c('SSB', 'R', 'F')))

# WEm and WNP results:
p1 = make_plot_1(temp, y_break = 0.25)
ggsave(filename = file.path(save_folder, paste0(paa_gen_approach, '_ts_agg', fig_type)), plot = p1,
       width = 190 , height = 210, units = 'mm', dpi = 500)

# -------------------------------------------------------------------------
# TS plot (by year, for ALL scenarios):

# TODO

# -------------------------------------------------------------------------
# WAA info (median over years, only for WAA and Ewaa scenarios):
temp = waa_df %>% filter(paa_generation == paa_gen_approach) %>%
            dplyr::group_by(scenario, age, data_scen, caal_samp, age_selex, re_method, 
                  method, growth_var, im) %>% 
            dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad))
# Set EM and OM labels:
temp = set_labels(temp)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(age, levels = 1:10, labels = 1:10))

# Make plot (stationary):
p6 = make_plot_1(temp, y_break = 0.1)
ggsave(filename = file.path(save_folder, paste0(paa_gen_approach, '_waa', fig_type)), plot = p6, 
       width = 190 , height = 240, units = 'mm', dpi = 500)

# -------------------------------------------------------------------------
# Pred catch CAA info:
temp = catch_paa_df %>% filter(paa_generation == paa_gen_approach) %>%
            dplyr::group_by(scenario, age, data_scen, caal_samp, age_selex, re_method, 
                  method, growth_var, im) %>%  
            dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad))
# Set EM and OM labels:
temp = set_labels(temp)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(age, levels = 1:10, labels = 1:10))

# Make plot (stationary):
p7 = make_plot_1(temp, y_break = 0.5)
ggsave(filename = file.path(save_folder, paste0(paa_gen_approach, '_catch_paa', fig_type)), plot = p7, 
       width = 190 , height = 240, units = 'mm', dpi = 500)

# -------------------------------------------------------------------------
# Pred catch IAA info:
temp = index_paa_df %>% filter(paa_generation == paa_gen_approach) %>%
  dplyr::group_by(scenario, age, data_scen, caal_samp, age_selex, re_method, 
                  method, growth_var, im) %>%  
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad))
# Set EM and OM labels:
temp = set_labels(temp)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(age, levels = 1:10, labels = 1:10))

# Make plot (stationary):
p8 = make_plot_1(temp, y_break = 0.5)
ggsave(filename = file.path(save_folder, paste0(paa_gen_approach, '_index_paa', fig_type)), plot = p8, 
       width = 190 , height = 240, units = 'mm', dpi = 500)

# -------------------------------------------------------------------------
# Selex parameters (only for LP and Ecov scenarios):
temp = selex_df %>% filter(paa_generation == paa_gen_approach) %>%
          dplyr::group_by(scenario, fleet, data_scen, caal_samp, age_selex, re_method, 
                  method, growth_var, im) %>%  
          dplyr::summarise(par1 = mean(par1), maxgrad = 1e-06) #median(maxgrad)
# Set EM and OM labels:
temp = set_labels(temp)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(fleet, levels = 1:2, labels = c('Fishery', 'Survey')))

# Make plot (stationary):
p9 = make_plot_3(temp,  par1, var_name = 'Selectivity parameter 1')
ggsave(filename = file.path(save_folder, paste0(paa_gen_approach, '_sel_par', fig_type)), plot = p9, 
       width = 190 , height = 140, units = 'mm', dpi = 500)




# -------------------------------------------------------------------------
# Stepwise approach ----------------------------------------------------
paa_gen_approach = 'stepwise'

# # Convergence rates:
# n_sim = 50 # number of iterations run per scenario.
# 
# # Set EM and OM labels:
# temp = set_labels(par_df)
# temp$data_scen = factor(temp$data_scen, labels = c('Data-rich', 'Data-poor'))
# conv_df = temp %>% group_by(em_label, om_label, Ecov_sim, data_scen, caal_samp) %>% 
#             dplyr::summarise(n_conv = length(unique(maxgrad) < 1)) %>%
#             dplyr::mutate(n_tot = n_sim) %>%
#             dplyr::mutate(conv_rate = (n_conv/n_tot)*100)
# # OUTPUT TABLE WITH SCENARIO LABELS
# # TODO: add missing scenarios due to 0 convergence rate

# -------------------------------------------------------------------------
# PAR plot (for ALL scenarios):
temp = par_df %>% filter(par %in% c('logit_q', 'mean_rec_pars'), # 'log_F1', 'log_N1_pars'
                         paa_generation == paa_gen_approach) 
# Set EM and OM labels:
temp = set_labels(temp)
# Filter first X reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(par, levels = c('mean_rec_pars', 'logit_q'),
                                     labels = c(expression(bar(R)), 'Q'))) # expression(N["1,1"]) 'F[1]'

# WEm and WNP results:
p1 = make_plot_1(temp)
ggsave(filename = file.path(save_folder, paste0(paa_gen_approach, '_par', fig_type)), plot = p1,
       width = 190 , height = 140, units = 'mm', dpi = 500)

# -------------------------------------------------------------------------
# TS plot (median over years, for ALL scenarios):
temp = ts_df %>% filter(paa_generation == paa_gen_approach) %>%
  dplyr::group_by(scenario, par, data_scen, caal_samp, age_selex, re_method, 
                  method, growth_var, im) %>% 
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad))
# Set EM and OM labels:
temp = set_labels(temp)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(par, levels = c('SSB', 'Rec', 'F'),
                                     labels = c('SSB', 'R', 'F')))

# WEm and WNP results:
p2 = make_plot_1(temp, y_break = 0.25)
ggsave(filename = file.path(save_folder, paste0(paa_gen_approach, '_ts_agg', fig_type)), plot = p2,
       width = 190 , height = 210, units = 'mm', dpi = 500)

# -------------------------------------------------------------------------
# TS plot (by year, for ALL scenarios):

# TODO

# -------------------------------------------------------------------------
# WAA info (median over years, only for WAA and Ewaa scenarios):
temp = waa_df %>% filter(paa_generation == paa_gen_approach) %>%
  dplyr::group_by(scenario, age, data_scen, caal_samp, age_selex, re_method, 
                  method, growth_var, im) %>% 
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad))
# Set EM and OM labels:
temp = set_labels(temp)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(age, levels = 1:10, labels = 1:10))

# Make plot (stationary):
p6 = make_plot_1(temp, y_break = 0.4)
ggsave(filename = file.path(save_folder, paste0(paa_gen_approach, '_waa', fig_type)), plot = p6, 
       width = 190 , height = 240, units = 'mm', dpi = 500)

# -------------------------------------------------------------------------
# Pred catch CAA info:
temp = catch_paa_df %>% filter(paa_generation == paa_gen_approach) %>%
  dplyr::group_by(scenario, age, data_scen, caal_samp, age_selex, re_method, 
                  method, growth_var, im) %>%  
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad))
# Set EM and OM labels:
temp = set_labels(temp)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(age, levels = 1:10, labels = 1:10))

# Make plot (stationary):
p7 = make_plot_1(temp, y_break = 1)
ggsave(filename = file.path(save_folder, paste0(paa_gen_approach, '_catch_paa', fig_type)), plot = p7, 
       width = 190 , height = 240, units = 'mm', dpi = 500)

# -------------------------------------------------------------------------
# Pred catch IAA info:
temp = index_paa_df %>% filter(paa_generation == paa_gen_approach) %>%
  dplyr::group_by(scenario, age, data_scen, caal_samp, age_selex, re_method, 
                  method, growth_var, im) %>%  
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad))
# Set EM and OM labels:
temp = set_labels(temp)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(age, levels = 1:10, labels = 1:10))

# Make plot (stationary):
p8 = make_plot_1(temp, y_break = 1)
ggsave(filename = file.path(save_folder, paste0(paa_gen_approach, '_index_paa', fig_type)), plot = p8, 
       width = 190 , height = 240, units = 'mm', dpi = 500)

# -------------------------------------------------------------------------
# Selex parameters (only for LP and Ecov scenarios):
temp = selex_df %>% filter(paa_generation == paa_gen_approach) %>%
  dplyr::group_by(scenario, fleet, data_scen, caal_samp, age_selex, re_method, 
                  method, growth_var, im) %>%  
  dplyr::summarise(par1 = mean(par1), maxgrad = 1e-06) #median(maxgrad)
# Set EM and OM labels:
temp = set_labels(temp)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(fleet, levels = 1:2, labels = c('Fishery', 'Survey')))

# Make plot (stationary):
p9 = make_plot_3(temp,  par1, var_name = 'Selectivity parameter 1')
ggsave(filename = file.path(save_folder, paste0(paa_gen_approach, '_sel', fig_type)), plot = p9, 
       width = 190 , height = 140, units = 'mm', dpi = 500)

# -------------------------------------------------------------------------
# WAA re parameters :
temp = waare_df %>% filter(paa_generation == paa_gen_approach) %>%
            dplyr::mutate(maxgrad = 1e-06) #median(maxgrad)
# Set EM and OM labels:
temp = set_labels(temp)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(par, levels = paste0('par', 1:4), labels = paste0('par', 1:4)))

# Make plot (stationary):
p10 = make_plot_3(temp,  est, var_name = 'Parameter value')
ggsave(filename = file.path(save_folder, paste0(paa_gen_approach, '_waare', fig_type)), plot = p10, 
       width = 190 , height = 220, units = 'mm', dpi = 500)


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Evaluate the impacts of caal samp and varying selex:
# Only for stepwise generation approach

paa_gen_approach = 'stepwise'
# Chose data scen:
this_data_scen = 'rich'


# -------------------------------------------------------------------------
# PAR plot:
temp = par_df %>% filter(par %in% c('logit_q', 'mean_rec_pars'), # 'log_F1', 'log_N1_pars'
                         paa_generation == paa_gen_approach) 
# Set EM and OM labels:
temp = set_labels(temp, selex_type = c('fixed','varying'), caal_type = c('random', 'strat'))
# only chosen data scen:
temp = temp %>% filter(data_scen == this_data_scen)

# Filter first X reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(par, levels = c('mean_rec_pars', 'logit_q'),
                                     labels = c(expression(bar(R)), 'Q'))) # expression(N["1,1"]) 'F[1]'
# Set combination type labels:
temp = temp %>% mutate(comb = paste(caal_samp, age_selex, sep = '+'))

# Make plot:
g1 = make_plot_1(temp, comb, this_pal = 'Set2', y_break = 0.3, violin_sep = 0.6, 
            leg_pos = 'bottom', leg_title = '', alpha_level = 1, col_vals = colpal2)
ggsave(filename = file.path(save_folder, paste0(paa_gen_approach, '_par_comb', fig_type)), plot = g1, 
       width = 190 , height = 140, units = 'mm', dpi = 500)


# -------------------------------------------------------------------------
# TS plot (median over years, for ALL scenarios):
temp = ts_df %>% filter(paa_generation == paa_gen_approach) %>%
  dplyr::group_by(scenario, par, data_scen, caal_samp, age_selex, re_method, 
                  method, growth_var, im) %>% 
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad))

# Set EM and OM labels:
temp = set_labels(temp, selex_type = c('fixed','varying'), caal_type = c('random', 'strat'))
# only chosen data scen:
temp = temp %>% filter(data_scen == this_data_scen)

# Filter first X reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(par, levels = c('SSB', 'Rec', 'F'),
                                     labels = c('SSB', 'R', 'F')))

# Set combination type labels:
temp = temp %>% mutate(comb = paste(caal_samp, age_selex, sep = '+'))

# Make plot:
g2 = make_plot_1(temp, comb, y_break = 0.3, violin_sep = 0.6, 
                 leg_pos = 'bottom', leg_title = '', alpha_level = 1, col_vals = colpal2)
ggsave(filename = file.path(save_folder, paste0(paa_gen_approach, '_ts_agg_comb', fig_type)), plot = g2, 
       width = 190 , height = 140, units = 'mm', dpi = 500)

# -------------------------------------------------------------------------
# WAA info (median over years, only for WAA and Ewaa scenarios):
temp = waa_df %>% filter(paa_generation == paa_gen_approach) %>%
  dplyr::group_by(scenario, age, data_scen, caal_samp, age_selex, re_method, 
                  method, growth_var, im) %>% 
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad))

# Set EM and OM labels:
temp = set_labels(temp, selex_type = c('fixed','varying'), caal_type = c('random', 'strat'))
# only chosen data scen:
temp = temp %>% filter(data_scen == this_data_scen)

# Filter first X reps:
temp = filter_iter(temp)

# Set par labels:
temp = temp %>% mutate(par2 = factor(age, levels = 1:10, labels = 1:10))

# Set combination type labels:
temp = temp %>% mutate(comb = paste(caal_samp, age_selex, sep = '+'))

# Make plot:
g2 = make_plot_1(temp, comb, y_break = 0.5, violin_sep = 0.6, 
                 leg_pos = 'bottom', leg_title = '', alpha_level = 1, col_vals = colpal2)
ggsave(filename = file.path(save_folder, paste0(paa_gen_approach, '_waa_comb', fig_type)), plot = g2, 
       width = 190 , height = 240, units = 'mm', dpi = 500)

# -------------------------------------------------------------------------
# Pred catch CAA info:
temp = catch_paa_df %>% filter(paa_generation == paa_gen_approach) %>%
  dplyr::group_by(scenario, age, data_scen, caal_samp, age_selex, re_method, 
                  method, growth_var, im) %>%  
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad))

# Set EM and OM labels:
temp = set_labels(temp, selex_type = c('fixed','varying'), caal_type = c('random', 'strat'))
# only chosen data scen:
temp = temp %>% filter(data_scen == this_data_scen)

# Filter first X reps:
temp = filter_iter(temp)

# Set par labels:
temp = temp %>% mutate(par2 = factor(age, levels = 1:10, labels = 1:10))

# Set combination type labels:
temp = temp %>% mutate(comb = paste(caal_samp, age_selex, sep = '+'))

# Make plot:
g2 = make_plot_1(temp, comb, y_break = 1, violin_sep = 0.6, 
                 leg_pos = 'bottom', leg_title = '', alpha_level = 1, col_vals = colpal2)
ggsave(filename = file.path(save_folder, paste0(paa_gen_approach, '_catch_paa_comb', fig_type)), 
       plot = g2, width = 190 , height = 240, units = 'mm', dpi = 500)

# -------------------------------------------------------------------------
# Pred catch IAA info:
temp = index_paa_df %>% filter(paa_generation == paa_gen_approach) %>%
  dplyr::group_by(scenario, age, data_scen, caal_samp, age_selex, re_method, 
                  method, growth_var, im) %>%  
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad))

# Set EM and OM labels:
temp = set_labels(temp, selex_type = c('fixed','varying'), caal_type = c('random', 'strat'))
# only chosen data scen:
temp = temp %>% filter(data_scen == this_data_scen)

# Filter first X reps:
temp = filter_iter(temp)

# Set par labels:
temp = temp %>% mutate(par2 = factor(age, levels = 1:10, labels = 1:10))

# Set combination type labels:
temp = temp %>% mutate(comb = paste(caal_samp, age_selex, sep = '+'))

# Make plot:
g2 = make_plot_1(temp, comb, y_break = 1, violin_sep = 0.6, 
                 leg_pos = 'bottom', leg_title = '', alpha_level = 1, col_vals = colpal2)
ggsave(filename = file.path(save_folder, paste0(paa_gen_approach, '_index_paa_comb', fig_type)), 
       plot = g2, width = 190 , height = 240, units = 'mm', dpi = 500)

# -------------------------------------------------------------------------
# Selex parameters (only for LP and Ecov scenarios):
temp = selex_df %>% filter(paa_generation == paa_gen_approach) %>%
  dplyr::group_by(scenario, fleet, data_scen, caal_samp, age_selex, re_method, 
                  method, growth_var, im) %>%  
  dplyr::summarise(par1 = mean(par1), maxgrad = 1e-06) #median(maxgrad)

# Set EM and OM labels:
temp = set_labels(temp, selex_type = c('fixed','varying'), caal_type = c('random', 'strat'))
# only chosen data scen:
temp = temp %>% filter(data_scen == this_data_scen)

# Filter first X reps:
temp = filter_iter(temp)

# Set par labels:
temp = temp %>% mutate(par2 = factor(fleet, levels = 1:2, labels = c('Fishery', 'Survey')))

# Set combination type labels:
temp = temp %>% mutate(comb = paste(caal_samp, age_selex, sep = '+'))

# Make plot (stationary):
g9 = make_plot_3(temp,  par1, comb, violin_sep = 0.6, 
                 leg_pos = 'bottom', leg_title = '', alpha_level = 1, col_vals = colpal2,
                 var_name = 'Selectivity parameter 1')
ggsave(filename = file.path(save_folder, paste0(paa_gen_approach, '_sel_comb', fig_type)), plot = g9, 
       width = 190 , height = 140, units = 'mm', dpi = 500)

# -------------------------------------------------------------------------
# WAA re parameters :
temp = waare_df %>% filter(paa_generation == paa_gen_approach) %>%
  dplyr::mutate(maxgrad = 1e-06) #median(maxgrad)

# Set EM and OM labels:
temp = set_labels(temp, selex_type = c('fixed','varying'), caal_type = c('random', 'strat'))
# only chosen data scen:
temp = temp %>% filter(data_scen == this_data_scen)

# Filter first X reps:
temp = filter_iter(temp)

# Set par labels:
temp = temp %>% mutate(par2 = factor(par, levels = paste0('par', 1:4), labels = paste0('par', 1:4)))

# Set combination type labels:
temp = temp %>% mutate(comb = paste(caal_samp, age_selex, sep = '+'))

# Make plot (stationary):
g10 = make_plot_3(temp,  est, comb, violin_sep = 0.6, 
                 leg_pos = 'bottom', leg_title = '', alpha_level = 1, col_vals = colpal2,
                 var_name = 'Parameter value')
ggsave(filename = file.path(save_folder, paste0(paa_gen_approach, '_waare_comb', fig_type)), plot = g10, 
       width = 190 , height = 140, units = 'mm', dpi = 500)
