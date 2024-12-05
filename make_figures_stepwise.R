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
img_res = 400
img_width = 170

# Color palettes
colpal1 = brewer.pal(n = 8, name = 'Set1')[1:2] # for data poor rich plots
colpal2 = c(brewer.pal(n = 9, name = 'Blues')[c(9,7,5,3)], brewer.pal(n = 9, name = 'Reds')[c(9,7,5,3)])

# Convergence level:
max_grad = 1e-04

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
# Stepwise approach ----------------------------------------------------
paa_gen_approach = 'stepwise'

# -------------------------------------------------------------------------

# Convergence rates:
n_sim = 110 # number of iterations run per scenario.

# Set EM and OM labels:
paa_gen_approach = paa_gen_approach
tmp_df = par_df %>% dplyr::filter(paa_generation == paa_gen_approach)
temp = set_labels(tmp_df, caal_type = c('random', 'strat'), selex_type = c('fixed', 'varying'), remove_conv = FALSE)
conv_df = temp %>% group_by(em_label, om_label, data_scen, caal_samp, age_selex) %>%
            dplyr::summarise(n_conv = length(unique(maxgrad) < max_grad)) %>%
            dplyr::mutate(n_tot = n_sim) %>%
            dplyr::mutate(conv_rate = (n_conv/n_tot)*100)
# OUTPUT TABLE WITH SCENARIO LABELS
plot_data = conv_df %>% mutate(y_label = paste(caal_samp, age_selex, sep = '/'))
c1 = ggplot(data = plot_data, aes(x = em_label, y = data_scen, fill = conv_rate)) +
  geom_tile() +
  viridis::scale_fill_viridis(discrete = FALSE) +
  xlab(NULL) + ylab(NULL) +
  theme(legend.position = 'bottom',
        strip.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 9.3),
        axis.text.y = element_text(size = 10)) +
  labs(fill = 'Convergence rate (%)') +
  facet_grid(y_label ~ om_label, labeller = 'label_parsed', scales = 'free_y')
ggsave(filename = file.path(save_folder, paste0(paa_gen_approach, '_convrate', fig_type)),
       plot = c1, width = img_width , height = 190, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# PAR plot:
this_age_selex = 'fixed' # fixed or varying
this_caal_samp = c('random', 'strat') # random or strat
temp = par_df %>% filter(par %in% c('logit_q', 'mean_rec_pars'), # 'log_F1', 'log_N1_pars'
                         paa_generation == paa_gen_approach) 
# Set EM and OM labels:
temp = set_labels(temp, selex_type = this_age_selex, caal_type = this_caal_samp, conv_level = max_grad)
# Filter first X reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(par, levels = c('mean_rec_pars', 'logit_q'),
                                     labels = c(expression(bar(R)), 'Q'))) # expression(N["1,1"]) 'F[1]'

# Prepare data for geom linerage plot:
plot_dat = temp %>% group_by(em_label, par2, om_label, data_scen, caal_samp, age_selex) %>%
  dplyr::summarise(q025 = quantile(rel_error, probs = 0.025), q50 = quantile(rel_error, probs = 0.5),
                   q975 = quantile(rel_error, probs = 0.975))

# WEm and WNP results:
# p1 = make_plot_1(temp, comb, y_break = 0.3, violin_sep = 0.7, 
#                  leg_pos = 'bottom', leg_title = '', alpha_level = 0.75, col_vals = colpal1)
p1 = make_plot_1c(plot_dat, data_scen, caal_samp, y_break = 0.3, violin_sep = 0.6,  min_alpha = 0.35,
                  leg_pos = 'bottom', leg_title = '', leg_title2 = '', col_vals = colpal1)
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, this_age_selex, 'par', sep = '-'), fig_type)), 
       plot = p1, width = img_width , height = 140, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# TS plot:
this_age_selex = 'fixed' # fixed or varying
this_caal_samp = c('random', 'strat') # random or strat
temp = ts_df %>% filter(paa_generation == paa_gen_approach) %>%
  dplyr::group_by(scenario, par, data_scen, caal_samp, age_selex, re_method, 
                  method, growth_var, im) %>% 
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad))
# Set EM and OM labels:
temp = set_labels(temp, selex_type = this_age_selex, caal_type = this_caal_samp, conv_level = max_grad)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(par, levels = c('SSB', 'Rec', 'F'),
                                     labels = c('SSB', 'R', 'F')))

# Prepare data for geom linerage plot:
plot_dat = temp %>% group_by(em_label, par2, om_label, data_scen, caal_samp, age_selex) %>%
  dplyr::summarise(q025 = quantile(rel_error, probs = 0.025), q50 = quantile(rel_error, probs = 0.5),
                   q975 = quantile(rel_error, probs = 0.975))

# WEm and WNP results:
# p2 = make_plot_1(temp, comb, y_break = 0.2, violin_sep = 0.7, 
#                  leg_pos = 'bottom', leg_title = '', alpha_level = 1, col_vals = colpal2)
p2 = make_plot_1c(plot_dat, data_scen, caal_samp, y_break = 0.3, violin_sep = 0.6,  min_alpha = 0.35,
                  leg_pos = 'bottom', leg_title = '', leg_title2 = '', col_vals = colpal1)
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, this_age_selex, 'ts', sep = '-'), fig_type)), plot = p2,
       width = img_width , height = 210, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# TS plot (by year, for ALL scenarios):

# TODO

# -------------------------------------------------------------------------
# WAA info (median over years, only for WAA and Ewaa scenarios):
this_age_selex = 'fixed' # fixed or varying
this_caal_samp = c('random', 'strat') # random or strat
temp = waa_df %>% filter(paa_generation == paa_gen_approach) %>%
  dplyr::group_by(scenario, age, data_scen, caal_samp, age_selex, re_method, 
                  method, growth_var, im) %>% 
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad))
# Set EM and OM labels:
temp = set_labels(temp, selex_type = this_age_selex, caal_type = this_caal_samp, conv_level = max_grad)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(age, levels = 1:10, labels = 1:10))

# Prepare data for geom linerage plot:
plot_dat = temp %>% group_by(em_label, par2, om_label, data_scen, caal_samp, age_selex) %>%
  dplyr::summarise(q025 = quantile(rel_error, probs = 0.025), q50 = quantile(rel_error, probs = 0.5),
                   q975 = quantile(rel_error, probs = 0.975))

# Make plot (stationary):
# p6 = make_plot_1(temp, comb, y_break = 0.5, violin_sep = 0.7, 
#                  leg_pos = 'bottom', leg_title = '', alpha_level = 1, col_vals = colpal2)
p6 = make_plot_1c(plot_dat, data_scen, caal_samp, y_break = 0.5, violin_sep = 0.6,  min_alpha = 0.35,
                  leg_pos = 'bottom', leg_title = '', leg_title2 = '', col_vals = colpal1)
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, this_age_selex, 'waa', sep = '-'), fig_type)), plot = p6, 
       width = img_width , height = 240, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# Pred catch CAA info:
this_age_selex = 'fixed' # fixed or varying
this_caal_samp = c('random', 'strat') # random or strat
temp = catch_paa_df %>% filter(paa_generation == paa_gen_approach) %>%
  dplyr::group_by(scenario, age, data_scen, caal_samp, age_selex, re_method, 
                  method, growth_var, im) %>%  
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad))
# Set EM and OM labels:
temp = set_labels(temp, selex_type = this_age_selex, caal_type = this_caal_samp, conv_level = max_grad)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(age, levels = 1:10, labels = 1:10))

# Prepare data for geom linerage plot:
plot_dat = temp %>% group_by(em_label, par2, om_label, data_scen, caal_samp, age_selex) %>%
  dplyr::summarise(q025 = quantile(rel_error, probs = 0.025), q50 = quantile(rel_error, probs = 0.5),
                   q975 = quantile(rel_error, probs = 0.975))

# Make plot (stationary):
# p7 = make_plot_1(temp, comb, y_break = 0.5, violin_sep = 0.7, 
#                  leg_pos = 'bottom', leg_title = '', alpha_level = 1, col_vals = colpal2)
p7 = make_plot_1c(plot_dat, data_scen, caal_samp, y_break = 1, violin_sep = 0.6,  min_alpha = 0.35,
                  leg_pos = 'bottom', leg_title = '', leg_title2 = '', col_vals = colpal1)
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, this_age_selex, 'caa', sep = '-'), fig_type)), plot = p7, 
       width = img_width , height = 240, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# Pred catch IAA info:
this_age_selex = 'fixed' # fixed or varying
this_caal_samp = c('random', 'strat') # random or strat
temp = index_paa_df %>% filter(paa_generation == paa_gen_approach) %>%
  dplyr::group_by(scenario, age, data_scen, caal_samp, age_selex, re_method, 
                  method, growth_var, im) %>%  
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad))
# Set EM and OM labels:
temp = set_labels(temp, selex_type = this_age_selex, caal_type = this_caal_samp, conv_level = max_grad)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(age, levels = 1:10, labels = 1:10))

# Prepare data for geom linerage plot:
plot_dat = temp %>% group_by(em_label, par2, om_label, data_scen, caal_samp, age_selex) %>%
  dplyr::summarise(q025 = quantile(rel_error, probs = 0.025), q50 = quantile(rel_error, probs = 0.5),
                   q975 = quantile(rel_error, probs = 0.975))

# Make plot (stationary):
# p8 = make_plot_1(temp, comb, y_break = 0.5, violin_sep = 0.7, 
#                  leg_pos = 'bottom', leg_title = '', alpha_level = 1, col_vals = colpal2)
p8 = make_plot_1c(plot_dat, data_scen, caal_samp, y_break = 0.4, violin_sep = 0.6,  min_alpha = 0.35,
                  leg_pos = 'bottom', leg_title = '', leg_title2 = '', col_vals = colpal1)
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, this_age_selex, 'iaa', sep = '-'), fig_type)), plot = p8, 
       width = img_width , height = 240, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# Selex parameters (only for LP and Ecov scenarios):
this_age_selex = 'fixed' # fixed or varying
this_caal_samp = c('random', 'strat') # random or strat
temp = selex_df %>% filter(paa_generation == paa_gen_approach) %>%
  dplyr::group_by(scenario, fleet, data_scen, caal_samp, age_selex, re_method, 
                  method, growth_var, im) %>%  
  dplyr::summarise(par1 = mean(par1), maxgrad = median(maxgrad)) #median(maxgrad)
# Set EM and OM labels:
temp = set_labels(temp, selex_type = this_age_selex, caal_type = this_caal_samp, conv_level = max_grad)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(fleet, levels = 1:2, labels = c('Fishery', 'Survey')))

# Prepare data for geom linerage plot:
plot_dat = temp %>% group_by(em_label, par2, om_label, data_scen, caal_samp, age_selex) %>%
  dplyr::summarise(q025 = quantile(par1, probs = 0.025), q50 = quantile(par1, probs = 0.5),
                   q975 = quantile(par1, probs = 0.975))

# Make plot (stationary):
# p9 = make_plot_3(temp,  par1, comb, violin_sep = 0.6, 
#                  leg_pos = 'bottom', leg_title = '', alpha_level = 1, col_vals = colpal2,
#                  var_name = expression('Selectivity parameter '~beta[1]))
p9 = make_plot_3c(plot_dat, data_scen, caal_samp, violin_sep = 0.4, min_alpha = 0.35,
                  leg_pos = 'bottom', leg_title = '', leg_title2 = '', col_vals = colpal1,
                  var_name = expression('Selectivity parameter '~beta[1]))
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, this_age_selex, 'sel', sep = '-'), fig_type)), plot = p9, 
       width = img_width , height = 140, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# WAA re parameters :
this_age_selex = 'fixed' # fixed or varying
this_caal_samp = c('random', 'strat') # random or strat
temp = waare_df %>% filter(paa_generation == paa_gen_approach)
# Set EM and OM labels:
temp = set_labels(temp, selex_type = this_age_selex, caal_type = this_caal_samp, conv_level = max_grad)
# Filter first 100 reps:
temp = filter_iter(temp)

# Specify NA when meaningless parameter value:
temp = temp %>% mutate(est = if_else(method == 'WEm', NA, est))
temp = temp %>% mutate(est = if_else(method == 'WNP' & re_method == 'iid' & par %in% c('par2', 'par3', 'par4'), NA, est))
temp = temp %>% mutate(est = if_else(method == 'WNP' & re_method == '2D' & par %in% c('par4'), NA, est))
# Exp(sigma)
temp = temp %>% mutate(est = if_else(par == 'par1', exp(est), est))

# Set par labels:
temp = temp %>% mutate(par2 = factor(par, levels = paste0('par', 1:4), 
                                     labels = c(expression(sigma[w]), expression(rho[age]),
                                                expression(rho[year]), expression(rho[cohort]))))

# Prepare data for geom linerage plot:
plot_dat = temp %>% group_by(em_label, par2, om_label, data_scen, caal_samp, age_selex) %>%
  dplyr::summarise(q025 = quantile(est, probs = 0.025, na.rm = T), q50 = quantile(est, probs = 0.5, na.rm = T),
                   q975 = quantile(est, probs = 0.975, na.rm = T))

# Make plot (stationary):
# p10 = make_plot_3c(temp,  est, comb, violin_sep = 0.6, 
#                   leg_pos = 'bottom', leg_title = '', alpha_level = 1, col_vals = colpal2,
#                   var_name = 'Parameter')
p10 = make_plot_3c(plot_dat, data_scen, caal_samp, violin_sep = 0.4, min_alpha = 0.35,
                  leg_pos = 'bottom', leg_title = '', leg_title2 = '', col_vals = colpal1,
                  var_name = 'Parameter value')
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, this_age_selex, 'waare', sep = '-'), fig_type)), plot = p10, 
       width = img_width , height = 200, units = 'mm', dpi = img_res)
