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
colpal1 = viridisLite::viridis(n = 6)[c(1,4)]
colpal2 = brewer.pal(n = 9, name = 'YlGnBu')[c(1,5,9)]

# Convergence level:
max_grad = 1e-04

# RE in %?
re_mult = 100

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
# WAA re
selre_df = readRDS(file = file.path(output_folder, 'selre_results.RDS'))

# -------------------------------------------------------------------------
# PAR plot
# Select parameters:
temp = par_df %>% dplyr::filter(par %in% c('logit_q', 'mean_rec_pars', 'log_NAA_sigma')) # 'log_F1', 'log_N1_pars'

# Set EM and OM labels:
tmp_df = set_labels(temp, conv_level = max_grad)
# Filter first X reps:
tmp_df = filter_iter(tmp_df)
# Set par labels:
tmp_df = tmp_df %>% mutate(par2 = factor(par, levels = c('mean_rec_pars', 'logit_q', 'log_NAA_sigma'),
                                         labels = c(expression(R[0]), 'Q', expression(sigma[R]))), # expression(N["1,1"]) 'F[1]'
                           paa_generation = factor(paa_generation, levels = c('traditional', 'stepwise'),
                                                   labels = c('Traditional', 'Stepwise'))) 

# Prepare data for geom linerage plot:
plot_dat = tmp_df %>% group_by(em_label, par2, om_label, paa_generation, Ecov_sim, 
                               caal_samp, age_selex) %>%
  dplyr::summarise(q025 = quantile(rel_error, probs = 0.025)*re_mult, 
                   q50 = quantile(rel_error, probs = 0.5)*re_mult,
                   q975 = quantile(rel_error, probs = 0.975)*re_mult)

# Make plot:
p1 = make_plot_1b(plot_dat, paa_generation, y_break = 0.2, violin_sep = 0.4, 
                  leg_pos = 'bottom', leg_title = '', alpha_level = 1, col_vals = colpal1)
ggsave(filename = file.path(save_folder, paste0(paste('main', 'par', sep = '-'), fig_type)), 
       plot = p1, width = img_width, height = 150, units = 'mm', dpi = img_res)


# -------------------------------------------------------------------------
# average TS plot
temp = ts_df %>% dplyr::group_by(scenario, par, paa_generation, data_scen, Ecov_sim, 
                                  caal_samp, age_selex, re_method, method, growth_var, im) %>% 
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad)) # median over the years

# Set EM and OM labels:
tmp_df = set_labels(temp, conv_level = max_grad)
# Filter first X reps:
tmp_df = filter_iter(tmp_df)
# Set par labels:
tmp_df = tmp_df %>% mutate(par2 = factor(par, levels = c('SSB', 'Rec', 'F'),
                                     labels = c('SSB', 'R', 'F')), # expression(N["1,1"]) 'F[1]'
                       paa_generation = factor(paa_generation, levels = c('traditional', 'stepwise'),
                                               labels = c('Traditional', 'Stepwise'))) 

# Prepare data for geom linerage plot:
plot_dat = tmp_df %>% group_by(em_label, par2, om_label, paa_generation, Ecov_sim, 
                               caal_samp, age_selex) %>%
  dplyr::summarise(q025 = quantile(rel_error, probs = 0.025)*re_mult, 
                   q50 = quantile(rel_error, probs = 0.5)*re_mult,
                   q975 = quantile(rel_error, probs = 0.975)*re_mult)

# Make plot:
p1 = make_plot_1b(plot_dat, paa_generation, y_break = 0.2, violin_sep = 0.4, 
                  leg_pos = 'bottom', leg_title = '', alpha_level = 1, col_vals = colpal1)
ggsave(filename = file.path(save_folder, paste0(paste('main', 'avg-ts', sep = '-'), fig_type)), 
       plot = p1, width = img_width, height = 150, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# TS plot (by year, for ALL scenarios):
# Do it variable by variable: F, R, SSB
ts_folder_plot = file.path(save_folder, 'ts_plots')
dir.create(ts_folder_plot, showWarnings = FALSE)

# Sort data:
temp = ts_df %>% dplyr::group_by(paa_generation, scenario, par, year, data_scen, caal_samp, age_selex, re_method, 
                  Ecov_sim, method, growth_var, im) %>% 
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad))

# Select filter:
sel_var = 'SSB' # Rec, SSB, F

######
# Set EM and OM labels:
temp2 = set_labels(temp, conv_level = max_grad)
# Filter first 100 reps:
temp2 = filter_iter(temp2)
# Select variable to plot:
temp2 = temp2 %>% dplyr::filter(par == sel_var)
# Make em label:
temp2$em_label2 = factor(temp2$em_label, labels = c("WEm", expression(WNP*"-"*iid), 
                                                    expression(WNP*"-"*2*"D"),
                                                    expression(WNP*"-"*3*"D")))
temp2 = temp2 %>% mutate(paa_generation = factor(paa_generation, levels = c('traditional', 'stepwise'),
                                                labels = c('Traditional', 'Stepwise'))) 

# Prepare data for geom linerage plot:
plot_dat = temp2 %>% group_by(paa_generation, em_label2, year, om_label) %>%
  dplyr::summarise(q025 = quantile(rel_error, probs = 0.025)*re_mult, 
                   q50 = quantile(rel_error, probs = 0.5)*re_mult,
                   q975 = quantile(rel_error, probs = 0.975)*re_mult)

# Make plot:
p1 = ggplot(plot_dat, aes(x = year, y = q50)) +
  geom_line(aes(color = paa_generation)) +
  geom_ribbon(aes(ymin = q025, ymax = q975, fill = paa_generation), alpha = 0.3) +
  geom_hline(yintercept=0, color=1, linetype='dashed') +
  # coord_cartesian(ylim = 50*c(-1, 1)) +
  scale_color_manual(values = colpal1) +
  scale_fill_manual(values = colpal1) +
  ylab('Relative error (%)') + xlab('Simulated year') +
  theme(legend.position = 'bottom',
        strip.background = element_rect(fill="white")) +
  facet_grid(em_label2 ~ om_label, labeller = 'label_parsed') +
  guides(colour=guide_legend(title=NULL), fill=guide_legend(title=NULL))
ggsave(filename = file.path(ts_folder_plot, paste0(paste('main', 'ts', sel_var, sep = '-'), fig_type)), 
       plot = p1, width = img_width, height = 210, units = 'mm', dpi = img_res)


# -------------------------------------------------------------------------
# WAA plots:
temp = waa_df %>% dplyr::group_by(scenario, age, paa_generation, data_scen, Ecov_sim, 
                                  caal_samp, age_selex, re_method, method, growth_var, im) %>% 
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad)) # median over the years

# Set EM and OM labels:
tmp_df = set_labels(temp, conv_level = max_grad)
# Filter first X reps:
tmp_df = filter_iter(tmp_df)
# Set par labels:
tmp_df = tmp_df %>% mutate(par2 = factor(age, levels = 1:10, labels = 1:10),
                           paa_generation = factor(paa_generation, levels = c('traditional', 'stepwise'),
                                                   labels = c('Traditional', 'Stepwise'))) 

# Prepare data for geom linerage plot:
plot_dat = tmp_df %>% group_by(em_label, par2, age, om_label, paa_generation, Ecov_sim, 
                               caal_samp, age_selex) %>%
  dplyr::summarise(q025 = quantile(rel_error, probs = 0.025)*re_mult, 
                   q50 = quantile(rel_error, probs = 0.5)*re_mult,
                   q975 = quantile(rel_error, probs = 0.975)*re_mult)

# Make plot:
p1 = make_plot_1b(plot_dat, paa_generation, y_break = 0.2, violin_sep = 0.4, 
                  leg_pos = 'bottom', leg_title = '', alpha_level = 1, col_vals = colpal1)
ggsave(filename = file.path(save_folder, paste0(paste('main', 'waa', sep = '-'), fig_type)), 
       plot = p1, width = img_width, height = 150, units = 'mm', dpi = img_res)


# -------------------------------------------------------------------------
# CAA plots:
temp = catch_paa_df %>% dplyr::group_by(scenario, age, paa_generation, data_scen, Ecov_sim, 
                                  caal_samp, age_selex, re_method, method, growth_var, im) %>% 
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad)) # median over the years

# Set EM and OM labels:
tmp_df = set_labels(temp, conv_level = max_grad)
# Filter first X reps:
tmp_df = filter_iter(tmp_df)
# Set par labels:
tmp_df = tmp_df %>% mutate(par2 = factor(age, levels = 1:10, labels = 1:10),
                           paa_generation = factor(paa_generation, levels = c('traditional', 'stepwise'),
                                                   labels = c('Traditional', 'Stepwise'))) 

# Prepare data for geom linerage plot:
plot_dat = tmp_df %>% group_by(em_label, par2, age, om_label, paa_generation, Ecov_sim, 
                               caal_samp, age_selex) %>%
  dplyr::summarise(q025 = quantile(rel_error, probs = 0.025)*re_mult, 
                   q50 = quantile(rel_error, probs = 0.5)*re_mult,
                   q975 = quantile(rel_error, probs = 0.975)*re_mult)

# Make plot:
p1 = make_plot_1b(plot_dat, paa_generation, y_break = 0.2, violin_sep = 0.4, 
                  leg_pos = 'bottom', leg_title = '', alpha_level = 1, col_vals = colpal1)
ggsave(filename = file.path(save_folder, paste0(paste('main', 'caa', sep = '-'), fig_type)), 
       plot = p1, width = img_width, height = 150, units = 'mm', dpi = img_res)


# -------------------------------------------------------------------------
# IAA plots:
temp = index_paa_df %>% dplyr::group_by(scenario, age, paa_generation, data_scen, Ecov_sim, 
                                        caal_samp, age_selex, re_method, method, growth_var, im) %>% 
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad)) # median over the years

# Set EM and OM labels:
tmp_df = set_labels(temp, conv_level = max_grad)
# Filter first X reps:
tmp_df = filter_iter(tmp_df)
# Set par labels:
tmp_df = tmp_df %>% mutate(par2 = factor(age, levels = 1:10, labels = 1:10),
                           paa_generation = factor(paa_generation, levels = c('traditional', 'stepwise'),
                                                   labels = c('Traditional', 'Stepwise'))) 

# Prepare data for geom linerage plot:
plot_dat = tmp_df %>% group_by(em_label, par2, age, om_label, paa_generation, Ecov_sim, 
                               caal_samp, age_selex) %>%
  dplyr::summarise(q025 = quantile(rel_error, probs = 0.025)*re_mult, 
                   q50 = quantile(rel_error, probs = 0.5)*re_mult,
                   q975 = quantile(rel_error, probs = 0.975)*re_mult)

# Make plot:
p1 = make_plot_1b(plot_dat, paa_generation, y_break = 0.2, violin_sep = 0.4, 
                  leg_pos = 'bottom', leg_title = '', alpha_level = 1, col_vals = colpal1)
ggsave(filename = file.path(save_folder, paste0(paste('main', 'iaa', sep = '-'), fig_type)), 
       plot = p1, width = img_width, height = 150, units = 'mm', dpi = img_res)


# -------------------------------------------------------------------------
# WAA RE parameters:
temp = waare_df
# Set EM and OM labels:
temp = set_labels(temp, conv_level = max_grad)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(paa_generation = factor(paa_generation, levels = c('traditional', 'stepwise'),
                                                   labels = c('Traditional', 'Stepwise'))) 

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
plot_dat = temp %>% group_by(em_label, par2, paa_generation, om_label, Ecov_sim, data_scen, caal_samp, age_selex) %>%
  dplyr::summarise(q025 = quantile(est, probs = 0.025, na.rm = T), q50 = quantile(est, probs = 0.5, na.rm = T),
                   q975 = quantile(est, probs = 0.975, na.rm = T))
plot_dat = plot_dat %>% dplyr::filter(!(em_label == 'WEm'))

# Make plot:
p1 = make_plot_3b(plot_dat, paa_generation, violin_sep = 0.4, 
                  leg_pos = 'bottom', leg_title = '', col_vals = colpal1,
                  var_name = 'Value')
ggsave(filename = file.path(save_folder, paste0(paste('main', 'waare', sep = '-'), fig_type)), 
       plot = p1, width = img_width, height = 150, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# Selex RE parameters:
temp = selre_df %>% dplyr::filter(growth_var > 0)
# Set EM and OM labels:
temp = set_labels(temp, selex_type = 'varying', conv_level = max_grad)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(paa_generation = factor(paa_generation, levels = c('traditional', 'stepwise'),
                                               labels = c('Traditional', 'Stepwise'))) 

# Set par labels:
temp = temp %>% mutate(par2 = factor(par, levels = paste0('par', 1:2), 
                                     labels = c(expression(sigma[beta[1*","*f]]), expression(sigma[beta[1*","*s]]))))

# Prepare data for geom linerage plot:
plot_dat = temp %>% group_by(em_label, par2, paa_generation, om_label, Ecov_sim, data_scen, caal_samp, age_selex) %>%
  dplyr::summarise(q025 = quantile(est, probs = 0.025, na.rm = T), q50 = quantile(est, probs = 0.5, na.rm = T),
                   q975 = quantile(est, probs = 0.975, na.rm = T))

# Make plot:
p1 = make_plot_3b(plot_dat, paa_generation, violin_sep = 0.4, 
                  leg_pos = 'bottom', leg_title = '', col_vals = colpal1,
                  var_name = 'Value')
ggsave(filename = file.path(save_folder, paste0(paste('main', 'selre', sep = '-'), fig_type)), 
       plot = p1, width = img_width, height = 130, units = 'mm', dpi = img_res)
