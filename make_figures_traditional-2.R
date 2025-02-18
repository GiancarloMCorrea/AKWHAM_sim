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

# RE in %?
re_mult = 100

# Color palettes
colpal1 = brewer.pal(n = 8, name = 'Dark2')[1:2] # for data poor rich plots
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
# Traditional approach ----------------------------------------------------
paa_gen_approach = 'traditional'

# -------------------------------------------------------------------------

# Convergence rates:
n_sim = 20 # number of iterations run per scenario.

# Set EM and OM labels:
tmp_df = par_df %>% dplyr::filter(paa_generation == paa_gen_approach)
temp = set_labels(tmp_df, caal_type = c('random', 'strat'),  
                  selex_type = c('fixed', 'varying'),
                  ecov_type = c('stationary', 'trend'),
                  remove_conv = FALSE)
conv_df = temp %>% group_by(em_label, om_label, Ecov_sim, caal_samp, age_selex) %>%
  dplyr::summarise(n_conv = length(unique(maxgrad) < max_grad)) %>%
  dplyr::mutate(n_tot = n_sim) %>%
  dplyr::mutate(conv_rate = (n_conv/n_tot)*100)
# OUTPUT TABLE WITH SCENARIO LABELS
plot_data = conv_df %>% mutate(y_label = age_selex)
c1 = ggplot(data = plot_data, aes(x = em_label, y = Ecov_sim, fill = conv_rate)) +
  geom_tile() +
  viridis::scale_fill_viridis(discrete = FALSE) +
  xlab(NULL) + ylab(NULL) +
  theme(legend.position = 'bottom',
        strip.text = element_text(size = 10),
        strip.background = element_rect(fill="white"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 9.3),
        axis.text.y = element_text(size = 10)) +
  labs(fill = 'Convergence rate (%)') +
  facet_grid(y_label ~ om_label, labeller = 'label_parsed', scales = 'free_y')
ggsave(filename = file.path(save_folder, paste0(paa_gen_approach, '_convrate', fig_type)),
       plot = c1, width = img_width , height = 110, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# Heatmap to analyze effects of Ecov sim:
# Traditional approach
paa_gen_approach = 'traditional'

# -------------------------------------------------------------------------
# PAR plot:
# Prepare data
temp = par_df %>% dplyr::filter(par %in% c('logit_q', 'mean_rec_pars', 'log_NAA_sigma'),
                                growth_var > 0)

tmp_df = temp %>% dplyr::filter(paa_generation == paa_gen_approach)
# Set EM and OM labels:
tmp_df = set_labels(tmp_df, ecov_type = c('stationary', 'trend'), conv_level = max_grad)
# Filter first X reps:
tmp_df = filter_iter(tmp_df)
# Set par labels:
tmp_df = tmp_df %>% mutate(par2 = factor(par, levels = c('mean_rec_pars', 'logit_q', 'log_NAA_sigma'),
                                         labels = c(expression(R[0]), 'Q', expression(sigma[R]))) # expression(N["1,1"]) 'F[1]'
)

# Plot:
plot_dat = tmp_df %>% group_by(em_label, par2, om_label, Ecov_sim, caal_samp, age_selex) %>%
  dplyr::summarise(bias = round(quantile(rel_error, probs = 0.5)*re_mult),
                   precision = round(sd(rel_error)*re_mult))
plot_dat = plot_dat %>% mutate(box_label = paste0(bias, '(', precision, ')'))

p3 = make_heatmap(df = plot_dat, this_factor = bias, this_label = box_label, y_label = Ecov_sim)
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, 'par', sep = '-'), fig_type)), 
       plot = p3, width = img_width, height = 160, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# avg TS plot:
# Prepare data
temp = ts_df %>% dplyr::group_by(scenario, par, paa_generation, data_scen, Ecov_sim, 
                                  caal_samp, age_selex, re_method, method, growth_var, im) %>% 
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad)) # median over the years
temp = temp %>% dplyr::filter(growth_var > 0)

tmp_df = temp %>% dplyr::filter(paa_generation == paa_gen_approach)
# Set EM and OM labels:
tmp_df = set_labels(tmp_df, ecov_type = c('stationary', 'trend'), conv_level = max_grad)
# Filter first X reps:
tmp_df = filter_iter(tmp_df)
# Set par labels:
tmp_df = tmp_df %>% mutate(par2 = factor(par, levels = c('SSB', 'Rec', 'F'),
                                         labels = c('SSB', 'R', 'F')) # expression(N["1,1"]) 'F[1]'
) 

# Plot:
plot_dat = tmp_df %>% group_by(em_label, par2, om_label, Ecov_sim, caal_samp, age_selex) %>%
  dplyr::summarise(bias = round(quantile(rel_error, probs = 0.5)*re_mult),
                   precision = round(sd(rel_error)*re_mult))
plot_dat = plot_dat %>% mutate(box_label = paste0(bias, '(', precision, ')'))

p3 = make_heatmap(df = plot_dat, this_factor = bias, this_label = box_label, y_label = Ecov_sim)
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, 'avg-ts', sep = '-'), fig_type)), 
       plot = p3, width = img_width, height = 160, units = 'mm', dpi = img_res)


# -------------------------------------------------------------------------
# WAA info:
temp = waa_df %>% filter(paa_generation == paa_gen_approach) %>%
  dplyr::group_by(scenario, age, data_scen, Ecov_sim, caal_samp, age_selex, re_method, 
                  method, growth_var, im) %>% 
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad))
temp = temp %>% dplyr::filter(growth_var > 0)

# Set EM and OM labels:
temp = set_labels(temp, ecov_type = c('stationary', 'trend'), conv_level = max_grad)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(age, levels = 1:10, labels = 1:10))

# Prepare data for geom linerage plot:
plot_dat = temp %>% group_by(em_label, par2, om_label, Ecov_sim, age_selex) %>%
  dplyr::summarise(bias = round(quantile(rel_error, probs = 0.5)*re_mult),
                   precision = round(sd(rel_error)*re_mult))
plot_dat = plot_dat %>% mutate(box_label = paste0(bias, '(', precision, ')'))

p3 = make_heatmap(df = plot_dat, this_factor = bias, this_label = box_label, y_label = Ecov_sim)
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, 'waa', sep = '-'), fig_type)), 
       plot = p3, width = img_width, height = 210, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# Pred catch CAA info:
temp = catch_paa_df %>% filter(paa_generation == paa_gen_approach) %>%
  dplyr::group_by(scenario, age, data_scen, Ecov_sim, caal_samp, age_selex, re_method, 
                  method, growth_var, im) %>%  
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad))
temp = temp %>% dplyr::filter(growth_var > 0)

# Set EM and OM labels:
temp = set_labels(temp, ecov_type = c('stationary', 'trend'), conv_level = max_grad)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(age, levels = 1:10, labels = 1:10))

# Prepare data for plot:
plot_dat = temp %>% group_by(em_label, par2, om_label, Ecov_sim, caal_samp, age_selex) %>%
  dplyr::summarise(bias = round(quantile(rel_error, probs = 0.5)*re_mult),
                   precision = round(sd(rel_error)*re_mult))
plot_dat = plot_dat %>% mutate(box_label = paste0(bias, '(', precision, ')'))

# Make plot:
p3 = make_heatmap(df = plot_dat, this_factor = bias, this_label = box_label, y_label = Ecov_sim)
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, 'caa', sep = '-'), fig_type)), 
       plot = p3, width = img_width, height = 210, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# Pred catch IAA info:
temp = index_paa_df %>% filter(paa_generation == paa_gen_approach) %>%
  dplyr::group_by(scenario, age, data_scen, Ecov_sim, caal_samp, age_selex, re_method, 
                  method, growth_var, im) %>%  
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad))
temp = temp %>% dplyr::filter(growth_var > 0)

# Set EM and OM labels:
temp = set_labels(temp, ecov_type = c('stationary', 'trend'), conv_level = max_grad)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(age, levels = 1:10, labels = 1:10))

# Prepare data for plot:
plot_dat = temp %>% group_by(em_label, par2, om_label, Ecov_sim, caal_samp, age_selex) %>%
  dplyr::summarise(bias = round(quantile(rel_error, probs = 0.5)*re_mult),
                   precision = round(sd(rel_error)*re_mult))
plot_dat = plot_dat %>% mutate(box_label = paste0(bias, '(', precision, ')'))

# Make plot:
p3 = make_heatmap(df = plot_dat, this_factor = bias, this_label = box_label, y_label = Ecov_sim)
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, 'iaa', sep = '-'), fig_type)), 
       plot = p3, width = img_width, height = 210, units = 'mm', dpi = img_res)
