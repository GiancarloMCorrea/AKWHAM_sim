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

# color scale for EBS and MAB:
colpal1 = wesanderson::wes_palettes$GrandBudapest1[3:4]

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
n_sim = 110 # number of iterations run per scenario.

# Set EM and OM labels:
tmp_df = par_df %>% dplyr::filter(paa_generation == paa_gen_approach,
                                  par == 'logit_q') # filter any parameter
temp = set_labels(tmp_df, caal_type = c('random', 'strat'),  
                  selex_type = c('fixed', 'varying'),
                  ecov_type = c('stationary', 'trend'),
                  remove_conv = FALSE)
temp = temp %>% mutate(Ecov_sim = if_else(growth_var == 0, 'None', Ecov_sim))
# Create convergence column:
temp = temp %>% mutate(converged = if_else(maxgrad < max_grad & !na_sdrep & convergence == 0, TRUE, FALSE))
# Summarise
conv_df = temp %>% group_by(em_label, om_label, Ecov_sim, caal_samp, age_selex) %>%
  dplyr::summarise(n_conv = sum(converged)) %>%
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
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 9),
        axis.text.y = element_text(size = 10)) +
  labs(fill = 'Convergence rate (%)') +
  facet_grid(y_label ~ om_label, labeller = 'label_parsed', scales = 'free_y')
ggsave(filename = file.path(save_folder, paste0(paa_gen_approach, '_convrate', fig_type)),
       plot = c1, width = img_width , height = 130, units = 'mm', dpi = img_res)

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
tmp_df = set_labels(tmp_df, ecov_type = c('stationary', 'trend'), 
                    selex_type = c('fixed', 'varying'), 
                    conv_level = max_grad)
# Filter first X reps:
tmp_df = filter_iter(tmp_df)
# Set par labels:
tmp_df = tmp_df %>% mutate(par2 = factor(par, levels = c('mean_rec_pars', 'logit_q', 'log_NAA_sigma'),
                                         labels = c(expression(bar(R)), 'Q', expression(sigma[R]))) # expression(N["1,1"]) 'F[1]'
)
# Define y label
tmp_df = tmp_df %>% mutate(y_label = paste(Ecov_sim, age_selex, sep = '/'))


# Plot:
plot_dat = tmp_df %>% group_by(em_label, par2, om_label, y_label) %>%
  dplyr::summarise(bias = round(quantile(rel_error, probs = 0.5)*re_mult),
                   precision = round(sd(rel_error)*re_mult))
plot_dat = plot_dat %>% mutate(box_label = paste0(bias, '(', precision, ')'))

p3 = make_heatmap(df = plot_dat, this_factor = bias, this_label = box_label, y_label = y_label)
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, 'par', sep = '-'), fig_type)), 
       plot = p3, width = img_width, height = 170, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# avg TS plot:
# Prepare data
temp = ts_df %>% dplyr::group_by(scenario, par, paa_generation, data_scen, Ecov_sim, 
                                 caal_samp, age_selex, re_method, method, growth_var, im) %>% 
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad), 
                   na_sdrep = unique(na_sdrep), convergence = unique(convergence)) # median over the years
temp = temp %>% dplyr::filter(growth_var > 0)

tmp_df = temp %>% dplyr::filter(paa_generation == paa_gen_approach)
# Set EM and OM labels:
tmp_df = set_labels(tmp_df, ecov_type = c('stationary', 'trend'), 
                    selex_type = c('fixed', 'varying'), 
                    conv_level = max_grad)
# Filter first X reps:
tmp_df = filter_iter(tmp_df)
# Set par labels:
tmp_df = tmp_df %>% mutate(par2 = factor(par, levels = c('SSB', 'Rec', 'F'),
                                         labels = c('SSB', 'R', 'F')) # expression(N["1,1"]) 'F[1]'
) 
# Define y label
tmp_df = tmp_df %>% mutate(y_label = paste(Ecov_sim, age_selex, sep = '/'))

# Plot:
plot_dat = tmp_df %>% group_by(em_label, par2, om_label, y_label) %>%
  dplyr::summarise(bias = round(quantile(rel_error, probs = 0.5)*re_mult),
                   precision = round(sd(rel_error)*re_mult))
plot_dat = plot_dat %>% mutate(box_label = paste0(bias, '(', precision, ')'))

p3 = make_heatmap(df = plot_dat, this_factor = bias, this_label = box_label, y_label = y_label)
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, 'avg-ts', sep = '-'), fig_type)), 
       plot = p3, width = img_width, height = 170, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# TS plot (by year, for ALL scenarios):
# Do it variable by variable: F, R, SSB
ts_folder_plot = file.path(save_folder, 'ts_plots')
dir.create(ts_folder_plot, showWarnings = FALSE)

# Sort data:
temp = ts_df %>% dplyr::filter(growth_var > 0, paa_generation == paa_gen_approach) %>% 
  dplyr::group_by(paa_generation, scenario, par, year, data_scen, caal_samp, age_selex, re_method, 
                                 Ecov_sim, method, growth_var, im) %>% 
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad), 
                   na_sdrep = unique(na_sdrep), convergence = unique(convergence))

# Plot TS by variable:
all_vars = c('Rec', 'SSB', 'F')
for(i in seq_along(all_vars)) {
  
  sel_var = all_vars[i]
  # Set EM and OM labels:
  temp2 = set_labels(temp, ecov_type = c('stationary', 'trend'), 
                     conv_level = max_grad)
  # Filter first 100 reps:
  temp2 = filter_iter(temp2)
  # Select variable to plot:
  temp2 = temp2 %>% dplyr::filter(par == sel_var)
  # Make em label:
  temp2$em_label2 = factor(temp2$em_label, labels = c("WEm", expression(WNP*"-"*iid), 
                                                      expression(WNP*"-"*2*"D"),
                                                      expression(WNP*"-"*3*"D")))
  # Prepare data for geom linerage plot:
  plot_dat = temp2 %>% group_by(Ecov_sim, em_label2, year, om_label) %>%
    dplyr::summarise(q025 = quantile(rel_error, probs = 0.025)*re_mult, 
                     q50 = quantile(rel_error, probs = 0.5)*re_mult,
                     q975 = quantile(rel_error, probs = 0.975)*re_mult)
  
  # Make plot:
  if(sel_var == 'Rec') { yLim = c(-50, 50) } else { yLim = NULL }
  p1 = make_plot_ts(plot_dat, Ecov_sim, col_vals = colpal1, leg_pos = 'bottom', yLim = yLim)
  ggsave(filename = file.path(ts_folder_plot, paste0(paste(paa_gen_approach, 'ts', sel_var, sep = '-'), fig_type)), 
         plot = p1, width = img_width, height = 210, units = 'mm', dpi = img_res)
  
}


# -------------------------------------------------------------------------
# WAA info:
temp = waa_df %>% filter(growth_var > 0, paa_generation == paa_gen_approach) %>%
  dplyr::group_by(scenario, age, data_scen, Ecov_sim, caal_samp, age_selex, re_method, 
                  method, growth_var, im) %>% 
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad), 
                   na_sdrep = unique(na_sdrep), convergence = unique(convergence))

# Set EM and OM labels:
temp = set_labels(temp, ecov_type = c('stationary', 'trend'), 
                  selex_type = c('fixed', 'varying'), 
                  conv_level = max_grad)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% dplyr::filter(age %in% c(1:3, 9:10)) %>% 
          mutate(par2 = factor(age, levels = 1:10, labels = 1:10))
# Define y label
temp = temp %>% mutate(y_label = paste(Ecov_sim, age_selex, sep = '/'))

# Prepare data for geom linerage plot:
plot_dat = temp %>% group_by(em_label, par2, om_label, y_label) %>%
  dplyr::summarise(bias = round(quantile(rel_error, probs = 0.5)*re_mult),
                   precision = round(sd(rel_error)*re_mult))
plot_dat = plot_dat %>% mutate(box_label = paste0(bias, '(', precision, ')'))

p3 = make_heatmap(df = plot_dat, this_factor = bias, this_label = box_label, y_label = y_label)
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, 'waa', sep = '-'), fig_type)), 
       plot = p3, width = img_width, height = 180, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# Pred catch CAA info:
temp = catch_paa_df %>% filter(growth_var > 0, paa_generation == paa_gen_approach) %>%
  dplyr::group_by(scenario, age, data_scen, Ecov_sim, caal_samp, age_selex, re_method, 
                  method, growth_var, im) %>%  
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad), 
                   na_sdrep = unique(na_sdrep), convergence = unique(convergence))

# Set EM and OM labels:
temp = set_labels(temp, ecov_type = c('stationary', 'trend'), 
                  selex_type = c('fixed', 'varying'), 
                  conv_level = max_grad)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% dplyr::filter(age %in% c(2:4, 9:10)) %>% 
  mutate(par2 = factor(age, levels = 1:10, labels = 1:10))
# Define y label
temp = temp %>% mutate(y_label = paste(Ecov_sim, age_selex, sep = '/'))

# Prepare data for plot:
plot_dat = temp %>% group_by(em_label, par2, om_label, y_label) %>%
  dplyr::summarise(bias = round(quantile(rel_error, probs = 0.5)*re_mult),
                   precision = round(sd(rel_error)*re_mult))
plot_dat = plot_dat %>% mutate(box_label = paste0(bias, '(', precision, ')'))

# Make plot:
p3 = make_heatmap(df = plot_dat, this_factor = bias, this_label = box_label, y_label = y_label)
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, 'caa', sep = '-'), fig_type)), 
       plot = p3, width = img_width, height = 180, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# Pred catch IAA info:
temp = index_paa_df %>% filter(growth_var > 0, paa_generation == paa_gen_approach) %>%
  dplyr::group_by(scenario, age, data_scen, Ecov_sim, caal_samp, age_selex, re_method, 
                  method, growth_var, im) %>%  
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad), 
                   na_sdrep = unique(na_sdrep), convergence = unique(convergence))

# Set EM and OM labels:
temp = set_labels(temp, ecov_type = c('stationary', 'trend'), 
                  selex_type = c('fixed', 'varying'),
                  conv_level = max_grad)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% dplyr::filter(age %in% c(1:3, 9:10)) %>% 
  mutate(par2 = factor(age, levels = 1:10, labels = 1:10))
# Define y label
temp = temp %>% mutate(y_label = paste(Ecov_sim, age_selex, sep = '/'))

# Prepare data for plot:
plot_dat = temp %>% group_by(em_label, par2, om_label, y_label) %>%
  dplyr::summarise(bias = round(quantile(rel_error, probs = 0.5)*re_mult),
                   precision = round(sd(rel_error)*re_mult))
plot_dat = plot_dat %>% mutate(box_label = paste0(bias, '(', precision, ')'))

# Make plot:
p3 = make_heatmap(df = plot_dat, this_factor = bias, this_label = box_label, y_label = y_label)
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, 'iaa', sep = '-'), fig_type)), 
       plot = p3, width = img_width, height = 180, units = 'mm', dpi = img_res)
