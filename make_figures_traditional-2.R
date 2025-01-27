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
temp = set_labels(tmp_df, caal_type = c('random', 'strat'), selex_type = c('fixed', 'varying'), remove_conv = FALSE)
conv_df = temp %>% group_by(em_label, om_label, Ecov_sim, caal_samp, age_selex) %>%
  dplyr::summarise(n_conv = length(unique(maxgrad) < max_grad)) %>%
  dplyr::mutate(n_tot = n_sim) %>%
  dplyr::mutate(conv_rate = (n_conv/n_tot)*100)
# OUTPUT TABLE WITH SCENARIO LABELS
plot_data = conv_df %>% mutate(y_label = paste(caal_samp, age_selex, sep = '/'))
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
# Heatmap to analyze effects of data_scen and sampling strategy:
# Traditional approach
paa_gen_approach = 'traditional'

# -------------------------------------------------------------------------
# Prepare data:
this_age_selex = 'fixed'
this_caal_samp = 'random'
temp1 = par_df %>% dplyr::filter(par %in% c('logit_q', 'mean_rec_pars')) # 'log_F1', 'log_N1_pars'
temp2 = ts_df %>% dplyr::group_by(scenario, par, paa_generation, data_scen, Ecov_sim, 
                                  caal_samp, age_selex, re_method, method, growth_var, im) %>% 
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad)) # median over the years
# Merge both:
temp = bind_rows(temp1, temp2)

####
tmp_df = temp %>% dplyr::filter(paa_generation == paa_gen_approach)
# Set EM and OM labels:
tmp_df = set_labels(tmp_df, selex_type = this_age_selex, caal_type = this_caal_samp, conv_level = max_grad)
# Filter first X reps:
tmp_df = filter_iter(tmp_df)
# Set par labels:
tmp_df = tmp_df %>% mutate(par2 = factor(par, levels = c('mean_rec_pars', 'logit_q', 'SSB', 'Rec', 'F'),
                                         labels = c(expression(bar(R)), 'Q', 'SSB', 'R', 'F')) # expression(N["1,1"]) 'F[1]'
) 

# Make heatmap parameter by parameter and OM by OM:
all_pars = c('mean_rec_pars', 'logit_q', 'SSB', 'Rec', 'F')
save_bias = list()
save_precision = list()
counter = 1
for(j in seq_along(all_pars)) { 
  
  dat_i = tmp_df %>% dplyr::filter(par == all_pars[j])
  dat_i = dat_i %>% group_by(em_label, par2, om_label, Ecov_sim, caal_samp, age_selex) %>%
    dplyr::summarise(bias = quantile(rel_error, probs = 0.5)*re_mult,
                     precision = (quantile(rel_error, probs = 0.975)-quantile(rel_error, probs = 0.025))*re_mult)
  
  # Bias plot:
  save_bias[[counter]] = make_heatmap(dat_i, bias, Ecov_sim)
  # Precision plot:
  save_precision[[counter]] = make_heatmap(dat_i, precision, Ecov_sim, type = 2)
  
  counter = counter + 1
  
}

p_bias = gridExtra::grid.arrange(grobs = save_bias, ncol = 1)
p_precision = gridExtra::grid.arrange(grobs = save_precision, ncol = 1)
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, this_age_selex, 'par-bias', sep = '-'), fig_type)), 
       plot = p_bias, width = img_width, height = 210, units = 'mm', dpi = img_res)
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, this_age_selex, 'par-precision', sep = '-'), fig_type)), 
       plot = p_precision, width = img_width, height = 210, units = 'mm', dpi = img_res)

# # -------------------------------------------------------------------------
# # TS plot (by year, for ALL scenarios):
# # Do it variable by variable: F, R, SSB
# ts_folder_plot = file.path(save_folder, 'ts_plots')
# dir.create(ts_folder_plot, showWarnings = FALSE)
# 
# # Sort data:
# temp = ts_df %>% filter(paa_generation == paa_gen_approach) %>%
#   dplyr::group_by(scenario, par, year, data_scen, caal_samp, age_selex, re_method, 
#                   method, growth_var, im) %>% 
#   dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad))
# 
# # Select filter:
# this_age_selex = 'fixed'
# this_caal_samp = 'random' # only select one
# sel_var = 'F' # Rec, SSB, F
# 
# # Set EM and OM labels:
# temp2 = set_labels(temp, selex_type = this_age_selex, caal_type = this_caal_samp, conv_level = max_grad)
# # Filter first 100 reps:
# temp2 = filter_iter(temp2)
# # Select variable to plot:
# temp2 = temp2 %>% dplyr::filter(par == sel_var)
# # Make em label:
# temp2$em_label2 = factor(temp2$em_label, labels = c("WEm", expression(WNP*"-"*iid), 
#                                                           expression(WNP*"-"*2*"D"),
#                                                           expression(WNP*"-"*3*"D")))
# 
# # Prepare data for geom linerage plot:
# plot_dat = temp2 %>% group_by(em_label2, year, om_label, data_scen) %>%
#   dplyr::summarise(q025 = quantile(rel_error, probs = 0.025)*re_mult, 
#                    q50 = quantile(rel_error, probs = 0.5)*re_mult,
#                    q975 = quantile(rel_error, probs = 0.975)*re_mult)
# 
# # Make plot:
# p1 = ggplot(plot_dat, aes(x = year, y = q50)) +
#   geom_line(aes(color = data_scen)) +
#   geom_ribbon(aes(ymin = q025, ymax = q975, fill = data_scen), alpha = 0.3) +
#   scale_color_manual(values = colpal1) +
#   scale_fill_manual(values = colpal1) +
#   ylab('Relative error (%)') + xlab('Simulated year') +
#   theme(legend.position = 'bottom',
#         strip.background = element_rect(fill="white")) +
#   facet_grid(em_label2 ~ om_label, labeller = 'label_parsed') +
#   guides(colour=guide_legend(title=NULL), fill=guide_legend(title=NULL))
# ggsave(filename = file.path(ts_folder_plot, paste0(paste(paa_gen_approach, this_age_selex, this_caal_samp, sel_var, sep = '-'), fig_type)), 
#        plot = p1, width = img_width, height = 210, units = 'mm', dpi = img_res)


# -------------------------------------------------------------------------
# WAA info (median over years, only for WAA and Ewaa scenarios):
this_age_selex = 'fixed'
this_caal_samp = 'random'
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
  dplyr::summarise(bias = quantile(rel_error, probs = 0.5)*re_mult,
                   precision = (quantile(rel_error, probs = 0.975)-quantile(rel_error, probs = 0.025))*re_mult)

p3 = make_heatmap(plot_dat, bias, data_scen)
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, this_age_selex, 'waa-bias', sep = '-'), fig_type)), 
       plot = p3, width = img_width, height = 210, units = 'mm', dpi = img_res)
p4 = make_heatmap(plot_dat, precision, data_scen, type = 2)
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, this_age_selex, 'waa-precision', sep = '-'), fig_type)), 
       plot = p4, width = img_width, height = 210, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# Pred catch CAA info:
this_age_selex = 'fixed'
this_caal_samp = 'random'
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

# Prepare data for plot:
plot_dat = temp %>% group_by(em_label, par2, om_label, data_scen, caal_samp, age_selex) %>%
  dplyr::summarise(bias = quantile(rel_error, probs = 0.5)*re_mult,
                   precision = (quantile(rel_error, probs = 0.975)-quantile(rel_error, probs = 0.025))*re_mult)

# Make plot:
p3 = make_heatmap(plot_dat, bias, data_scen)
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, this_age_selex, 'caa-bias', sep = '-'), fig_type)), 
       plot = p3, width = img_width, height = 210, units = 'mm', dpi = img_res)
p4 = make_heatmap(plot_dat, precision, data_scen, type = 2)
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, this_age_selex, 'caa-precision', sep = '-'), fig_type)), 
       plot = p4, width = img_width, height = 210, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# Pred catch IAA info:
this_age_selex = 'fixed'
this_caal_samp = 'random'
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

# Prepare data for plot:
plot_dat = temp %>% group_by(em_label, par2, om_label, data_scen, caal_samp, age_selex) %>%
  dplyr::summarise(bias = quantile(rel_error, probs = 0.5)*re_mult,
                   precision = (quantile(rel_error, probs = 0.975)-quantile(rel_error, probs = 0.025))*re_mult)

# Make plot:
p3 = make_heatmap(plot_dat, bias, data_scen)
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, this_age_selex, 'iaa-bias', sep = '-'), fig_type)), 
       plot = p3, width = img_width, height = 210, units = 'mm', dpi = img_res)
p4 = make_heatmap(plot_dat, precision, data_scen, type = 2)
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, this_age_selex, 'iaa-precision', sep = '-'), fig_type)), 
       plot = p4, width = img_width, height = 210, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# Selex parameters:
this_age_selex = 'fixed'
this_caal_samp = 'random'
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
#                  leg_pos = 'bottom', leg_title = '', alpha_level = 0.75, col_vals = colpal1,
#                  var_name = expression('Selectivity parameter '~beta[1]))
p9 = make_plot_3b(plot_dat, data_scen, violin_sep = 0.4, 
                  leg_pos = 'bottom', leg_title = '', alpha_level = 1, col_vals = colpal1,
                  var_name = expression('Selectivity parameter '~beta[1]))
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, this_age_selex, 'sel', sep = '-'), fig_type)), 
       plot = p9, width = img_width , height = 130, units = 'mm', dpi = img_res)
