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

# -------------------------------------------------------------------------
# Prepare data:

this_age_selex = 'fixed'
this_caal_samp = 'random'
this_data_scen = 'rich'
temp1 = par_df %>% dplyr::filter(par %in% c('logit_q', 'mean_rec_pars')) # 'log_F1', 'log_N1_pars'
temp2 = ts_df %>% dplyr::group_by(scenario, par, paa_generation, data_scen, caal_samp, age_selex, re_method, method, growth_var, im) %>% 
            dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad)) # median over the years
# Merge both:
temp = bind_rows(temp1, temp2)


# -------------------------------------------------------------------------
# PAR and average TS plot (traditional vs stepwise):

# Set EM and OM labels:
tmp_df = set_labels(temp, selex_type = this_age_selex, caal_type = this_caal_samp, data_type = this_data_scen, conv_level = max_grad)
# Filter first X reps:
tmp_df = filter_iter(tmp_df)
# Set par labels:
tmp_df = tmp_df %>% mutate(par2 = factor(par, levels = c('mean_rec_pars', 'logit_q', 'SSB', 'Rec', 'F'),
                                     labels = c(expression(bar(R)), 'Q', 'SSB', 'R', 'F')), # expression(N["1,1"]) 'F[1]'
                       paa_generation = factor(paa_generation, levels = c('traditional', 'stepwise'),
                                               labels = c('Traditional', 'Stepwise'))) 

# Prepare data for geom linerage plot:
plot_dat = tmp_df %>% group_by(em_label, par2, om_label, paa_generation, data_scen, caal_samp, age_selex) %>%
  dplyr::summarise(q025 = quantile(rel_error, probs = 0.025)*re_mult, 
                   q50 = quantile(rel_error, probs = 0.5)*re_mult,
                   q975 = quantile(rel_error, probs = 0.975)*re_mult)

# Make plot:
p1 = make_plot_1b(plot_dat, paa_generation, y_break = 0.2, violin_sep = 0.4, 
                  leg_pos = 'bottom', leg_title = '', alpha_level = 1, col_vals = colpal1)
ggsave(filename = file.path(save_folder, paste0(paste('main', this_caal_samp, this_data_scen, this_age_selex, 'par', sep = '-'), fig_type)), 
       plot = p1, width = img_width, height = 220, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# TS plot (by year, for ALL scenarios):
# Do it variable by variable: F, R, SSB
ts_folder_plot = file.path(save_folder, 'ts_plots')
dir.create(ts_folder_plot, showWarnings = FALSE)

# Sort data:
temp = ts_df %>% dplyr::group_by(paa_generation, scenario, par, year, data_scen, caal_samp, age_selex, re_method, 
                  method, growth_var, im) %>% 
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad))

# Select filter:
this_age_selex = 'fixed'
this_caal_samp = 'random' # only select one
this_data_scen = 'rich'
sel_var = 'F' # Rec, SSB, F

######
# Set EM and OM labels:
temp2 = set_labels(temp, selex_type = this_age_selex, caal_type = this_caal_samp, data_type = this_data_scen, conv_level = max_grad)
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
plot_dat = temp2 %>% group_by(paa_generation, em_label2, year, om_label, data_scen) %>%
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
ggsave(filename = file.path(ts_folder_plot, paste0(paste('main', this_caal_samp, this_data_scen, this_age_selex, 'ts', sel_var, sep = '-'), fig_type)), 
       plot = p1, width = img_width, height = 210, units = 'mm', dpi = img_res)



# -------------------------------------------------------------------------

# Heatmap to analyze effects of data_scen and sampling strategy:
# Stepwise approach
paa_gen_approach = 'stepwise'
tmp_df = temp %>% dplyr::filter(paa_generation == paa_gen_approach)
# Set EM and OM labels:
tmp_df = set_labels(tmp_df, selex_type = this_age_selex, caal_type = c('random', 'strat'), conv_level = max_grad)
# Filter first X reps:
tmp_df = filter_iter(tmp_df)
# Set par labels:
tmp_df = tmp_df %>% mutate(par2 = factor(par, levels = c('mean_rec_pars', 'logit_q', 'SSB', 'Rec', 'F'),
                                         labels = c(expression(bar(R)), 'Q', 'SSB', 'R', 'F')) # expression(N["1,1"]) 'F[1]'
) 
tmp_df = tmp_df %>% mutate(y_label = paste(caal_samp, data_scen, sep = '/'))

# Make heatmap parameter by parameter and OM by OM:
all_pars = c('mean_rec_pars', 'logit_q', 'SSB', 'Rec', 'F')
save_bias = list()
save_precision = list()
counter = 1
for(j in seq_along(all_pars)) { 

    dat_i = tmp_df %>% dplyr::filter(par == all_pars[j])
    dat_i = dat_i %>% group_by(em_label, par2, om_label, y_label, age_selex) %>%
      dplyr::summarise(bias = quantile(rel_error, probs = 0.5)*re_mult,
                       precision = (quantile(rel_error, probs = 0.975)-quantile(rel_error, probs = 0.025))*re_mult)
    
    # Bias plot:
    save_bias[[counter]] = make_heatmap(dat_i, bias, y_label)
    # Precision plot:
    save_precision[[counter]] = make_heatmap(dat_i, precision, y_label, type = 2)
    
    counter = counter + 1
    
}

p_bias = gridExtra::grid.arrange(grobs = save_bias, ncol = 1)
p_precision = gridExtra::grid.arrange(grobs = save_precision, ncol = 1)
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, this_age_selex, 'par-bias', sep = '-'), fig_type)), 
       plot = p_bias, width = img_width, height = 220, units = 'mm', dpi = img_res)
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, this_age_selex, 'par-precision', sep = '-'), fig_type)), 
       plot = p_precision, width = img_width, height = 220, units = 'mm', dpi = img_res)
