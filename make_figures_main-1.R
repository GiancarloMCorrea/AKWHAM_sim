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
dir.create(save_folder, showWarnings = FALSE, recursive = TRUE)

# Read scenarios df
df.scenario = readRDS('inputs/df.scenarios.RDS')

# Define figure type:
fig_type = '.png'
img_res = 400
img_width = 170

# Color palettes
colpal1 = viridisLite::viridis(n = 6)[c(1,4)]
colpal2 = wesanderson::wes_palettes$Darjeeling1[1:2]

# Convergence level:
max_grad = 1e-04

# RE in %?
re_mult = 100

# -------------------------------------------------------------------------
# Read output files -------------------------------------------------------

# Read for cod:
output_folder = file.path('outputs', 'cod')
ts_df1 = readRDS(file = file.path(output_folder, 'ts_results.RDS')) %>% mutate(species = 'Cod') # TS data
par_df1 = readRDS(file = file.path(output_folder, 'par_results.RDS')) %>% mutate(species = 'Cod') # par data
index_paa_df1 = readRDS(file = file.path(output_folder, 'index_paa_results.RDS')) %>% mutate(species = 'Cod') # Index pred paa
waa_df1 = readRDS(file = file.path(output_folder, 'waa_results.RDS')) %>% mutate(species = 'Cod') # WAA data

# Read for haddock:
output_folder = file.path('outputs', 'haddock')
ts_df2 = readRDS(file = file.path(output_folder, 'ts_results.RDS')) %>% mutate(species = 'Haddock') # TS data
par_df2 = readRDS(file = file.path(output_folder, 'par_results.RDS')) %>% mutate(species = 'Haddock') # par data
index_paa_df2 = readRDS(file = file.path(output_folder, 'index_paa_results.RDS')) %>% mutate(species = 'Haddock') # Index pred paa
waa_df2 = readRDS(file = file.path(output_folder, 'waa_results.RDS')) %>% mutate(species = 'Haddock') # WAA data

# Merge both:
ts_df = rbind(ts_df1, ts_df2)
rm(ts_df1, ts_df2)
par_df = rbind(par_df1, par_df2)
rm(par_df1, par_df2)
index_paa_df = rbind(index_paa_df1, index_paa_df2)
rm(index_paa_df1, index_paa_df2)
# Merge only for plus group:
waa_df = rbind(waa_df1 %>% filter(age == max(age)), waa_df2 %>% filter(age == max(age)))
rm(waa_df1, waa_df2)

# -------------------------------------------------------------------------
# PAR plot
# Select parameters:
temp = par_df %>% dplyr::filter(par %in% c('logit_q', 'mean_rec_pars', 'log_NAA_sigma')) # 'log_F1'

# Set EM and OM labels:
tmp_df = set_labels(temp, conv_level = max_grad)
# Filter first X reps:
tmp_df = filter_iter(tmp_df)
# Set par labels:
tmp_df = tmp_df %>% mutate(par2 = factor(par, levels = c('mean_rec_pars', 'logit_q', 'log_NAA_sigma'),
                                         labels = c(expression(bar(R)), 'Q', expression(sigma[R]))), # expression(N["1,1"]) 'F[1]'
                           paa_generation = factor(paa_generation, levels = c('traditional', 'stepwise'),
                                                   labels = c('Traditional', 'Stepwise'))) 

# Prepare data for geom linerage plot:
plot_dat = tmp_df %>% group_by(em_label, par2, om_label, paa_generation, species) %>%
  dplyr::summarise(q025 = quantile(rel_error, probs = 0.025)*re_mult, 
                   q50 = quantile(rel_error, probs = 0.5)*re_mult,
                   q975 = quantile(rel_error, probs = 0.975)*re_mult)

# Make plot:
p1 = make_plot_1b(plot_dat, paa_generation, y_break = 0.2, violin_sep = 0.4, 
                  leg_pos = 'bottom', leg_title = '', alpha_level = 1, col_vals = colpal1)
ggsave(filename = file.path(save_folder, paste0(paste('main', 'par', sep = '-'), fig_type)), 
       plot = p1, width = img_width, height = 170, units = 'mm', dpi = img_res)


# -------------------------------------------------------------------------
# average TS plot
temp = ts_df %>% dplyr::select(scenario, species, par, year, paa_generation, data_scen, Ecov_sim, 
            caal_samp, age_selex, re_method, method, growth_var, im, rel_error, maxgrad, na_sdrep, convergence) 

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
plot_dat = tmp_df %>% group_by(em_label, par2, om_label, paa_generation, species, year) %>%
  dplyr::summarise(q025 = quantile(rel_error, probs = 0.025)*re_mult, 
                   q50 = quantile(rel_error, probs = 0.5)*re_mult,
                   q975 = quantile(rel_error, probs = 0.975)*re_mult) %>%
  group_by(em_label, par2, om_label, paa_generation, species) %>%
  dplyr::summarise(q025 = mean(q025), q50 = mean(q50), q975 = mean(q975))

# Make plot:
p1 = make_plot_1b(plot_dat, paa_generation, y_break = 0.2, violin_sep = 0.4, 
                  leg_pos = 'bottom', leg_title = '', alpha_level = 1, col_vals = colpal1)
ggsave(filename = file.path(save_folder, paste0(paste('main', 'avg-ts', sep = '-'), fig_type)), 
       plot = p1, width = img_width, height = 170, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# Plot parameter estimates meanR and sigmaR
# Only show stepwise
temp = par_df %>% dplyr::filter(par %in% c('mean_rec_pars', 'log_NAA_sigma'), 
                                growth_var > 0, paa_generation == 'stepwise') %>% 
  dplyr::select(scenario, par, species, data_scen, Ecov_sim, 
                caal_samp, age_selex, re_method, method, growth_var, im, rel_error, maxgrad, na_sdrep, convergence) 

# Set EM and OM labels:
tmp_df = set_labels(temp, caal_type = c('random', 'strat'), 
                    selex_type = c('fixed', 'varying'), 
                    ecov_type = c('stationary', 'trend'),
                    conv_level = max_grad)
# Filter first X reps:
tmp_df = filter_iter(tmp_df)
# Set par labels:
tmp_df = tmp_df %>% mutate(par2 = factor(par, levels = c('mean_rec_pars', 'logit_q', 'log_NAA_sigma'),
                                         labels = c(expression(bar(R)), 'Q', expression(sigma[R]))) # expression(N["1,1"]) 'F[1]'
)
tmp_df = tmp_df %>% mutate(y_label = paste(caal_samp, Ecov_sim, age_selex, sep = '/'))

# Prepare data for plot:
plot_dat = tmp_df %>% group_by(em_label, par2, om_label, y_label,species) %>%
  dplyr::summarise(bias = round(quantile(rel_error, probs = 0.5)*re_mult),
                   precision = round(sd(rel_error)*re_mult))
plot_dat = plot_dat %>% mutate(box_label = paste0(bias, '(', precision, ')'))

# Make plot:
col_vals = c("#075AFF", "white",  "#FF0000")
p1 = ggplot(data = plot_dat, aes(x = em_label, y = y_label, fill = bias)) +
  geom_tile(color = NA) +
  geom_text(aes(label = box_label), color = 'black', size = 3) +
  scale_fill_gradient2(low = col_vals[1], mid = col_vals[2], high = col_vals[3]) +
  xlab(NULL) + ylab(NULL) +
  theme(legend.position = 'none', 
        axis.text.y = element_text(angle = 0, hjust = 1),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 9),
        strip.text = element_text(size = 10),
        strip.background = element_rect(fill="white")) +
  facet_nested(species+par2 ~ om_label, labeller = 'label_parsed')
ggsave(filename = file.path(save_folder, paste0(paste('main', 'par-diff', sep = '-'), fig_type)), 
       plot = p1, width = img_width, height = 150, units = 'mm', dpi = img_res)


# -------------------------------------------------------------------------
# Plot annual Recruitment ts for both species, const+vary
# Only show WAA-iid and stepwise
temp = ts_df %>% dplyr::filter(par == 'Rec', growth_var > 0, paa_generation == 'stepwise', re_method == 'iid') %>% 
  dplyr::select(scenario, species, year, data_scen, Ecov_sim, 
                caal_samp, age_selex, re_method, method, growth_var, im, rel_error, maxgrad, na_sdrep, convergence) 

# Set EM and OM labels:
tmp_df = set_labels(temp, caal_type = c('random', 'strat'), 
                    selex_type = c('fixed', 'varying'), 
                    ecov_type = c('stationary', 'trend'),
                    conv_level = max_grad)
# Filter first X reps:
tmp_df = filter_iter(tmp_df)

# Prepare data for plot:
plot_dat = tmp_df %>% group_by(em_label, om_label, caal_samp, age_selex, Ecov_sim, species, year) %>%
  dplyr::summarise(q025 = quantile(rel_error, probs = 0.025)*re_mult, 
                   q50 = quantile(rel_error, probs = 0.5)*re_mult,
                   q975 = quantile(rel_error, probs = 0.975)*re_mult) 

# Make plot:
p1 = ggplot(plot_dat, aes(x = year, y = q50)) +
  geom_line(aes(color = caal_samp)) +
  geom_ribbon(aes(ymin = q025, ymax = q975, fill = caal_samp), alpha = 0.3) +
  geom_hline(yintercept=0, color=1, linetype='dashed') +
  scale_color_manual(values = colpal2) +
  scale_fill_manual(values = colpal2) +
  coord_cartesian(ylim = 50*c(-1, 1)) +
  ylab('Relative error (%)') + xlab('Simulated year') +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(size = 9),
        strip.text = element_text(size = 10),
        legend.text=element_text(size=10),
        strip.background = element_rect(fill="white")) +
  facet_nested(species+Ecov_sim ~ om_label+age_selex, labeller = 'label_parsed') +
  guides(colour=guide_legend(title=NULL), fill=guide_legend(title=NULL))
ggsave(filename = file.path(save_folder, paste0(paste('main', 'rec-diff', sep = '-'), fig_type)), 
       plot = p1, width = img_width, height = 170, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# Plot IAA-1 ts for both species
# Only show stepwise
temp = index_paa_df %>% dplyr::filter(age == 1, growth_var > 0, paa_generation == 'stepwise') %>% 
  dplyr::select(scenario, species, year, data_scen, Ecov_sim, 
                caal_samp, age_selex, re_method, method, growth_var, im, rel_error, maxgrad, na_sdrep, convergence) 

# Set EM and OM labels:
tmp_df = set_labels(temp, caal_type = c('random', 'strat'), 
                    selex_type = c('fixed', 'varying'), 
                    ecov_type = c('stationary', 'trend'),
                    conv_level = max_grad)
# Filter first X reps:
tmp_df = filter_iter(tmp_df)
temp = tmp_df %>% mutate(y_label = paste(caal_samp, Ecov_sim, age_selex, sep = '/'))

# Prepare data for plot:
plot_dat = temp %>% group_by(em_label, species, om_label, y_label, year) %>%
  dplyr::summarise(bias = quantile(rel_error, probs = 0.5)*re_mult,
                   precision = sd(rel_error)*re_mult) %>%
  group_by(em_label, species, om_label, y_label) %>%
  dplyr::summarise(bias = round(mean(bias)),
                   precision = round(mean(precision)))
plot_dat = plot_dat %>% mutate(box_label = paste0(bias, '(', precision, ')'))

# Make plot:
col_vals = c("#075AFF", "white",  "#FF0000")
p1 = ggplot(data = plot_dat, aes(x = em_label, y = y_label, fill = bias)) +
  geom_tile(color = NA) +
  geom_text(aes(label = box_label), color = 'black', size = 3) +
  scale_fill_gradient2(low = col_vals[1], mid = col_vals[2], high = col_vals[3]) +
  xlab(NULL) + ylab(NULL) +
  theme(legend.position = 'none', 
        axis.text.y = element_text(angle = 0, hjust = 1),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 9),
        strip.text = element_text(size = 10),
        strip.background = element_rect(fill="white")) +
  facet_grid(species ~ om_label, labeller = 'label_parsed')
ggsave(filename = file.path(save_folder, paste0(paste('main', 'iaa1-diff', sep = '-'), fig_type)), 
       plot = p1, width = img_width, height = 150, units = 'mm', dpi = img_res)


# -------------------------------------------------------------------------
# Plot WAA plus group ts for both species, EBS+MAB
# Only show stepwise
temp = waa_df %>% dplyr::filter(growth_var > 0, paa_generation == 'stepwise') %>% 
  dplyr::select(scenario, species, year, data_scen, Ecov_sim, 
                caal_samp, age_selex, re_method, method, growth_var, im, rel_error, maxgrad, na_sdrep, convergence) 

# Set EM and OM labels:
tmp_df = set_labels(temp, caal_type = c('random'), 
                    selex_type = c('fixed'), # only fixed selec
                    ecov_type = c('stationary', 'trend'),
                    conv_level = max_grad)
# Filter first X reps:
tmp_df = filter_iter(tmp_df)
# Make em label:
tmp_df$em_label2 = factor(tmp_df$em_label, labels = c("WEm", expression(WNP*"-"*iid), 
                                                    expression(WNP*"-"*2*"D"),
                                                    expression(WNP*"-"*3*"D")))

# Prepare data for plot:
plot_dat = tmp_df %>% group_by(em_label2, om_label, caal_samp, Ecov_sim, species, year) %>%
  dplyr::summarise(q025 = quantile(rel_error, probs = 0.025)*re_mult, 
                   q50 = quantile(rel_error, probs = 0.5)*re_mult,
                   q975 = quantile(rel_error, probs = 0.975)*re_mult) 

# Make plot:
p1 = ggplot(plot_dat, aes(x = year, y = q50)) +
  geom_line(aes(color = caal_samp)) +
  geom_ribbon(aes(ymin = q025, ymax = q975, fill = caal_samp), alpha = 0.3) +
  geom_hline(yintercept=0, color=1, linetype='dashed') +
  scale_color_manual(values = colpal2) +
  scale_fill_manual(values = colpal2) +
  coord_cartesian(ylim = 80*c(-1, 1)) +
  ylab('Relative error (%)') + xlab('Simulated year') +
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 9),
        strip.text = element_text(size = 10),
        legend.text=element_text(size=10),
        strip.background = element_rect(fill="white")) +
  facet_nested(species+em_label2 ~ om_label+Ecov_sim, labeller = 'label_parsed') +
  guides(colour=guide_legend(title=NULL), fill=guide_legend(title=NULL))
ggsave(filename = file.path(save_folder, paste0(paste('main', 'waa-diff', sep = '-'), fig_type)), 
       plot = p1, width = img_width, height = 180, units = 'mm', dpi = img_res)
