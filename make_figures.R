# Code to make figures
library(ggplot2)
library(dplyr)
library(plyr)
library(tidyr)
library(ggh4x)
library(viridis)
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
EM_order = c("WEm:paa(r)/paa(r)", "WEm:paa(s)/paa(s)", "WNP:paa(r)/paa(r)", "WNP:paa(s)/paa(s)", 
             "LP:pal/pal", "LP:pal/paa(r)", "LP:pal/paa(s)", "LP:pal/pal+caal(r)", "LP:pal/pal+caal(s)",  
             "LEc:pal/pal", "LEc:pal/paa(r)", "LEc:pal/paa(s)", "LEc:pal/pal+caal(r)", "LEc:pal/pal+caal(s)")

# -------------------------------------------------------------------------
# Read output files -------------------------------------------------------

# TS data:
ts_df1 = readRDS(file = 'outputs/ts_results1.RDS')
#ts_df2 = readRDS(file = 'outputs/ts_results2.RDS')
ts_df = rbind(ts_df1)

# par data:
par_df1 = readRDS(file = 'outputs/par_results1.RDS')
#par_df2 = readRDS(file = 'outputs/par_results2.RDS')
par_df = rbind(par_df1)

# WAA data:
waa_df1 = readRDS(file = 'outputs/waa_results1.RDS')
#waa_df2 = readRDS(file = 'outputs/waa_results2.RDS')
waa_df = rbind(waa_df1)

# Selex data:
sel_df1 = readRDS(file = 'outputs/sel_results1.RDS')
#sel_df2 = readRDS(file = 'outputs/sel_results2.RDS')
sel_df = rbind(sel_df1)


# -------------------------------------------------------------------------
# Convergence rates:
n_sim = 125 # number of iterations run per scenario.

conv_df = par_df %>% group_by(Scenario) %>% 
            dplyr::summarise(n_conv = length(unique(maxgrad) < 1)) %>%
            dplyr::mutate(n_tot = n_sim) %>%
            dplyr::mutate(conv_rate = (n_conv/n_tot)*100)
# OUTPUT TABLE WITH SCENARIO LABELS

# -------------------------------------------------------------------------
# PAR plot (for ALL scenarios):
temp = par_df %>% filter(par %in% c('logit_q', 'mean_rec_pars', 'log_N1_pars')) # 'log_F1'
# Set EM and OM labels:
temp = set_labels(temp)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(par, levels = c('mean_rec_pars', 'logit_q', 'log_N1_pars'),
                                     labels = c(expression(bar(R)), 'Q', expression(N["1,1"])))) # 'F[1]'

# WEm and WNP results:
temp2 = temp %>% filter(method %in% c('WEm', 'WNP'))
p1 = make_plot_1(temp2)
ggsave(filename = file.path(save_folder, 'par_1.jpg'), plot = p1, 
       width = 190 , height = 220, units = 'mm', dpi = 500)

# LP results:
temp2 = temp %>% filter(method %in% c('LP'))
p1 = make_plot_1(temp2)
ggsave(filename = file.path(save_folder, 'par_2.jpg'), plot = p1, 
       width = 190 , height = 220, units = 'mm', dpi = 500)

# LEc results:
temp2 = temp %>% filter(method %in% c('LEc'))
p1 = make_plot_1(temp2)
ggsave(filename = file.path(save_folder, 'par_3.jpg'), plot = p1, 
       width = 190 , height = 220, units = 'mm', dpi = 500)

# -------------------------------------------------------------------------
# TS plot (median over years, for ALL scenarios):
temp = ts_df %>% dplyr::group_by(scenario, par, data_scen, catch_data, index_data, caal_samp, method, Ecov_sim, growth_par, im) %>% 
            dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad))
# Set EM and OM labels:
temp = set_labels(temp)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(par, levels = c('SSB', 'Rec', 'F'),
                                     labels = c('SSB', 'R', 'F')))

# WEm and WNP results:
temp2 = temp %>% filter(method %in% c('WEm', 'WNP'))
p1 = make_plot_1(temp2, y_break = 0.2)
ggsave(filename = file.path(save_folder, 'ts_1.jpg'), plot = p1,
       width = 190 , height = 230, units = 'mm', dpi = 500)

# LP results:
temp2 = temp %>% filter(method %in% c('LP'))
p1 = make_plot_1(temp2, y_break = 0.2)
ggsave(filename = file.path(save_folder, 'ts_2.jpg'), plot = p1,
       width = 190 , height = 230, units = 'mm', dpi = 500)

# LEc results:
temp2 = temp %>% filter(method %in% c('LEc'))
p1 = make_plot_1(temp2, y_break = 0.2)
ggsave(filename = file.path(save_folder, 'ts_3.jpg'), plot = p1,
       width = 190 , height = 230, units = 'mm', dpi = 500)


# -------------------------------------------------------------------------
# TS plot (by year, for ALL scenarios):

# TODO


# -------------------------------------------------------------------------
# Main growth parameters (only for growth, Ecov scenarios):
temp = par_df %>% filter(par %in% c('k', 'Linf', 'L1')) 
# Set EM and OM labels:
temp = set_labels(temp)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(par, levels = c('k', 'Linf', 'L1'),
                                     labels = c('k', expression(L[infinity]), expression(L[1]))))

# LP results:
temp2 = temp %>% filter(method %in% c('LP'))
p1 = make_plot_1(temp2, y_break = 0.2)
ggsave(filename = file.path(save_folder, 'growth_1.jpg'),  plot = p1,
       width = 190 , height = 230, units = 'mm', dpi = 500)

# LEc results:
temp2 = temp %>% filter(method %in% c('LEc'))
p1 = make_plot_1(temp2, y_break = 0.2)
ggsave(filename = file.path(save_folder, 'growth_2.jpg'),  plot = p1,
       width = 190 , height = 230, units = 'mm', dpi = 500)

# -------------------------------------------------------------------------
# SD growth parameters (only for growth, Ecov scenarios):
temp = par_df %>% filter(par %in% c('SD1', 'SDA')) # 'SD1', 'SDA'
# Set EM and OM labels:
temp = set_labels(temp)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(par, levels = c('SD1', 'SDA'),
                                     labels = c(expression(SD[1]), expression(SD[A]))))

# LP results:
temp2 = temp %>% filter(method %in% c('LP'))
p1 = make_plot_1(temp2, y_break = 0.2)
ggsave(filename = file.path(save_folder, 'growthSD_1.jpg'),  plot = p1,
       width = 190 , height = 230, units = 'mm', dpi = 500)

# LEc results:
temp2 = temp %>% filter(method %in% c('LEc'))
p1 = make_plot_1(temp2, y_break = 0.2)
ggsave(filename = file.path(save_folder, 'growthSD_2.jpg'),  plot = p1,
       width = 190 , height = 230, units = 'mm', dpi = 500)


# -------------------------------------------------------------------------
# Ecov parameters (only for Ecov scenarios):
temp = par_df %>% filter(par %in% c('EcovBeta')) # 'sigma', 'rho',  
# Set EM and OM labels:
temp = set_labels(temp)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(par, levels = c('EcovBeta'),
                                     labels = c(expression(beta)))) # expression(sigma[X]^2), expression(rho[X]), 
# Select relevant scenarios:
temp = temp %>% filter(method %in% c('LEc'))
temp$rel_error[is.nan(temp$rel_error)] = NA

# Make plot:
p1 = make_plot_1(temp2, y_break = 0.2)
ggsave(filename = file.path(save_folder, 'ecov_1.jpg'),  plot = p1,
       width = 190 , height = 140, units = 'mm', dpi = 500)

# -------------------------------------------------------------------------
# WAA info (median over years, only for WAA and Ewaa scenarios):
temp = waa_df %>% dplyr::group_by(scenario, age, data_scen, catch_data, index_data, caal_samp, method, Ecov_sim, growth_par, im) %>% 
            dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad))
# Set EM and OM labels:
temp = set_labels(temp)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
temp = temp %>% mutate(par2 = factor(age, levels = 1:10, labels = 1:10))
# Select relevant scenarios:
temp = temp %>% filter(method %in% c('WEm', 'WNP'))

# Make plot (stationary):
this_ecov = 'Stationary' # select the Ecov scenario
df_plot = temp %>% filter(Ecov_sim == this_ecov)
p6 = make_plot_2(df_plot, y_break = 0.4)
ggsave(filename = file.path(save_folder, paste0('waa_', this_ecov,'.jpg')), plot = p6, 
       width = 190 , height = 240, units = 'mm', dpi = 500)

# Make plot (trend):
this_ecov = 'Trend' # select the Ecov scenario
df_plot = temp %>% filter(Ecov_sim == this_ecov)
p6 = make_plot_2(df_plot, y_break = 0.4)
ggsave(filename = file.path(save_folder, paste0('waa_', this_ecov,'.jpg')), plot = p6, 
       width = 190 , height = 240, units = 'mm', dpi = 500)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Selex parameters (only for LP and Ecov scenarios):
# temp = sel_df  
# # Set EM and OM labels:
# temp = set_labels(temp)
# # Set par labels:
# temp = temp %>% filter(method %in% c('LP', 'LEc')) # expression(sigma[X]^2), expression(rho[X]), 
# 
# # Make plot:
# p1 = ggplot(temp, aes(x=em_label, y=rel_error, fill=data_scen)) +
#   geom_violin(position=position_dodge(0.4), alpha = 0.6, color = NA) +
#   scale_fill_brewer(palette = "Set1") +
#   theme_bw() +
#   coord_cartesian(ylim = 0.5*c(-1, 1)) +
#   geom_hline(yintercept=0, color=1, linetype='dashed') +
#   theme(legend.position = 'none',
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         strip.text = element_text(size = 10)) +
#   scale_y_continuous(breaks=0.5*c(-1, 0, 1)) +
#   xlab(NULL) + ylab('Relative error') +
#   facet_nested(par2+Ecov_sim ~ om_label, labeller = 'label_parsed')
# 
# ggsave(filename = file.path(save_folder, 'ecov_1.jpg'),  plot = p1,
#        width = 190 , height = 140, units = 'mm', dpi = 500)
