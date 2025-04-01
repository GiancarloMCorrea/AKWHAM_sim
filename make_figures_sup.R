# Code to make other figures:
library(ggplot2)
library(dplyr)
library(plyr)
library(tidyr)
library(reshape2)
require(ggh4x)
library(fields)
require(DiagrammeR)
library(scales)
require(DiagrammeRsvg)
require(gridExtra)
require(rsvg)
require(wesanderson)
library(RColorBrewer)
theme_set(theme_bw())

# Clean workspace
rm(list = ls())

# Call aux functions
source('aux_functions.R')
source(file.path('code', 'config_params.R'))
seeds = readRDS(file.path("inputs","seeds.RDS"))
df.scenarios = readRDS(file.path("inputs","df.scenarios.RDS"))

# Save folder:
save_folder = 'plots'
fig_type = '.png'
img_res = 400
img_width = 170

# color scale for EBS and MAB:
colpal1 = wesanderson::wes_palettes$GrandBudapest1[3:4]
# color scale for RS and LSS:
colpal2 = wesanderson::wes_palettes$Darjeeling1[1:2]

# -------------------------------------------------------------------------
# Make diagram stepwise sampling process:

diag1 = DiagrammeR::grViz("digraph {

graph [layout = dot, rankdir = TB];

# define the global styles of the nodes. We can override these in box if we wish
node [shape = rectangle, style = filled, fontsize=25];

POP1 [label = 'Predicted \n age-length structure', fillcolor = Pink];
POP3 [label = 'Weight-at-length', fillcolor = Pink];
SAMP1 [label =  'Length sample', fillcolor = lightskyblue2];
SAMP2 [label =  'Age subsample', fillcolor = lightskyblue2];
DAT1 [label =  'Marginal length \n composition', fillcolor = lightskyblue2];
DAT2 [label =  'Conditional \n age-at-length', fillcolor = lightskyblue2];
DAT3 [label =  'Marginal age \n composition', fillcolor = Beige, style = rounded];
DAT4 [label =  'Mean weight-at-age', fillcolor = Beige, style = rounded];

{rank = min; POP1};
{rank = same; SAMP1 SAMP2};
{rank = same; DAT1 DAT2 POP3};
{rank = max; DAT3 DAT4};

# edge definitions with the node IDs
d1 [shape=point,width=0.01,height=0.01];
d2 [shape=point,width=0.01,height=0.01];
d3 [shape=point,width=0.01,height=0.01];
POP1 -> {SAMP1}[label='random \n sample'];
SAMP1 -> {DAT1};
SAMP1 -> {SAMP2}[label='sample (either RS or LSS)'];
SAMP2 -> DAT2;
{DAT1 DAT2}->d2[dir=none];
d2->DAT3;
{DAT2 POP3}->d3[dir=none];
d3->DAT4;

}")


# Save:
DPI = 500
WidthCM = 17
HeightCM = 10

diag1 %>% export_svg %>% charToRaw %>% 
  rsvg(width = WidthCM *(DPI/2.54), height = HeightCM *(DPI/2.54)) %>% 
  png::writePNG("plots/Figure_samp_step.png")

# Now you have to modify the DPI using GIMP. Load the jpg file just created and go to
# Image > Scale Image, and change resolution (px/in) to 500

# -------------------------------------------------------------------------
# Make diagram Traditional scenarios:

diag2 = DiagrammeR::grViz("digraph {

graph [layout = dot, rankdir = TB];

# define the global styles of the nodes. We can override these in box if we wish
node [shape = rectangle, style = filled, fontsize=25];

LEV1 [label = 'Traditional', fillcolor = lightskyblue2];
LEV2 [label = 'Stepwise', fillcolor = lightskyblue2];

OM1 [label = 'Time invariant', fillcolor = Pink];
OM3 [label = 'Variability in L\U2081: \n simulated from EBS or \n MAB', fillcolor = Pink];
OM2 [label = 'Variability in k/L\u221e: \n simulated from EBS or \n MAB', fillcolor = Pink];

DAT1 [label = 'Age sampling: \n RS or LSS', fillcolor = DarkSeaGreen];
DAT2 [label = 'Age sampling: \n RS or LSS', fillcolor = DarkSeaGreen];

EM1 [label =  'Random effects on WAA: \n WEm \n WNP-iid \n WNP-2D \n WNP-3D', fillcolor = Beige];
EM2 [label =  'Random effects on WAA: \n WEm \n WNP-iid \n WNP-2D \n WNP-3D', fillcolor = Beige];
EM3 [label =  'Selectivity: \n constant (Const) or \n time-varying (Vary)', fillcolor = Beige];

{rank = min; LEV1};
{rank = same; OM1 OM2 OM3};
{rank = same; DAT1 DAT2};
{rank = same; EM1 EM2};

# edge definitions with the node IDs
{LEV1} -> {OM1 OM2 OM3};
{LEV2} -> {OM1 OM2 OM3}[style=dashed];
{OM1} -> {DAT1}[style=dashed];
{OM2 OM3} -> {DAT2}[style=dashed];
DAT1 -> {EM1}[style=dashed];
DAT2 -> {EM2}[style=dashed];
OM1 -> {EM1};
{OM2 OM3} -> {EM2};
EM2 -> EM3[dir=both];
EM2 -> EM3[dir=both, style=dashed];

}", engine = "dot")


# Save:
DPI = 500
WidthCM = 17
HeightCM = 10

diag2 %>% export_svg %>% charToRaw %>% 
  rsvg(width = WidthCM *(DPI/2.54), height = HeightCM *(DPI/2.54)) %>% 
  png::writePNG("plots/Figure_sim.png")

# Now you have to modify the DPI using GIMP. Load the jpg file just created and go to
# Image > Scale Image, and change resolution (px/in) to 500

# -------------------------------------------------------------------------
# Supp figure: selectivity, phi matrix, F trajectory

fish_lengths = lengths_base
n_years = n_years_base+n_years_burnin
om_sim1 = readRDS(file = 'sample_data/om_sample/om_sample_1.RDS')
om_sim2 = readRDS(file = 'sample_data/om_sample/om_sample_37.RDS')
env_data = readRDS(file = 'env_data/env_sim.rds')
cex_lab = 0.8

png(filename = 'plots/Figure_config.png', width = 170, height = 210, units = 'mm', res = 400)
par(mfrow = c(3,2))

# Selectivity (age based, traditional):
fish_sel = om_sim1$rep$selAL[[1]][1,]
surv_sel = om_sim1$rep$selAL[[2]][1,]
par(mar = c(3,3.5,0.8,0.5))
plot(ages_base, fish_sel, type = 'l', xlab = '', ylab = '', ylim = c(0,1))
lines(ages_base, surv_sel, lty = 2)
text(x = 1, y = 1, labels = "A", xpd = NA, cex = 1.5)
mtext(text = 'Age (years)', side = 1, line = 2, cex = cex_lab)
mtext(text = 'Selectivity', side = 2, line = 2.25, cex = cex_lab)
legend('bottomright', legend = c('Fishery', 'Survey'), lty = c(1,2), lwd = 1, bty = 'n')

# Selectivity (size based, stepwise):
fish_sel = om_sim2$rep$selAL[[1]][1,]
surv_sel = om_sim2$rep$selAL[[2]][1,]
par(mar = c(3,3.5,0.8,0.5))
plot(fish_lengths, fish_sel, type = 'l', xlab = '', ylab = '', ylim = c(0,1))
lines(fish_lengths, surv_sel, lty = 2)
text(x = 2, y = 1, labels = "B", xpd = NA, cex = 1.5)
mtext(text = 'Length (cm)', side = 1, line = 2, cex = cex_lab)
mtext(text = 'Selectivity', side = 2, line = 2.25, cex = cex_lab)
legend('bottomright', legend = c('Fishery', 'Survey'), lty = c(1,2), lwd = 1, bty = 'n')

# Fishery mortality
f_vector = om_sim1$rep$F[,1]
par(mar = c(3,3.5,0.8,0.5))
plot(NA, NA, xlab = '', ylab = '', xlim = c(1, n_years),
     ylim = c(0, F_max))
polygon(x = c(-10, 10, 10, -10, -10), y = c(-1, -1, 1, 1, -1), col = 'grey', border = NA)
lines(1:length(f_vector), f_vector)
text(x = 1, y = F_max, labels = "C", xpd = NA, cex = 1.5)
mtext(text = 'Simulated years', side = 1, line = 2, cex = cex_lab)
mtext(text = 'Fishing mortality (F)', side = 2, line = 2.25, cex = cex_lab)
box()

# Ecov time series: EBS
par(mar = c(3,3.5,0.8,0.5))
plot(NA, NA, xlab = '', ylab = '', 
     ylim = c(-3,3), xlim = c(1, n_years))
polygon(x = c(-10, 10, 10, -10, -10), y = c(-10, -10, 10, 10, -10), col = 'grey', border = NA)
ts_1 = env_data %>% dplyr::filter(type == 'stationary')
lines(1:n_years, c(rnorm(n = n_years_burnin, mean = 0, sd = 1), ts_1$var_std), lwd = 0.5, col = colpal1[1]) # double check with sim_core.R
trend = lm(var_std ~ year_id, data = ts_1)
lines((n_years_burnin+1):n_years, predict(trend), lwd = 0.5, lty = 2)
text(x = 1, y = 3, labels = "D", xpd = NA, cex = 1.5)
mtext(text = 'Simulated years', side = 1, line = 2, cex = cex_lab)
mtext(text = 'EBS index', side = 2, line = 2.25, cex = cex_lab)
box()

# Ecov time series: MAB
par(mar = c(3,3.5,0.8,0.5))
plot(NA, NA, xlab = '', ylab = '', 
     ylim = c(-3,3), xlim = c(1, n_years))
polygon(x = c(-10, 10, 10, -10, -10), y = c(-10, -10, 10, 10, -10), col = 'grey', border = NA)
ts_1 = env_data %>% dplyr::filter(type == 'trend')
lines(1:n_years, c(rnorm(n = n_years_burnin, mean = 0, sd = 1), ts_1$var_std), lwd = 0.5, col = colpal1[2]) # double check with sim_core.R
trend = lm(var_std ~ year_id, data = ts_1)
lines((n_years_burnin+1):n_years, predict(trend), lwd = 0.5, lty = 2)
text(x = 1, y = 3, labels = "E", xpd = NA, cex = 1.5)
mtext(text = 'Simulated years', side = 1, line = 2, cex = cex_lab)
mtext(text = 'MAB index', side = 2, line = 2.25, cex = cex_lab)
box()

# Phi matrix
phi_matrix = om_sim1$rep$jan1_phi_mat[,,1]
par(mar = c(6,3.5,0.8,0.5))
image(phi_matrix, axes=FALSE, col='transparent', xlab = '', ylab = '', 
      main = NULL)
axis(1, at = seq(from = 0, to = 1, length.out = ncol(phi_matrix)), labels = 1:ncol(phi_matrix))
axis(2, at = seq(from = 0, to = 1, length.out = length(fish_lengths)), labels = fish_lengths)
fields::image.plot(t(phi_matrix), add=T, horizontal = TRUE,
                   col = rev(viridis::viridis(100)), legend.mar = 3.5)
mtext(text = 'Age', side = 1, line = 2, cex = cex_lab)
mtext(text = 'Length (cm)', side = 2, line = 2.25, cex = cex_lab)
text(x = 0.04, y = 1, labels = "F", xpd = NA, cex = 1.5)
box()


dev.off()

# # -------------------------------------------------------------------------
# # Supp figure: simulated environmental time series:
# 
# n_sim = 10 # number of replicates to plot
# n_years = 55
# 
# save_stationary = matrix(0, ncol= n_sim, nrow = n_years)
# save_trend = save_stationary
# # Stationary time series:
# for(iter in 1:n_sim) {
#   
#   set.seed(seeds[iter])
#   ecov_error = rnorm(n_years, mean = 0, sd = exp(Ecov_re_sig))
#   alpha = 0
#   beta = Ecov_trend[1] # trend
#   theta = -1 + 2/(1 + exp(-Ecov_re_cor)) # as in WHAM
#   sim_ecov = 0
#   for(i in 2:length(ecov_error)) sim_ecov[i] = alpha+beta*i+theta*sim_ecov[i-1] + ecov_error[i]
#   sim_ecov = scale(sim_ecov)
#   save_stationary[,iter] = sim_ecov[,1]
#   
#   # # Nonstationary time series:
#   # set.seed(seeds[iter])
#   # ecov_error = rnorm(n_years, mean = 0, sd = exp(Ecov_re_sig))
#   # alpha = 0
#   # beta = Ecov_trend[2] # trend
#   # theta = -1 + 2/(1 + exp(-Ecov_re_cor)) # as in WHAM
#   # sim_ecov = 0
#   # for(i in 2:length(ecov_error)) sim_ecov[i] = alpha+beta*i+theta*sim_ecov[i-1] + ecov_error[i]
#   # sim_ecov = scale(sim_ecov)
#   # save_trend[,iter] = sim_ecov[,1]
#   
# }
# 
# df1 = melt(save_stationary, varnames = c('year', 'iter'))
# df1 = df1 %>% mutate(type = 'Stationary')
# # df2 = melt(save_trend, varnames = c('year', 'iter'))
# # df2 = df2 %>% mutate(type = 'Trend')
# 
# df_plot = df1
# 
# figs1 = ggplot(df_plot, aes(x=year, y=value, group = factor(iter))) +
#   geom_vline( xintercept = 10, linetype = 'dashed') +
#   geom_line(aes(color = factor(type)), alpha = 0.5) +
#   xlab('Simulated year') +
#   ylab('Simulated environmental covariate') +
#   theme(legend.position = 'none') +
#   facet_wrap(. ~ factor(type)) 
# ggsave(filename = 'plots/Figure_S1.jpg', plot = figs1, 
#        width = 190 , height = 90, units = 'mm', dpi = 500)
# 
# -------------------------------------------------------------------------
# Supp figure: simulated variability in LAA:
# WARNING: you need to run the previous plot (Ecov sim)

all_files = list.files(path = 'sample_data/LAA_sample')

# Read sim data (growth var present):
all_df = list()
for(k in seq_along(all_files)) {
  all_df[[k]] = readRDS(file = file.path('sample_data/LAA_sample', all_files[k]))
}
all_df = dplyr::bind_rows(all_df)

# Merge dfs:
merged_df = all_df

# Prepare for plotting:
merged_df = merged_df %>% mutate(ecov = if_else(growth_var == 0, 'none', ecov))
merged_df = merged_df %>% mutate(om_label = factor(growth_var, levels = 0:2,
                                             labels = c('Time~invariant', Variability~"in"~k*"/"*L[infinity], 
                                                        expression(Variability~"in"~L[1]))),
                                 ecov = factor(ecov, levels = c('stationary', 'trend', 'none'), 
                                               labels = c('EBS', 'MAB', 'None')),
                                 i_group = paste(sim, ecov, sep = '-'))

figs2 = ggplot(merged_df, aes(x=year, y=value, group = factor(i_group), color = ecov)) +
  geom_vline(xintercept = 10, linetype = 'dashed') +
  geom_line(alpha = 1) +
  scale_color_manual(values = c(colpal1, 'gray50')) +
  xlab('Simulated year') +
  ylab('Mean length (cm)') +
  theme_classic() +
  theme(legend.position = 'bottom',
        legend.text = element_text(size = 10)) +
  guides(color = guide_legend(title = NULL)) +
  facet_nested(age ~ om_label, scales = 'free_y', labeller = 'label_parsed')
ggsave(filename = 'plots/Figure_LAA.png', plot = figs2,
       width = 170 , height = 210, units = 'mm', dpi = 500)


# -------------------------------------------------------------------------
# Make figure on stability of X replicates on SSB RE

# TS data:

# Specify these values (see main plot script)
output_folder = 'outputs'
max_grad = 1e-04
min_alpha = 0.35
ts_df = readRDS(file = file.path(output_folder, 'ts_results.RDS'))
paa_gen_approach = 'traditional'

this_age_selex = c('fixed', 'varying') # fixed or varying
this_caal_samp = c('random') # random or strat
this_ecov = c('stationary', 'trend')

# Tidy data:
temp = ts_df %>% filter(paa_generation == paa_gen_approach) %>%
  dplyr::group_by(scenario, par, data_scen, Ecov_sim, caal_samp, age_selex, re_method, 
                  method, growth_var, im) %>% 
  dplyr::summarise(rel_error = median(rel_error), maxgrad = median(maxgrad))
temp = temp %>% dplyr::filter(par == 'SSB')
# Set EM and OM labels:
temp = set_labels(temp, selex_type = this_age_selex, caal_type = this_caal_samp, 
                  ecov_type = this_ecov, conv_level = max_grad)
# Filter first 100 reps:
temp = filter_iter(temp)
# Set par labels:
plot_dat = temp %>% dplyr::group_by(Ecov_sim, caal_samp, age_selex, em_label, om_label) %>%
              group_split()
plot_dat = lapply(plot_dat, function(x) {
  outdf = x %>% mutate(cum_im = 1:nrow(x),
                       cum_re = cumsum(rel_error)/cum_im)
  return(outdf)
})
plot_dat = bind_rows(plot_dat)
plot_dat$em_label2 = factor(plot_dat$em_label, labels = c("WEm", expression(WNP*"-"*iid), 
                                                     expression(WNP*"-"*2*"D"),
                                                     expression(WNP*"-"*3*"D")))

p1 = ggplot(plot_dat, aes(x = cum_im, y = cum_re*100)) +
  geom_line(aes(color = Ecov_sim, linetype = age_selex)) +
  scale_color_manual(values = c(colpal1, 'gray50')) +
  scale_linetype_manual(values = c('solid', 'longdash')) +
  xlab('Number of replicates') +
  ylab('Mean relative error (%)') +
  theme_classic() +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(size = 9, angle = 45, vjust = 1, hjust=1),
        strip.text = element_text(size = 10),
        legend.text=element_text(size=10)) +
  facet_grid(em_label2 ~ om_label, labeller = 'label_parsed') +
  guides(color=guide_legend(title=NULL),
         linetype=guide_legend(title=NULL)
         )
ggsave(filename = file.path(save_folder, paste0(paste(paa_gen_approach, 'iter-stability', sep = '-'), fig_type)), plot = p1,
       width = img_width , height = 210, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# Sup figure: Impact of length-based selectivity and sampling

om_sim1 = readRDS(file = 'sample_data/om_sample/om_sample_37.RDS')

phi_mat1 = om_sim1$rep$catch_phi_mat[,,11] # only first year
rownames(phi_mat1) = lengths_base
colnames(phi_mat1) = ages_base
phi_df1 = reshape2::melt(phi_mat1, varnames = c('len', 'age'))

phi_mat2 = om_sim1$rep$jan1_phi_mat[,,11] # only first year
rownames(phi_mat2) = lengths_base
colnames(phi_mat2) = ages_base
phi_df2 = reshape2::melt(phi_mat2, varnames = c('len', 'age'))

laa_df1 = phi_df1 %>% group_by(age) %>% filter(value == max(value)) %>% 
            mutate(ypos = value + 0.03)
laa_df2 = phi_df2 %>% group_by(age) %>% filter(value == max(value)) %>% 
            mutate(ypos = value + 0.03)
sel_df1 = data.frame(len = lengths_base, value = om_sim1$rep$selAL[[1]][1,], type = 'Fishery')
sel_df2 = data.frame(len = lengths_base, value = om_sim1$rep$selAL[[2]][1,], type = 'Survey')

p1 = ggplot() +
  geom_rect(data = sel_df1, aes(xmin = len, xmax = len+2, ymin = 0, 
                                ymax = max(phi_df1$value) + 0.05, fill=value), alpha = 0.75) +
  geom_line(data = phi_df1, aes(x = len, y = value, group = factor(age))) +
  geom_text(data = laa_df1, aes(x = len, y = ypos, label = age), size = 4) +
  scale_fill_gradientn(colours = rev(terrain.colors(7))) +
  xlab('Length (cm)') + ylab('Proportion') +
  theme_classic() +
  guides(fill = guide_colorbar(title = 'Selectivity')) +
  ggtitle('Fishery')
p2 = ggplot() +
  geom_rect(data = sel_df2, aes(xmin = len, xmax = len+2, ymin = 0, 
                                ymax = max(phi_df2$value) + 0.05, fill=value), alpha = 0.75) +
  geom_line(data = phi_df2, aes(x = len, y = value, group = factor(age))) +
  geom_text(data = laa_df2, aes(x = len, y = ypos, label = age), size = 4) +
  scale_fill_gradientn(colours = rev(terrain.colors(7))) +
  xlab('Length (cm)') + ylab('Proportion') +
  theme_classic() +
  guides(fill = guide_colorbar(title = 'Selectivity')) +
  ggtitle('Survey')

p3 = grid.arrange(p1, p2)
ggsave(filename = file.path(save_folder, paste0('Figure_selex', fig_type)), plot = p3,
       width = img_width*0.75 , height = 150, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# Sup figure: Number of fish sampled ALK by strategy:

all_files = list.files(path = 'sample_data/ALK_sample')
# Read sim data (growth var present):
all_df = list()
for(k in seq_along(all_files)) {
  all_df[[k]] = readRDS(file = file.path('sample_data/ALK_sample', all_files[k]))
}
all_df = dplyr::bind_rows(all_df)

# Aggregate by age:
tmp_df = all_df %>% dplyr::filter(fleet_type == 'survey', year > 10) %>% 
            dplyr::group_by(age, growth_var, sim, ecov, caal_samp) %>% 
            dplyr::summarise(n_fish = sum(value))
# Average over sims:
tmp_df = tmp_df %>% dplyr::group_by(age, growth_var, ecov, caal_samp) %>% 
            dplyr::summarise(n_fish = mean(n_fish))
# Define labels:
tmp_df = tmp_df %>% mutate(om_label = factor(growth_var, levels = 0:2,
                                         labels = c('Time~invariant', Variability~"in"~k*"/"*L[infinity], 
                                                    expression(Variability~"in"~L[1])))) %>%
              mutate(ecov = if_else(growth_var == 0, 'none', ecov)) %>% 
              mutate(ecov = factor(ecov, levels = c('stationary','trend', 'none'), 
                       labels = c('EBS', 'MAB', 'None'))) %>%
              mutate(caal_samp = factor(caal_samp, levels = c('random', 'strat'), 
                                          labels = c('RS', 'LSS')))

# Make plot:
p1 = ggplot(tmp_df, aes(x = factor(age), y = n_fish)) +
  geom_bar(aes(color = caal_samp, fill = caal_samp), 
           stat = "identity", position = "dodge", width = 0.5) +
  scale_color_manual(values = colpal2) +
  scale_fill_manual(values = colpal2) +
  ylab('Number of fish in age subsample') + xlab('Age') +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(size = 9),
        strip.text = element_text(size = 10),
        legend.text=element_text(size=10),
        strip.background = element_rect(fill="white")) +
  facet_grid(ecov ~ om_label, labeller = 'label_parsed', scales = "free_y") +
  guides(colour=guide_legend(title=NULL), fill=guide_legend(title=NULL))
ggsave(filename = file.path(save_folder, paste0('Figure_alksamp', fig_type)), plot = p1,
       width = img_width , height = 150, units = 'mm', dpi = img_res)


# -------------------------------------------------------------------------
# Sup figure: observed mean weight at age data before imputation

all_files = list.files(path = 'sample_data/WAA_sample')
# Read sim data (growth var present):
all_df = list()
for(k in seq_along(all_files)) {
  all_df[[k]] = readRDS(file = file.path('sample_data/WAA_sample', all_files[k]))
}
all_df = dplyr::bind_rows(all_df)

# Aggregate by age:
tmp_df = all_df %>% dplyr::filter(waa_pointer == 2, sim == 1, year > 10) %>% 
  mutate(wt = value, year = year - 10)
# Define labels:
tmp_df = tmp_df %>% mutate(om_label = factor(growth_var, levels = 0:2,
                                             labels = c('Time~invariant', Variability~"in"~k*"/"*L[infinity], 
                                                        expression(Variability~"in"~L[1])))) %>%
  mutate(ecov = if_else(growth_var == 0, 'none', ecov)) %>% 
  mutate(ecov = factor(ecov, levels = c('stationary','trend', 'none'), 
                       labels = c('EBS', 'MAB', 'None'))) %>%
  mutate(caal_samp = factor(caal_samp, levels = c('random', 'strat'), 
                            labels = c('RS', 'LSS')))

# Make plot:
p1 = ggplot(data = tmp_df, aes(x = year, y = factor(age), fill = wt)) +
  geom_tile(color = NA) +
  scale_fill_viridis_c() +
  xlab('Simulated year') + ylab('Age') +
  scale_x_continuous(breaks = seq(from = 10, to = 40, by = 10)) +
  theme(legend.position = 'bottom', 
        axis.text.y = element_text(angle = 0, hjust = 1),
        strip.text = element_text(size = 10),
        strip.background = element_rect(fill="white")) +
  facet_nested(caal_samp ~ om_label+ecov, labeller = 'label_parsed') +
  guides(fill=guide_legend(title='Obs mean weight'))
ggsave(filename = file.path(save_folder, paste0('Figure_waasamp', fig_type)), plot = p1,
       width = img_width , height = 130, units = 'mm', dpi = img_res)
