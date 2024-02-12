# Code to make figures

library(ggplot2)
library(dplyr)
library(plyr)
library(tidyr)
library(reshape2)
require(ggh4x)
theme_set(theme_bw())

# Clean workspace
rm(list = ls())

# Call aux functions
source('aux_functions.R')
source(file.path('code', 'config_params.R'))
seeds = readRDS(file.path("inputs","seeds.RDS"))

# Save folder:
save_folder = 'plots'


# -------------------------------------------------------------------------
# Make diagram sampling process:

require(DiagrammeR)
require(DiagrammeRsvg)
require(rsvg)

diag1 = DiagrammeR::grViz("digraph {

graph [layout = dot, rankdir = TB];

# define the global styles of the nodes. We can override these in box if we wish
node [shape = rectangle, style = filled, fontsize=25];

POP1 [label = 'Predicted \n age-length structure', fillcolor = Pink];
POP3 [label = 'Weight-at-length', fillcolor = Pink];
SAMP1 [label =  'Length sample', fillcolor = lightskyblue2];
SAMP2 [label =  'Age subsample', fillcolor = lightskyblue2];
DAT1 [label =  'Marginal length \n composition', fillcolor = Beige, style = rounded];
DAT2 [label =  'Conditional \n age-at-length', fillcolor = Beige, style = rounded];
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
SAMP1 -> {SAMP2}[label='sample (either random \n or length-stratified)'];
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
  jpeg::writeJPEG("plots/Figure_2.jpg", quality = 1)

# Now you have to modify the DPI using GIMP. Load the jpg file just created and go to
# Image > Scale Image, and change resolution (px/in) to 500

# -------------------------------------------------------------------------
# Supp figure: selectivity, phi matrix, F trajectory

fish_lengths = seq(from = 2, to = 130, by = 2)
om_sim = readRDS(file = 'inputs/om_sample/om_sample_1.RDS')

jpeg(filename = 'plots/Figure_1.jpg', width = 190, height = 60, units = 'mm', res = 500)
par(mfrow = c(1,3))
# Selectivity:
fish_sel = om_sim$rep$selAL[[1]][1,]
surv_sel = om_sim$rep$selAL[[2]][1,]
par(mar = c(4,4,1,1))
plot(fish_lengths, fish_sel, type = 'l', xlab = 'Length (cm)', ylab = 'Selectivity', ylim = c(0,1))
lines(fish_lengths, surv_sel, col = 2)
text(x = 2, y = 1, labels = "A", xpd = NA, cex = 1.5)
legend('bottomright', legend = c('Fishery', 'Survey'), col = c(1,2), lwd = 1)
# Fishery mortality
f_vector = om_sim$rep$F[,1]
par(mar = c(4,4,1,1))
plot(1:length(f_vector), f_vector, type = 'l', xlab = 'Simulated years', ylab = 'Fishing mortality (F)')
text(x = 1, y = 0.35, labels = "B", xpd = NA, cex = 1.5)
# Phi matrix
phi_matrix = om_sim$rep$jan1_phi_mat[,,1]
par(mar = c(4,4,1,5))
image(phi_matrix, axes=FALSE, col='transparent', xlab = '', ylab = 'Length (cm)', 
      main = NULL)
axis(1, at = seq(from = 0, to = 1, length.out = ncol(phi_matrix)), labels = 1:ncol(phi_matrix))
axis(2, at = seq(from = 0, to = 1, length.out = length(fish_lengths)), labels = fish_lengths)
mtext(text = 'Age', side = 1, line = 3, cex = 0.8)
fields::image.plot(t(phi_matrix), add=T, legend.mar = 6, col = rev(viridis::viridis(100)))
text(x = 0.04, y = 1, labels = "C", xpd = NA, cex = 1.5)
box()
dev.off()

# -------------------------------------------------------------------------
# Supp figure: simulated environmental time series:

n_sim = 10 # number of replicates to plot
n_years = 55

save_stationary = matrix(0, ncol= n_sim, nrow = n_years)
save_trend = save_stationary
# Stationary time series:
for(iter in 1:n_sim) {
  
  set.seed(seeds[iter])
  ecov_error = rnorm(n_years, mean = 0, sd = exp(Ecov_re_sig))
  alpha = 0
  beta = Ecov_trend[1] # trend
  theta = -1 + 2/(1 + exp(-Ecov_re_cor)) # as in WHAM
  sim_ecov = 0
  for(i in 2:length(ecov_error)) sim_ecov[i] = alpha+beta*i+theta*sim_ecov[i-1] + ecov_error[i]
  sim_ecov = scale(sim_ecov)
  save_stationary[,iter] = sim_ecov[,1]
  
  # Nonstationary time series:
  set.seed(seeds[iter])
  ecov_error = rnorm(n_years, mean = 0, sd = exp(Ecov_re_sig))
  alpha = 0
  beta = Ecov_trend[2] # trend
  theta = -1 + 2/(1 + exp(-Ecov_re_cor)) # as in WHAM
  sim_ecov = 0
  for(i in 2:length(ecov_error)) sim_ecov[i] = alpha+beta*i+theta*sim_ecov[i-1] + ecov_error[i]
  sim_ecov = scale(sim_ecov)
  save_trend[,iter] = sim_ecov[,1]
  
}

df1 = melt(save_stationary, varnames = c('year', 'iter'))
df1 = df1 %>% mutate(type = 'Stationary')
df2 = melt(save_trend, varnames = c('year', 'iter'))
df2 = df2 %>% mutate(type = 'Trend')

df_plot = rbind(df1, df2)

figs1 = ggplot(df_plot, aes(x=year, y=value, group = factor(iter))) +
  geom_vline( xintercept = 10, linetype = 'dashed') +
  geom_line(aes(color = factor(type)), alpha = 0.5) +
  xlab('Simulated year') +
  ylab('Simulated environmental covariate') +
  theme(legend.position = 'none') +
  facet_wrap(. ~ factor(type)) 
ggsave(filename = 'plots/Figure_S1.jpg', plot = figs1, 
       width = 190 , height = 90, units = 'mm', dpi = 500)

# -------------------------------------------------------------------------
# Supp figure: simulated variability in LAA:
# WARNING: you need to run the previous plot (Ecov sim)

all_files = list.files(path = 'sample_data/LAA_sample')

all_df = list()
for(k in seq_along(all_files)) {
  all_df[[k]] = readRDS(file = file.path('sample_data/LAA_sample', all_files[k]))
}

all_df = dplyr::bind_rows(all_df)

#scenj = 114, simi = 89
#scenj = 114, simi = 6

all_df = all_df %>% mutate(om_label = factor(growth_par, levels = 0:3,
                                           labels = c('Time~invariant', Variability~"in"~k, 
                                                      expression(Variability~"in"~L[infinity]), expression(Variability~"in"~L[1]))))
all_df = all_df %>% mutate(ecov = factor(ecov, levels = c('stationary', 'trend'),
                                         labels = c('Stationary', 'Trend')))
  
figs2 = ggplot(all_df, aes(x=year, y=value, group = factor(sim))) +
  geom_vline( xintercept = 10, linetype = 'dashed') +
  geom_line(aes(color = factor(ecov)), alpha = 0.2) +
  xlab('Simulated year') +
  ylab('Mean length (cm)') +
  theme(legend.position = 'none') +
  facet_nested(age ~ om_label+ecov, scales = 'free_y', labeller = 'label_parsed')
ggsave(filename = 'plots/Figure_S2.jpg', plot = figs2,
       width = 190 , height = 230, units = 'mm', dpi = 500)

# -------------------------------------------------------------------------
# Sup figure: Impact of length-based selectivity and sampling

year = 1 #select year to plot
om_sim = readRDS(file = 'inputs/om_sample/om_sample_1.RDS')

fish_lengths = om_sim$input$data$lengths
n_years = om_sim$input$data$n_years_model
n_ages = om_sim$input$data$n_ages

this_dist = t(om_sim$rep$NAA[year,] * t(om_sim$rep$jan1_phi_mat[,,year]))
rownames(this_dist) = fish_lengths
colnames(this_dist) = 1:n_ages
df1 = melt(this_dist, varnames = c('len', 'age'))
df1 = df1 %>% mutate(type = 'Population')

this_dist = om_sim$rep$pred_CAAL[year, 1, ,]
rownames(this_dist) = fish_lengths
colnames(this_dist) = 1:n_ages
df2 = melt(this_dist, varnames = c('len', 'age'))
df2 = df2 %>% mutate(type = 'Fishery')

this_dist = om_sim$rep$pred_IAAL[year, 1, ,]
rownames(this_dist) = fish_lengths
colnames(this_dist) = 1:n_ages
df3 = melt(this_dist, varnames = c('len', 'age'))
df3 = df3 %>% mutate(type = 'Survey')

df_plot = rbind(df1, df2, df3)
df_plot$type = factor(df_plot$type, levels = c('Population', 'Fishery', 'Survey'))
mean_plot = df_plot %>% 
  dplyr::group_by(age, type) %>%
  dplyr::summarise(mean_len = weighted.mean(x = len, w = value))

figs3 = ggplot(df_plot, aes(x=len, y=value)) +
  geom_line() +
  xlab('Length (cm)') +
  ylab('Abundance') +
  geom_vline(data = mean_plot, aes(xintercept = mean_len), color = 'red') +
  facet_grid(type ~ factor(age), scales = 'free_y') +
  theme(legend.position = 'none',
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
ggsave(filename = 'plots/Figure_S3.jpg', plot = figs3, 
       width = 190 , height = 90, units = 'mm', dpi = 500)
