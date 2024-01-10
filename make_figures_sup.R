# Code to make figures

library(ggplot2)
library(dplyr)
library(plyr)
library(tidyr)
library(reshape2)
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
# Supp figure: selectivity, phi matrix, F trajectory

fish_lengths = seq(from = 2, to = 130, by = 2)
om_sim = readRDS(file = 'inputs/om_sample_1.RDS')

jpeg(filename = 'plots/Figure_S1.jpg', width = 190, height = 160, units = 'mm', res = 500)
par(mfrow = c(2,2))
# Selectivity:
fish_sel = om_sim$rep$selAL[[1]][1,]
surv_sel = om_sim$rep$selAL[[2]][1,]
par(mar = c(4,4,1,1))
plot(fish_lengths, fish_sel, type = 'l', xlab = 'Length (cm)', ylab = 'Fishery selectivity', ylim = c(0,1))
text(x = 2, y = 1, labels = "A", xpd = NA, cex = 1.5)
par(mar = c(4,4,1,1))
plot(fish_lengths, surv_sel, type = 'l', xlab = 'Length (cm)', ylab = 'Survey selectivity', ylim = c(0,1)) 
text(x = 2, y = 1, labels = "B", xpd = NA, cex = 1.5)
# Fishery mortality
f_vector = om_sim$rep$F[,1]
par(mar = c(4,4,1,1))
plot(1:length(f_vector), f_vector, type = 'l', xlab = 'Simulated years', ylab = 'Fishing mortality (F)')
text(x = 1, y = 0.35, labels = "C", xpd = NA, cex = 1.5)
# Phi matrix
phi_matrix = om_sim$rep$phi_mat[1,,]
par(mar = c(4,4,1,5))
image(phi_matrix, axes=FALSE, col='transparent', xlab = '', ylab = 'Length (cm)', 
      main = NULL)
axis(1, at = seq(from = 0, to = 1, length.out = ncol(phi_matrix)), labels = 1:ncol(phi_matrix))
axis(2, at = seq(from = 0, to = 1, length.out = length(fish_lengths)), labels = fish_lengths)
mtext(text = 'Age', side = 1, line = 3)
fields::image.plot(t(phi_matrix), add=T, legend.mar = 6, col = rev(viridis::viridis(100)))
text(x = 0.04, y = 1, labels = "D", xpd = NA, cex = 1.5)
box()
dev.off()

# -------------------------------------------------------------------------
# Supp figure: simulated environmental time series:

n_sim = 100 # number of replicates to plot
n_years = 45

save_stationary = matrix(0, ncol= n_sim, nrow = n_years)
save_trend = save_stationary
# Stationary time series:
for(iter in 1:n_sim) {
  
  set.seed(seeds[iter])
  ecov_error = rnorm(length(years_base), mean = 0, sd = exp(Ecov_re_sig))
  alpha = 1
  beta = Ecov_trend[1] # trend
  theta = -1 + 2/(1 + exp(-Ecov_re_cor)) # as in WHAM
  sim_ecov = 0
  for(i in 2:length(ecov_error)) sim_ecov[i] = alpha+beta*i+theta*sim_ecov[i-1] + ecov_error[i]
  sim_ecov = scale(sim_ecov)
  save_stationary[,iter] = sim_ecov[,1]
  
  # Nonstationary time series:
  set.seed(seeds[iter])
  ecov_error = rnorm(length(years_base), mean = 0, sd = exp(Ecov_re_sig))
  alpha = 1
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

figs2 = ggplot(df_plot, aes(x=year, y=value, group = factor(iter))) +
  geom_line(color = 'gray70') +
  xlab('Simulated year') +
  ylab('Simulated environmental covariate') +
  facet_wrap(. ~ factor(type)) 
ggsave(filename = 'plots/Figure_S2.jpg', plot = figs2, 
       width = 190 , height = 90, units = 'mm', dpi = 500)

# -------------------------------------------------------------------------
# Supp figure: simulated variability in LAA:
# WARNING: you need to run the previous plot (Ecov sim)

# TODO: do it using the 100 replicates from the first 4 scenarios.

# -------------------------------------------------------------------------
# Sup figure: Impact of length-based selectivity and sampling

year = 1 #select year to plot
om_sim = readRDS(file = 'inputs/om_sample_1.RDS')

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

figs4 = ggplot(df_plot, aes(x=len, y=value)) +
  geom_line() +
  xlab('Length (cm)') +
  ylab('Abundance') +
  geom_vline(data = mean_plot, aes(xintercept = mean_len), color = 'red') +
  facet_grid(type ~ factor(age), scales = 'free_y') +
  theme(legend.position = 'none',
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
ggsave(filename = 'plots/Figure_S4.jpg', plot = figs4, 
       width = 190 , height = 90, units = 'mm', dpi = 500)
