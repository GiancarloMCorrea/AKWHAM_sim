# Code to make figures

library(ggplot2)
library(dplyr)
library(plyr)
library(tidyr)
theme_set(theme_bw())

# Clean workspace
rm(list = ls())

# Call aux functions
source('aux_functions.R')

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
# Supp figure: mean length-at-age variability by 
om_sim1 = readRDS(file = 'inputs/om_sample_1.RDS')
om_sim2 = readRDS(file = 'inputs/om_sample_2.RDS')
om_sim3 = readRDS(file = 'inputs/om_sample_3.RDS')
om_sim4 = readRDS(file = 'inputs/om_sample_4.RDS')

laa_1 = om_sim1$rep$LAA
laa_2 = om_sim2$rep$LAA
laa_3 = om_sim3$rep$LAA
laa_4 = om_sim4$rep$LAA

# Merge:
plot_df = rbind(reshape2::melt(laa_1) %>% mutate(type = '1'), reshape2::melt(laa_2) %>% mutate(type = '2'), 
                reshape2::melt(laa_3) %>% mutate(type = '3'), reshape2::melt(laa_4) %>% mutate(type = '4'))
plot_df = plot_df %>% mutate(Var2 = factor(Var2, levels = 1:10),
                              type = factor(type, levels = 1:4,
                                         labels = c('Time~invariant', Variability~"in"~k, expression(Variability~"in"~L[infinity]), expression(Variability~"in"~L[1]))))

figs2 = ggplot(plot_df, aes(x=Var1, y=value, color = Var2)) +
  geom_line() +
  theme_bw() +
  coord_cartesian(ylim = c(0, 120)) +
  theme(legend.position = 'bottom',
        strip.text = element_text(size = 12)) +
  xlab('Simulated years') + ylab('Population mean length (cm)') +
  guides(color = guide_legend(title='Ages')) +
  scale_color_viridis_d() +
  facet_wrap(. ~ type, labeller = my_label_parsed) 
ggsave(filename = 'plots/Figure_S2.jpg', plot = figs2, 
       width = 190 , height = 180, units = 'mm', dpi = 500)
