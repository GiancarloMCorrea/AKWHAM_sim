## Cole made this custom file to quickly look at results for
## R&D. Note the script to run things has modified code w/ what
## to return

library(ggplot2)
library(dplyr)
library(tidyr)
theme_set(theme_bw())

# Clean workspace
rm(list = ls())

# Call aux functions
source('aux_functions.R')

# Read outputs rds and save them:
# WARNING: this could take up to hours depending on the number of scenarios and replicates
# fits = list.files('results', pattern='RDS', recursive=1, full.names=TRUE) %>% lapply(readRDS)
# saveRDS(fits, 'results/fits.RDS')

# Read merged output:
fits = readRDS("results/fits.RDS")

# check convergence stats
models <- lapply(fits, function(x) x$model) %>% bind_rows 
group_by(models, om, em) %>%
  summarize(pct.converged=mean(optimized), n.converged=sum(optimized), n.run=length(optimized))


# Quick exploration via plots ---------------------------------------------

# Time series: SSB, F, and Rec:
ts <- get_ts(fits)

# Filter: only converged replicates
ts2 = ts %>% filter(abs(maxgrad)<1)

g = ggplot(ts2, aes(year,y = rel_error)) + 
  facet_grid(par ~ om + em)+ 
  geom_hline(yintercept=0, color=2) +
  xlab(NULL) + ylab('Relative error') +
  coord_cartesian(ylim = 0.5*c(-1,1)) +
  theme(strip.background = element_blank())
g2 = add_ci(g, ci=c(.5,.95), alpha=c(.4,.4), fill = '#606060', showMedian = TRUE)
ggsave("plots/RE_ts_1.jpg", g2, width = 190 , height = 150, units = 'mm', dpi = 500)

# Parameters: Q, M, log_F1, and mean_rec
pars <- get_pars(fits) %>% filter(abs(maxgrad)<1)


g <- ggplot(pars, aes(par2, rel_error)) +
  geom_hline(yintercept=0, col=2, lwd=1) + #ylim(-3,3)+
  geom_violin() +
    geom_jitter(width=.3, height=0, alpha=.3) +
  #geom_jitter(width=.1, height=0, alpha=.5, aes(color=abs(maxgrad)>1)) +
  coord_cartesian(ylim=c(-1,1))+
  facet_grid(om + em~par, scales='free') +
  theme(axis.text.x = element_text(angle = 90)) + labs(x=NULL) +
  theme(panel.spacing = unit(0, "cm"))
g
ggsave("../plots/relerror_pars.png", g, width=10 , height=10)

# ecov <- filter(pars, grepl(x=par, pattern='Ecov'))
# g <- ggplot(ecov, aes(emf, rel_error)) +
#   geom_hline(yintercept=0, col=2, lwd=1) + #ylim(-3,3)+
#   geom_violin() +
#   geom_jitter(width=.3, height=0, alpha=.3) +
#   coord_cartesian(ylim=c(-1,1))+
#   facet_grid(omf~par2, scales='free')+
#   theme(axis.text.x = element_text(angle = 90)) + labs(x=NULL)
# ## g <- ggplot(ecov, aes(par2, rel_error)) +
# ##   geom_hline(yintercept=0, col=2, lwd=1) + #ylim(-3,3)+
# ##   geom_violin() +
# ##   coord_cartesian(ylim=c(-1,1))+
# ##   facet_grid(omf+emf~par, scales='free')
# ggsave("../plots/relerror_pars_ecov.png", g, width=6 , height=4)

# growthpars <- filter(pars, grepl(x=par, pattern='growth_a'))
# g <- ggplot(growthpars, aes(emf, rel_error)) +
#   geom_hline(yintercept=0, col=2, lwd=1) + #ylim(-3,3)+
#   geom_violin() +
#   geom_jitter(width=.3, height=0, alpha=.3) +
#   coord_cartesian(ylim=c(-1,1))+
#   facet_grid(omf~par2, scales='free')+
#   theme(axis.text.x = element_text(angle = 90)) + labs(x=NULL)
# ggsave("../plots/relerror_pars_growth.png", g, width=5 , height=4)
## test <- filter(ts, par=='SSB' & sim==1 & em==2)
## plot(test$year, test$truth)
## lines(test$year, test$est)


# selex <- get_selex(fits) %>% filter(abs(maxgrad)<1)
# ggplot(selex, aes(age, est, group=sim)) +
#   geom_line() + facet_grid(par~emf+om)
# g <- ggplot(selex, aes(age, abs_error, group=sim)) + geom_line(alpha=.5) +
#   geom_hline(yintercept=0, col=2, lwd=1) +
#   facet_grid(par~omf+emf)
# ggsave("../plots/abserror_selex.png", g, width=12 , height=5)

# growth <- get_growth(fits) %>% filter(abs(maxgrad)<1)
# ## ggplot(growth, aes(par, abs_error)) + geom_violin() +
# ##   facet_grid(.~em_growth_est) + geom_hline(yintercept=0, col=2)
# g <- ggplot(growth, aes(emf, rel_error)) +
#   geom_hline(yintercept=0, col=2) +
#   geom_violin() +
#   geom_jitter(width=.3, height=0, alpha=.3) +
#   facet_grid(par~omf) +
#   coord_cartesian(ylim=c(-1,1))+
#   theme(axis.text.x = element_text(angle = 90)) + labs(x=NULL)
# ggsave("../plots/abserror_growth.png", g, width=7 , height=5)


## ## need to get this by year too
## waa <- get_waa(fits) %>% filter(abs(maxgrad)<1)
## g <- ggplot(waa, aes(age, rel_error, group=sim)) + geom_line() +
##   facet_grid(omf~emf)+
##   geom_hline(yintercept=0, col=2) + labs(y='rel_error WAA')
## g <- ggplot(waa, aes(age, abs_error, group=sim)) + geom_line() +
##   facet_grid(omf~emf)+
##   geom_hline(yintercept=0, col=2) + labs(y='abs_error WAA')
## ggsave("../plots/abserror_waa.png", g, width=7 , height=5)

waa <- get_waa(fits) %>% filter(abs(maxgrad)<1)
tmp <- filter(waa, year==52) # terminal year

g <- ggplot(tmp, aes(age, rel_error, group=sim)) + geom_line() +
  facet_grid(om~em)+
  geom_hline(yintercept=0, col=2) + labs(y='rel_error WAA')
ggsave("plots/relerror_waa_terminal_year.png", g, width=7 , height=5)

# g <- ggplot(tmp, aes(age, abs_error, group=sim)) + geom_line() +
#   facet_grid(omf~emf)+
#   geom_hline(yintercept=0, col=2) + labs(y='abs_error WAA')
# ggsave("plots/abserror_waa_terminal_year.png", g, width=7 , height=5)
## by year and age, mre= median relative error
tmp <- waa %>% group_by(om, em, age, year) %>% summarize(n=n(), mre=median(rel_error))
g <- ggplot(tmp, aes(year, age, size=abs(mre), color=mre>0)) +
  geom_point(alpha=.5) +
  facet_grid(om~em)
ggsave("plots/waa_year_by_age.png", g, width=10 , height=5)

# laa <- get_laa(fits) %>% filter(abs(maxgrad)<1)
# tmp <- filter(laa, year==40) # terminal year
# g <- ggplot(tmp, aes(age, rel_error, group=sim)) + geom_line() +
#   facet_grid(omf~emf)+
#   geom_hline(yintercept=0, col=2) + labs(y='rel_error LAA')
# g <- ggplot(tmp, aes(age, abs_error, group=sim)) + geom_line() +
#   facet_grid(omf~emf)+
#   geom_hline(yintercept=0, col=2) + labs(y='abs_error LAA')
# ggsave("../plots/abserror_laa_terminal_year.png", g, width=7 , height=5)
# ## by year and age, mre= median relative error
# tmp <- laa %>% group_by(omf, emf, age, year) %>% summarize(n=n(), mre=median(rel_error))
# g <- ggplot(tmp, aes(year, age, size=abs(mre), color=mre>0)) +
#   geom_point(alpha=.5) +
#   facet_grid(omf~emf)
# ggsave("../plots/laa_year_by_age.png", g, width=10 , height=5)
