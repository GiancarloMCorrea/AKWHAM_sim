# Function to make plot OM:
make_plot_om = function(om_toPlot, i, main_dir) {
  these_years = om_toPlot$year1_model:(om_toPlot$year1_model + om_toPlot$n_years_model - 1)
  these_ages = 1:om_toPlot$n_ages
  these_lengths = om_toPlot$lengths
  # Biology
  jpeg(filename = file.path(main_dir, 'plots', paste0('OM_biology_', i,'.jpg')), width = 190, 
       height = 160, units = 'mm', res = 500)
  par(mfrow = c(3,3))
  par(mar = c(2,4,1,1))
  plot(these_years, om_toPlot$SSB, type = 'b', xlab = NULL, ylab = 'SSB')
  par(mar = c(2,4,1,1))
  plot(these_years, om_toPlot$NAA[,1], type = 'b', xlab = NULL, ylab = 'Recruitment')
  par(mar = c(2,4,1,1))
  plot(these_years, om_toPlot$pred_catch[,1], type = 'b', xlab = NULL, ylab = 'Catch')
  par(mar = c(2,4,1,1))
  plot(these_years, om_toPlot$F[,1], type = 'b', xlab = NULL, ylab = 'F')
  par(mar = c(2,4,1,1))
  plot(these_ages, om_toPlot$mature[1,], type = 'b', xlab = NULL, ylab = 'Maturity')
  par(mar = c(2,4,1,1))
  matplot(om_toPlot$ssb_phi_mat[1,,], type = 'l', ylab = 'SSB phi matrix (year 1)')
  par(mar = c(2,4,1,1))
  matplot(om_toPlot$LAA, type = 'l', ylab = 'Mean length at age (Jan 1st)')
  par(mar = c(2,4,1,1))
  matplot(om_toPlot$pred_waa[2,,], type = 'l', ylab = 'Mean weight at age (Jan 1st)')
  par(mar = c(2,4,1,1))
  plot(these_years, om_toPlot$Ecov_out[,4,1], type = 'b', xlab = NULL, ylab = 'Ecov_out')
  dev.off()
  
  # Selectivity:
  jpeg(filename = file.path(main_dir, 'plots', paste0('OM_selectivity_', i,'.jpg')), width = 190, 
       height = 160, units = 'mm', res = 500)
  par(mfrow = c(2,2))
  par(mar = c(2,4,1,1))
  plot(these_ages, om_toPlot$selAA[[1]][1,], type = 'b', xlab = NULL, ylab = 'Selectivity at age (Fishery)', ylim = c(0,1))
  par(mar = c(2,4,1,1))
  plot(these_ages, om_toPlot$selAA[[2]][1,], type = 'b', xlab = NULL, ylab = 'Selectivity at age (Survey)', ylim = c(0,1))    
  par(mar = c(2,4,1,1))
  plot(these_lengths, om_toPlot$selLL[[1]][1,], type = 'b', xlab = NULL, ylab = 'Selectivity at length (Fishery)', ylim = c(0,1))
  par(mar = c(2,4,1,1))
  plot(these_lengths, om_toPlot$selLL[[2]][1,], type = 'b', xlab = NULL, ylab = 'Selectivity at length (Survey)', ylim = c(0,1))     
  dev.off()
}
