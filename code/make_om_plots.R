# Function to make plot OM:
make_plot_om = function(om_toPlot, i, main_dir) {
  these_years = om_toPlot$year1_model:(om_toPlot$year1_model + om_toPlot$n_years_model - 1)
  these_ages = 1:om_toPlot$n_ages
  these_lengths = om_toPlot$lengths
  # Biology
  dir.create(file.path(main_dir, 'plots/config'), recursive = T, showWarnings = FALSE)
  jpeg(filename = file.path(main_dir, 'plots/config', paste0('Scenario_', i,'.jpg')), width = 220, 
       height = 120, units = 'mm', res = 500)
  par(mfrow = c(3,5))
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
  matplot(om_toPlot$ssb_phi_mat[,,1], type = 'l', ylab = 'SSB phi matrix (year 1)', axes = FALSE)
  axis(1, at = 1:length(these_lengths), labels = these_lengths)
  axis(2)
  box()
  par(mar = c(2,4,1,1))
  matplot(om_toPlot$LAA, type = 'l', ylab = 'Mean length at age (Jan 1st)', axes = FALSE)
  axis(1, at = 1:length(these_years), labels = these_years)
  axis(2)
  box()
  par(mar = c(2,4,1,1))
  matplot(om_toPlot$pred_waa[2,,], type = 'l', ylab = 'Mean weight at age (Jan 1st)', axes = FALSE)
  axis(1, at = 1:length(these_years), labels = these_years)
  axis(2)
  box()
  par(mar = c(2,4,1,1))
  plot(these_years, om_toPlot$Ecov_out[,4,1], type = 'b', xlab = NULL, ylab = 'Ecov_out')
  # Selectivity:
  par(mar = c(2,4,1,1))
  plot(these_lengths, om_toPlot$selLL[[1]][1,], type = 'b', xlab = NULL, ylab = 'Sel at length (Fish)', ylim = c(0,1))
  par(mar = c(2,4,1,1))
  plot(these_lengths, om_toPlot$selLL[[2]][1,], type = 'b', xlab = NULL, ylab = 'Sel at length (Sur)', ylim = c(0,1))   
  # Selectivity (age-based transformed, year = 1):
  selAA_1 = as.vector(t(as.matrix(om_toPlot$catch_phi_mat[,,1])) %*% as.matrix(om_toPlot$selLL[[1]][1,]))
  selAA_2 = as.vector(t(as.matrix(om_toPlot$jan1_phi_mat[,,1])) %*% as.matrix(om_toPlot$selLL[[2]][1,]))
  par(mar = c(2,4,1,1))
  plot(these_ages, selAA_1, type = 'b', xlab = NULL, ylab = 'Sel at age (Fish,transf,y=1)', ylim = c(0,1))
  par(mar = c(2,4,1,1))
  plot(these_ages, selAA_2, type = 'b', xlab = NULL, ylab = 'Sel at length (Sur,transf,y=1)', ylim = c(0,1))    
  dev.off()

}
