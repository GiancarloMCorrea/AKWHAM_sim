make_plot_om = function(om_toPlot, i) {

  # Biology
  jpeg(filename = file.path('plots', paste0('OM_biology_', i,'.jpg')), width = 190, height = 160, units = 'mm', res = 500)
  par(mfrow = c(3,3))
  par(mar = c(2,4,1,1))
  plot(years_base, om_toPlot$rep$SSB, type = 'b', xlab = NULL, ylab = 'SSB')
  par(mar = c(2,4,1,1))
  plot(years_base, om_toPlot$rep$pred_NAA[,1], type = 'b', xlab = NULL, ylab = 'Recruitment')
  par(mar = c(2,4,1,1))
  plot(years_base, om_toPlot$rep$pred_catch[,1], type = 'b', xlab = NULL, ylab = 'Catch')
  par(mar = c(2,4,1,1))
  plot(years_base, om_toPlot$rep$F[,1], type = 'b', xlab = NULL, ylab = 'F')
  par(mar = c(2,4,1,1))
  plot(ages_base, om_toPlot$input$data$mature[1,], type = 'b', xlab = NULL, ylab = 'Maturity')
  par(mar = c(2,4,1,1))
  matplot(om_toPlot$rep$ssb_phi_mat[1,,], type = 'l', ylab = 'SSB phi matrix (year 1)')
  par(mar = c(2,4,1,1))
  matplot(om_toPlot$rep$LAA, type = 'l', ylab = 'Mean length at age (Jan 1st)')
  par(mar = c(2,4,1,1))
  matplot(om_toPlot$rep$pred_waa[2,,], type = 'l', ylab = 'Mean weight at age (Jan 1st)')
  par(mar = c(2,4,1,1))
  plot(years_base, om_toPlot$rep$Ecov_out[,4,1], type = 'b', xlab = NULL, ylab = 'Ecov_out')
  dev.off()
  
  # Selectivity:
  jpeg(filename = file.path('plots', paste0('OM_selectivity_', i,'.jpg')), width = 190, height = 160, units = 'mm', res = 500)
  par(mfrow = c(2,2))
  par(mar = c(2,4,1,1))
  plot(ages_base, om_toPlot$rep$selAA[[1]][1,], type = 'b', xlab = NULL, ylab = 'Selectivity at age (Fishery)', ylim = c(0,1))
  par(mar = c(2,4,1,1))
  plot(ages_base, om_toPlot$rep$selAA[[2]][1,], type = 'b', xlab = NULL, ylab = 'Selectivity at age (Survey)', ylim = c(0,1))    
  par(mar = c(2,4,1,1))
  plot(lengths_base, om_toPlot$rep$selLL[[1]][1,], type = 'b', xlab = NULL, ylab = 'Selectivity at length (Fishery)', ylim = c(0,1))
  par(mar = c(2,4,1,1))
  plot(lengths_base, om_toPlot$rep$selLL[[2]][1,], type = 'b', xlab = NULL, ylab = 'Selectivity at length (Survey)', ylim = c(0,1))     
  dev.off()

}