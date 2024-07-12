make_om <- function(Fmax = 0.8, Fmin = 0.05,
                    n_years_base = NULL, n_years_burnin = NULL,
                    ages_base = NULL, lengths_base = NULL,
                    selectivity = NULL, M = NULL, NAA_re = NULL, sigma_R = NULL,
                    catchability = NULL, maturity = NULL,
					          WAA = NULL, LAA = NULL,
                    n_fisheries = NULL, n_indices = NULL,
                    catch_sigma = NULL, agg_index_cv = NULL,
                    catch_Neff = NULL, index_Neff = NULL, catch_NeffL = NULL,
                    index_NeffL = NULL, catch_Neff_caal = NULL, 
                    index_Neff_caal = NULL, waa_cv = NULL,
                    ecov = NULL, age_comp = "multinomial",
                    len_comp='multinomial',
                    F_change_time = 0.5, 
                    Ecov_re_sig = NULL, Ecov_re_cor = NULL, Ecov_effect = NULL) {
  
  # Create basic WHAM input:
  basic_info = make_basic_info(n_years_base = n_years_base, n_years_burnin = n_years_burnin, type = 'om',
                               ages = ages_base, fish_len = lengths_base,
                            n_fisheries = n_fisheries, n_indices = n_indices,
                            catch_sigma = catch_sigma, agg_index_cv = agg_index_cv,
                            catch_Neff = catch_Neff, index_Neff = index_Neff, catch_NeffL = catch_NeffL,
                            index_NeffL = index_NeffL, catch_Neff_caal = catch_Neff_caal, 
                            index_Neff_caal = index_Neff_caal, waa_cv = waa_cv)

  ny = length(basic_info$years)
  nlbins = basic_info$n_lengths
  # Select data to simulate by OM:
  # if(df.scenario$catch_data == 'paa') {
  #   basic_info$use_catch_paa <- matrix(1, ncol = basic_info$n_fleets, nrow = ny)
  # }
  # if(df.scenario$catch_data == 'pal') {
  #   basic_info$use_catch_paa <- matrix(0, ncol = basic_info$n_fleets, nrow = ny)
  #   basic_info$use_catch_pal <- matrix(1, ncol = basic_info$n_fleets, nrow = ny)
  # }
  # if(df.scenario$catch_data == 'caal') {
  #   basic_info$use_catch_pal <- matrix(1, ncol = basic_info$n_fleets, nrow = ny)
  #   basic_info$use_catch_caal <- array(1, dim = c(ny, basic_info$n_fleets, nlbins))
  #   basic_info$use_catch_paa <- matrix(1, ncol = basic_info$n_fleets, nrow = ny)
  # }
  basic_info$use_catch_pal <- matrix(1, ncol = basic_info$n_fleets, nrow = ny)
  basic_info$use_catch_caal <- array(1, dim = c(ny, basic_info$n_fleets, nlbins))
  basic_info$use_catch_paa <- matrix(1, ncol = basic_info$n_fleets, nrow = ny)  
  
  # if(df.scenario$index_data == 'paa') {
  #   basic_info$use_index_paa <- matrix(1, ncol = basic_info$n_indices, nrow = ny)
  # }
  # if(df.scenario$index_data == 'pal') {
  #   basic_info$use_index_paa <- matrix(0, ncol = basic_info$n_indices, nrow = ny)
  #   basic_info$use_index_pal <- matrix(1, ncol = basic_info$n_indices, nrow = ny)
  # }
  # if(df.scenario$index_data == 'caal') {
  #   basic_info$use_index_pal <- matrix(1, ncol = basic_info$n_indices, nrow = ny)
  #   basic_info$use_index_caal <- array(1, dim = c(ny, basic_info$n_indices, nlbins))
  #   basic_info$use_index_paa <- matrix(0, ncol = basic_info$n_indices, nrow = ny)
  # }
  basic_info$use_index_pal <- matrix(1, ncol = basic_info$n_indices, nrow = ny)
  basic_info$use_index_caal <- array(1, dim = c(ny, basic_info$n_indices, nlbins))
  basic_info$use_index_paa <- matrix(1, ncol = basic_info$n_indices, nrow = ny)
  
  # if(df.scenario$method == 'WAA' | df.scenario$method == 'EWAA') { # only simulate waa index data when method = WAA or EWAA
  basic_info$use_index_waa <- matrix(1, ncol = basic_info$n_indices, nrow = ny)
  basic_info$use_catch_waa <- matrix(1, ncol = basic_info$n_fleets, nrow = ny)
  # }

  # F trajectory:
	year_change <- floor(n_years_base * F_change_time)
	F_vals = numeric(n_years_base)
	Slope = (Fmax - Fmin)/year_change
	F_vals[1] = Fmin
	F_vals[2:year_change] = Fmin + Slope*(2:year_change)
	F_vals[(year_change+1):n_years_base] = Fmax + Slope*(year_change - (year_change+1):n_years_base)
	F_vals = c(rep(0.005, times = n_years_burnin), F_vals) # F = 0.005 for burnin period. DO NOT PUT ZERO since log(0) = ERROR
	basic_info$F = matrix(0, ncol = basic_info$n_fleets, nrow = ny)
	basic_info$F[,1] = F_vals # only one fishery, make this more flexible in the future

  input <- wham::prepare_wham_input(basic_info = basic_info, LAA = LAA,
									WAA = WAA, maturity = maturity, len_comp = len_comp,
									selectivity = selectivity, NAA_re = NAA_re, M = M, ecov = ecov,
									age_comp = age_comp, catchability = catchability)
  # Change Ecov information:
  input$par$Ecov_process_pars[1,] = 0 # mean Ecov value 
  input$par$Ecov_process_pars[2,] = Ecov_re_sig # This is cond sd for the AR1 Ecov process, will be exp() internally
  input$par$Ecov_process_pars[3,] = Ecov_re_cor # This is phi for the AR1 Ecov process: 
  for(i in 1:3) {
    input$par$Ecov_beta[i+3,1,1,] = Ecov_effect[i] # 4:6 = LAA effect vonB. change if number of surveys change
  }
  input$par$log_NAA_sigma = log(sigma_R) # sigmaR recruitment
  input$map$log_NAA_sigma = factor(NA) # fix sigma Important to do this for plotting
  input$map$log_N1_pars <- factor(c(1, NA)) # Important to do this for plotting

  return(input)
}
