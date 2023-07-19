make_om <- function(Fmax = 0.8, Fmin = 0.05,
					years_base = NULL, ages_base = NULL, lengths_base = NULL,
                    selectivity = NULL, M = NULL, NAA_re = NULL,
                    catchability = NULL, growth = NULL, LW = NULL,
					WAA = NULL, LAA = NULL,
                    ecov = NULL, age_comp = "multinomial",
                    len_comp='multinomial',
                    F_change_time = 0.5, df.oms = NULL) {
  
  # Create basic WHAM input:
  basic_info = make_basic_info(base_years = years_base, ages = ages_base, fish_len = lengths_base)

  ny = length(basic_info$years)
  nlbins = basic_info$n_lengths
  # Turn on the simulation of all data:
  basic_info$use_catch_paa <- matrix(1, ncol = basic_info$n_fleets, nrow = ny)
  basic_info$use_index_paa <- matrix(1, ncol = basic_info$n_indices, nrow = ny)
  basic_info$use_catch_pal <- matrix(1, ncol = basic_info$n_fleets, nrow = ny)
  basic_info$use_index_pal <- matrix(1, ncol = basic_info$n_indices, nrow = ny)
  basic_info$use_catch_caal <- array(1, dim = c(ny, basic_info$n_fleets, nlbins))
  basic_info$use_index_caal <- array(1, dim = c(ny, basic_info$n_indices, nlbins))
  basic_info$use_catch_waa <- matrix(1, ncol = basic_info$n_fleets, nrow = ny)
  basic_info$use_index_waa <- matrix(1, ncol = basic_info$n_indices, nrow = ny)

    # F trajectory:
	year_change <- floor(ny * F_change_time)
	F_vals = numeric(ny)
	Slope = (Fmax - Fmin)/year_change
	F_vals[1] = Fmin
	F_vals[2:year_change] = Fmin + Slope*(2:year_change)
	F_vals[(year_change+1):ny] = Fmax + Slope*(year_change - (year_change+1):ny)
	basic_info$F = matrix(0, ncol = basic_info$n_fleets, nrow = ny)
	basic_info$F[,1] = F_vals # only one fishery

  input <- wham::prepare_wham_input(basic_info = basic_info, growth = growth,
									LW = LW, len_comp = len_comp,
									LAA = LAA, WAA = WAA,
									selectivity = selectivity, NAA_re = NAA_re, M = M, ecov = ecov,
									age_comp = age_comp, catchability = catchability)
  # Change Ecov information:
  input$par$Ecov_process_pars[1,] = 0 # mean Ecov value = 1
  input$par$Ecov_process_pars[2,] = df.oms$Ecov_re_sig # This is cond sd for the AR1 Ecov process
  input$par$Ecov_process_pars[3,] = df.oms$Ecov_re_cor # This is phi for the AR1 Ecov process
  input$par$Ecov_beta[4,1,1,] = df.oms$Ecov_effect # 4 = growth effect. change if number of surveys change

  return(input)
}
