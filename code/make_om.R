make_om <- function(Fmax = 0.8, Fmin = 0,
                    selectivity = NULL, M = NULL, NAA_re = NULL,
                    catchability = NULL, growth = NULL, LW = NULL,
					WAA = NULL, LAA = NULL,
                    ecov = NULL, age_comp = "multinomial",
                    len_comp='multinomial',
                    om_input = TRUE,
                    F_change_time = 0.5, df.oms = NULL) {

  basic_info <- make_basic_info()
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

  #overfishing_mult = 2.5 #multiplier for Fmsy for overfishing
  input <- wham::prepare_wham_input(basic_info = basic_info, growth = growth,
									LW = LW, len_comp = len_comp,
									LAA = LAA, WAA = WAA,
									selectivity = selectivity, NAA_re = NAA_re, M = M, ecov = ecov,
									age_comp = age_comp, catchability = catchability)
  # Change Ecov information:
  input$par$Ecov_process_pars[2,] = df.oms$Ecov_re_sig # This is cond sd for the AR1 Ecov process
  input$par$Ecov_process_pars[3,] = df.oms$Ecov_re_cor # This is phi for the AR1 Ecov process
  input$par$Ecov_beta[4,1,1,] = df.oms$Ecov_effect # 4 = growth effect. change if number of surveys change

  #input = set_q(input, catchability)
  ## what is this doing and why do I need it?
  ##  input = set_selectivity(input, selectivity)
  #input = set_M(input, M)
  #set F relative to Fmsy. This function is in get_FMSY.R
  input = set_F_scenario(input, Fmax = Fmax, Fmin = Fmin, change_time = F_change_time)
  if(om_input) return(input)
  else return(fit_wham(input, do.fit = FALSE, MakeADFun.silent = TRUE))
}
