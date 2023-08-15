# Create basic input. 
# IMPORTANT !!!
# This function is used by the OM and EM setup.

make_basic_info <- function(base_years = 1970:2021, ages = 1:10, fish_len = 1:100,
                        n_fisheries = 1, n_indices = 1,
                        catch_sigma = NULL, agg_index_cv = NULL,
                        catch_Neff = NULL, index_Neff = NULL, catch_NeffL = NULL,
                        index_NeffL = NULL, catch_Neff_caal = NULL, 
                        index_Neff_caal = NULL, waa_cv = NULL) { 
        
    info <- list()
    info$ages <- ages
    info$years <- as.integer(base_years[1] - 1 + 1:length(base_years))
    info$n_fleets <- n_fisheries
    info$n_indices <- n_indices
	ny <- length(info$years)
	info$lengths <- fish_len
	nlbins <- length(info$lengths)
	info$n_lengths <- nlbins
    na <- length(info$ages)
    ny <- length(info$years)
    nby <- length(base_years)
    mid <- floor(nby/2)
    
	# Define obs error: 
    info$catch_cv <- catch_sigma
    info$index_cv <- agg_index_cv
    info$catch_Neff <- catch_Neff
    info$index_Neff <- index_Neff
    info$catch_NeffL <- catch_NeffL
    info$index_NeffL <- index_NeffL
    info$catch_caal_Neff <- array(50, dim = c(ny, info$n_fleets, nlbins)) #Not important, will be replaced later
    info$index_caal_Neff <- array(50, dim = c(ny, info$n_indices, nlbins)) #Not important, will be replaced later

	# More information:
    info$fracyr_indices <- matrix(0, ny, info$n_indices) # Jan 1st
    info$index_units <- rep(1, length(info$n_indices)) #biomass
    info$index_paa_units <- rep(2, length(info$n_indices)) #abundance
    info$fracyr_SSB <- rep(0, ny)
    info$q <- rep(1, info$n_indices) # what is this?
    info$selblock_pointer_fleets <- t(matrix(1:info$n_fleets, info$n_fleets, ny))
    info$selblock_pointer_indices <- t(matrix(info$n_fleets + 1:info$n_indices, info$n_indices, ny))

    # maturity of generic groundfish from IBMWG
    #mat <- c(0.04, 0.25, 0.60, 0.77, 0.85, 0.92, 1, 1, 1, 1)
    # params below get close to these values
    m50 <- 3  # age at 50% maturity
    mslope <- 0.5 # how quickly maturity increases with age
    mat <- 1/(1+exp((m50 - 1:na)/mslope))
    info$maturity <- t(matrix(mat, na, ny))

	# Define biological parameters to create 'waa' matrix:
	# Data simulated on Jan 1st
	# This is NOT important since sim_data$waa will replace this
    Linf <- 100
    k <- 0.2
    t0 <- 0
    a_LW <- exp(-12.1)
    b_LW <- 3.2
    L <- Linf*(1-exp(-k*(1:na - t0)))
    W <- a_LW*L^b_LW
    nwaa <- info$n_indices + info$n_fleets + 2
    info$waa <- array(NA, dim = c(nwaa, ny, na))
    for(i in 1:nwaa) info$waa[i,,] <- t(matrix(W, na, ny))
	
	# Define pointers correctly (only works for 1 fishery and 1 survey):
	# Leave it as it is here:
	# Dim 3 and 4 will NOT be used
	info$waa_pointer_fleets = 1
	info$waa_pointer_indices = 2
	info$waa_pointer_totcatch = 1
	info$waa_pointer_ssb = 2
	info$waa_pointer_jan1 = 2

	# Define obs error for waa:
	info$waa_cv <- waa_cv

    #Do bias correct anything (why?)
    info$bias_correct_process = TRUE
    info$bias_correct_observation = TRUE
    
    return(info)
	
}
