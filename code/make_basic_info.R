# Create basic input. 
# IMPORTANT !!!
# This function is used by the OM and EM setup.

make_basic_info <- function(base_years = 1970:2021, ages = 1:10, Fhist = "updown", n_feedback_years = 0) { #changed years
        
    info <- list()
    info$ages <- ages
    info$years <- as.integer(base_years[1] - 1 + 1:(length(base_years) + n_feedback_years))
    info$n_fleets <- 1 
    info$n_indices <- 1
	ny <- length(info$years)
	na <- 10
	info$lengths <- seq(from = 2, to = 120, by = 2)
	nlbins <- length(info$lengths)
	info$n_lengths <- nlbins
    na <- length(info$ages)
    ny <- length(info$years)
    nby <- length(base_years)
    mid <- floor(nby/2)
    
    # These F trajectories should be tied to reference points
    #up then down
    if(Fhist == "updown") info$F <- matrix(0.2 + c(seq(0,0.4,length.out = mid),seq(0.4,0,length.out=nby-mid)),nby, info$n_fleets)
    #down then up
    if(Fhist == "downup") info$F <- matrix(0.2 + c(seq(0.4,0,length.out = mid),seq(0,0.4,length.out=nby-mid)),nby, info$n_fleets)
    if(n_feedback_years>0) info$F <- rbind(info$F, info$F[rep(nby, n_feedback_years),, drop = F]) #same F as terminal year for feedback period

	# Define obs error: 
    info$catch_cv <- matrix(0.05, ny, info$n_fleets)
    info$index_cv <- matrix(0.2, ny, info$n_indices)
    info$catch_Neff <- matrix(100, ny, info$n_fleets)
    info$index_Neff <- matrix(100, ny, info$n_indices)
    info$catch_NeffL <- matrix(100, ny, info$n_fleets)
    info$index_NeffL <- matrix(100, ny, info$n_indices)
    info$catch_caal_Neff <- array(5, dim = c(ny, info$n_fleets, nlbins))
    info$index_caal_Neff <- array(5, dim = c(ny, info$n_fleets, nlbins))	

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
    m50 <- 2.89  # age at 50% maturity
    mslope <- 0.88 # how quickly maturity increases with age
    mat <- 1/(1+exp((m50 - 1:na)/mslope))
    info$maturity <- t(matrix(mat, na, ny))

	# Define biological parameters to create 'waa' matrix:
	# Data simulated on Jan 1st
	# This is not important since sim_data will do it correctly
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
	info$waa_cv <- array(0.1, dim = c(nwaa, ny, na))

    #Don't bias correct anything (why?)
    info$bias_correct_process = FALSE
    info$bias_correct_observation = FALSE
    
    return(info)
	
}
