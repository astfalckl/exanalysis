
#' Calculate the SST update
#' 
#' This requires the simulations, the data, the incidence matrices, and the 
#' trained parameters
#'
#' @param simulation a sst_sim object containing the sst simulations
#' @param proxy a sst_data object containing the proxy data
#' @param H_list the output from generate_H() containing a list of the 
#' incidence matrices
#' @param params fit parameters from train_params()
#' @param incl_varM include the varM matrix in the calculation
#'
#' @return Returns loss
#' @export
calculate_sst_update <- function(simulation, proxy, H_list, params, incl_varM = NULL){

	cat(sprintf("\rSetting up bits...         "))

	varS <- rdist.earth(as.matrix(simulation$coords), R = 1) %>% 
	  wendland(params$tau, params$alpha, params$kappa, 1e-06) %>%
	  Matrix()

	varT <- Matrix(1, nrow = simulation$ntime, ncol = simulation$ntime)

	M <- Matrix(simulation$means$sst)
	D <- Diagonal(proxy$n, proxy$data$sd^2)

	M_scale <- params$beta2/(simulation$m + params$beta2)

	varYS <- varS # + M_scale * varM

	one <- matrix(1/simulation$ntime, nrow = simulation$ntime, ncol = 1)

	cat(sprintf("\rCalculating H components..."))

	HvarY <- H_list$Hs %*% kronecker(rbind(varYS, varYS), t(one) %*% varT)

	HvarYH <- H_list$Hs %*% 
		kronecker(matrix(1, nrow = 2, ncol = 2), varYS) %*% 
		t(H_list$Hs)

	HsvarYs <- H_list$Hs %*% rbind(varYS, varYS)

	solveU <- solve(HvarYH + proxy$varB + D)

	error <- as.numeric(Z - HM - proxy$B)

	cat(sprintf("\rCalculating updates...     "))

	E <- M + t(HvarY) %*% solveU %*% as.matrix(error)

	Vs <- varYS - t(HsvarYs) %*% solveU %*% HsvarYs
	Vt <- varT

	return(
		sst_reconstruction(
			list(
				simulation = simulation,
				proxy = proxy,
				E = E,
				Vs = Vs,
				Vt = Vt
			)
		)
	)

}