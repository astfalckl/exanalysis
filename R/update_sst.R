
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

	varS <- fields::rdist.earth(as.matrix(simulation$coords), R = 1) %>% 
	  wendland(params$tau, params$alpha, params$kappa, 1e-06) %>%
	  Matrix::Matrix()

	varT <- Matrix::Matrix(1, nrow = simulation$ntime, ncol = simulation$ntime)

	M <- Matrix::Matrix(simulation$means$sst)
	D <- Matrix::Diagonal(proxy$n, proxy$data$sd^2)

	M_scale <- params$beta/(simulation$m + params$beta)

	varYS <- varS # + M_scale * varM

	one <- matrix(1/simulation$ntime, nrow = simulation$ntime, ncol = 1)

	cat(sprintf("\rCalculating H components..."))

	HvarY <- H_list$Hs %*% kronecker(rbind(varYS, varYS), t(one) %*% varT)

	HvarYH <- H_list$Hs %*% 
		kronecker(matrix(1, nrow = 2, ncol = 2), varYS) %*% 
		Matrix::t(H_list$Hs)

	HsvarYs <- H_list$Hs %*% rbind(varYS, varYS)

	solveU <- solve(HvarYH + proxy$varB + D)

	HM <- H_list$H %*% as.matrix(simulation$means$sst)

	error <- as.numeric(matrix(proxy$data$sst_obs) - HM - proxy$B)

	cat(sprintf("\rCalculating updates...     "))

	E <- M + Matrix::t(HvarY) %*% solveU %*% as.matrix(error)

	Vs <- varYS - Matrix::t(HsvarYs) %*% solveU %*% HsvarYs
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