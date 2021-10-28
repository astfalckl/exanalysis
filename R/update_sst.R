
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
calculate_sst_update <- function(simulation, proxy, H_list, params, incl_varM = FALSE){

	cat(sprintf("\rSetting up bits...         "))

	varS <- fields::rdist.earth(as.matrix(simulation$coords), R = 1) %>% 
	  wendland(params$tau, params$c, params$kappa, 1e-06) %>%
	  Matrix::Matrix()

	varT <- Matrix::Matrix(1, nrow = simulation$ntime, ncol = simulation$ntime)

	M <- Matrix::Matrix(simulation$means$sst)
	D <- Matrix::Diagonal(proxy$n, proxy$data$sd^2)

	M_scale <- params$alpha2/(simulation$m + params$alpha2)

	ns <- simulation$n	
	varMs <- matrix(0, nrow = ns, ncol = ns)

	if(incl_varM){varMs <- calculate_Ms(simulation)}

	mask <- as.matrix(varS)
	mask[which(as.matrix(varS) != 0)] <- 1

	varYS <- varS + M_scale/2 * Matrix(varMs) #Matrix(mask * varMs)

	one <- matrix(1/simulation$ntime, nrow = simulation$ntime, ncol = 1)

	cat(sprintf("\rCalculating H components..."))

	# HvarY <- H_list$Hs %*% rbind(varYS, varYS)
	HsvarYs <- H_list$Hs %*% rbind(varYS, varYS)
	HvarY <- kronecker(HsvarYs, t(one) %*% varT)

	HvarYH <- H_list$Hs %*% 
		kronecker(matrix(1, nrow = 2, ncol = 2), varYS) %*% 
		Matrix::t(H_list$Hs)

	solveU <- solve(HvarYH + proxy$varB + D)

	HM <- H_list$H %*% as.matrix(simulation$means$sst)

	error <- as.numeric(matrix(proxy$data$sst_obs) - HM - proxy$B)

	cat(sprintf("\rCalculating updates...     "))

	E <- M + Matrix::t(HvarY) %*% solveU %*% as.matrix(error)

	Vs <- varYS - Matrix::t(HsvarYs) %*% solveU %*% HsvarYs
	Vt <- varT

	update_tbl <- sst_update$simulation$means %>%
		rename(Eadj1 = sst) %>%
		mutate(
			Vadj1 = rep(diag(varYS), each = 12),
			Eadj2 = as.numeric(E),
			Eadj2 = ifelse(Eadj2 < -1.92, -1.92, Eadj2),
			Vadj2 = rep(diag(Vs), each = 12)
		)

	return(
		sst_reconstruction(
			list(
				simulation = simulation,
				proxy = proxy,
				E = E,
				Vs = Vs,
				Vt = Vt,
				update_tbl = update_tbl
			)
		)
	)

}