
# ------------------------------------------------------------------------------
# ------------------------------ TODO ------------------------------------------
# ------------------------------------------------------------------------------

# Update parameter_loss
# What is this beta = TBD thing?
# Add in varM

# ------------------------------------------------------------------------------

#' Defines the loss function with which to train the sst parameters
#' 
#' This is currently negative log-likelihood, investigate using bias-variance
#'
#' @param params C4-Wendland parameters
#' @param error error vector
#' @param D data
#' @param varB bias variance
#' @param noise observation error
#' @param Hs spatial incidence matrix
#'
#' @return Returns loss
#' @export
parameter_loss <- function(params, error, D, varB, noise, Hs){

  tau <- 6
  alpha <- exp(params[1]) + 1/pi # constrains alpha > 1/pi
  kappa <- sqrt(exp(params[2])) # constrains kappa > 0

  cat(sprintf("\r Kappa = %f : Alpha = %f : Beta = TBD", kappa, alpha))

  sigma <- Matrix::Matrix(wendland(D, tau, alpha, kappa, 1e-06))
  sigma <- Hs %*% (sigma) %*% t(Hs) + varB + diag(noise)

  llike <- -1/2 * as.numeric(
    as.numeric(Matrix::determinant(sigma)$modulus) + 
    t(as.matrix(error)) %*% solve(sigma) %*% as.matrix(error)
  )
  
  return(-llike)
}

#' Crate a sst_data object
#' 
#' This creates the object that holds the sst_data results, i.e., the Z
#'
#' @param simulation a sst_sim object containing the sst simulations
#' @param proxy a sst_data object containing the proxy data
#' @param H_list the output from generate_H() containing a list of the 
#' incidence matrices
#'
#' @return Returns a sst_data object
#' @export
train_params <- function(simulation, proxy, H_list){

	Z <- as.numeric(proxy$data$sst_obs)
	HM <- H_list$H %*% as.matrix(simulation$means$sst)
	B <- proxy$B

  D <- fields::rdist.earth(as.matrix(simulation$coords), R = 1) 
	error <- as.numeric(Z - HM - B)

	Hs_collapse <- H_list$Hs_collapse

	params_optim <- stats::optim(
	  c(log(2), log(1)), parameter_loss, 
	  gr = NULL, error, D, proxy$varB, proxy$data$sd^2, Hs_collapse
	)

	tau <- 6
	alpha <- exp(params_optim$par[1]) + 1/pi
	kappa <- sqrt(exp(params_optim$par[2]))
	beta2 <- 1

	return(
		dplyr::tibble(
			tau = tau, alpha = alpha, kappa = kappa, beta2 = beta2
		)
	)

}