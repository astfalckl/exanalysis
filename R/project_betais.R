
#' Find the beta projection
#'
#' @param sst_update an sst_reconstruction object generated from
#' calculate_sst_update(). This contains the results of the SST updates.
#' @param spline_params prior specifications of spline functions. A named-list
#' with names bounds, knots, degree and prior_exp (default coefficients).
#'
#' @return list containing betais, Theta and the mean of the betais.
#' @export
project_betais <- function(sst_update, spline_params){

  model_names <- sst_update$simulation$data$model %>% unique()

  svd_beta_projection <- project_sic_coefs(sst_update, spline_params)

  Theta_list <- lapply(1:5, function(i){
    create_theta(svd_beta_projection, i)
  })

  Theta <- Matrix::bdiag(
    Theta_list[[1]]$theta, Theta_list[[2]]$theta, Theta_list[[3]]$theta,
    Theta_list[[4]]$theta, Theta_list[[5]]$theta
  )

  betais <- lapply(1:sst_update$simulation$m, function(i){

    cat(sprintf("\rProjecting onto the beta_i (%.0f%%)          ", 100*(i-1)/
      sst_update$simulation$m))

    Yi_tibble <- sst_update$simulation$data %>%
      dplyr::filter(model == model_names[i]) %>%
      dplyr::arrange(time, lon, lat)

    Psii <- create_psii(Yi_tibble, spline_params)

    Y <- Matrix::Matrix(Yi_tibble$ice)

    beta_mean <- Matrix::Matrix(
      c(Theta_list[[1]]$mean, Theta_list[[2]]$mean, Theta_list[[3]]$mean, 
        Theta_list[[4]]$mean, Theta_list[[5]]$mean)
    )

    Y_mean <- Psii %*% beta_mean

    Phii <- Psii %*% Theta

    solve(as.matrix(Matrix::t(Phii) %*% Phii)) %*% Matrix::t(Phii) %*% (Y - Y_mean)

  })

  list(
  	betais = betais,
  	Theta = Theta,
  	beta_mean = Matrix::Matrix(
      c(Theta_list[[1]]$mean, Theta_list[[2]]$mean, Theta_list[[3]]$mean, 
        Theta_list[[4]]$mean, Theta_list[[5]]$mean)
    )
  )

}