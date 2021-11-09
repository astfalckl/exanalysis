
#' Find the beta projection
#'
#' @param sst_update a sst_sim object containing the sst simulations
#' @param spline_params prior spline specifications
#'
#' @return list of betais
#' @export
project_betais <- function(sst_update, spline_params){

  model_names <- sst_update$simulation$data$model %>% unique()

  svd_beta_projection <- project_sic_coefs(sst_update, spline_params)

  Theta_list <- lapply(1:5, function(i){
    create_theta(svd_beta_projection, i)
  })

  Theta <- bdiag(Theta_list[[1]]$theta, Theta_list[[2]]$theta, Theta_list[[3]]$theta,
    Theta_list[[4]]$theta, Theta_list[[5]]$theta)

  betais <- lapply(1:sst_update$simulation$m, function(i){

    cat(sprintf("\rProjecting onto the beta_i (%.0f%%)          ", 100*(i-1)/
      sst_update$simulation$m))

    Yi_tibble <- sst_update$simulation$data %>%
      filter(model == model_names[i]) %>%
      arrange(time, lon, lat)

    Psii <- create_psii(Yi_tibble, spline_params)

    Y <- Matrix(Yi_tibble$ice)

    beta_mean <- Matrix(
      c(Theta_list[[1]]$mean, Theta_list[[2]]$mean, Theta_list[[3]]$mean, 
        Theta_list[[4]]$mean, Theta_list[[5]]$mean)
    )

    Y_mean <- Psii %*% beta_mean

    Phii <- Psii %*% Theta

    betai <- solve(as.matrix(Matrix::t(Phii) %*% Phii)) %*% Matrix::t(Phii) %*% (Y - Y_mean)

    betai

  })

  list(
  	betais = betais,
  	Theta = Theta,
  	beta_mean = Matrix(
      c(Theta_list[[1]]$mean, Theta_list[[2]]$mean, Theta_list[[3]]$mean, 
        Theta_list[[4]]$mean, Theta_list[[5]]$mean)
    )
  )

}