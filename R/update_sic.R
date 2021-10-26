
#' Calculates BL update of the SIC parameters
#'
#' @param all_ice_fits sic coefficient projections from project_sic_coefs()
#' @param spline_params chosen spline parameters. This is a list with elements
#' bounds, knots, degree, and prior_exp
#'
#' @return list with sic updates for each parameter
#' @export
update_sic <- function(
  all_ice_fits,
  spline_params
){

  lon <- lat <- idx <- NULL

  ave_ice_fits <- all_ice_fits %>%
    dplyr::group_by(lon, lat, idx) %>%
    dplyr::summarise(beta = mean(beta), .groups = "drop")

  ice_updates <- vector(mode = "list", length = 5)
  names(ice_updates) <- paste0("beta", 1:5)

  for (i in 1:5){

    cat(sprintf("\rBeta %i", i))

    ave_ice_fits <- all_ice_fits %>%
      dplyr::filter(idx == i) %>%
      dplyr::group_by(lon, lat) %>%
      dplyr::summarise(beta = mean(beta), .groups = "drop")

    coords <- ave_ice_fits %>%
      dplyr::select(lon, lat) %>%
      unique()

    D <- fields::rdist.earth(as.matrix(coords), R = 1) 

    C <- D %>% 
      wendland(6, 3, 1, 1e-06) %>%
      Matrix::Matrix()

    var_ice_fits <- all_ice_fits %>%
      dplyr::group_by(lon, lat, idx) %>%
      dplyr::summarise(var_beta = stats::var(beta), .groups = "drop") %>%
      dplyr::filter(idx == i)

    K <- Matrix::Matrix(diag(sqrt(var_ice_fits$var_beta)))

    V_ice <- K %*% C %*% K

    Mu_ice <- ave_ice_fits %>% dplyr::pull(beta) %>% Matrix::Matrix()
    Mu_prior <- Matrix::Matrix(rep(spline_params$prior_exp[i], nrow(coords)))

    model_names <- unique(all_ice_fits$model)
    m <- length(model_names)
    alpha <- 0.8
    n <- nrow(coords)

    inv_bit <- solve(
      alpha * V_ice + (1-alpha)/(2*m) * V_ice + 
        1/m * Matrix::Diagonal(n, rep(0.001, n))
    )

    E_adj <- Mu_prior + alpha * V_ice %*% inv_bit %*% (Mu_ice - Mu_prior)
    V_adj <- alpha * V_ice - alpha^2 * V_ice %*% inv_bit %*% V_ice

    E_adj[which(as.numeric(E_adj) < 0)] <- 0

    ice_updates[[i]]$E_adj <- E_adj
    ice_updates[[i]]$V_adj <- V_adj

  }

  return(ice_updates)

}