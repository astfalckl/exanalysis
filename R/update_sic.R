
#' Calculates BL update of the SIC parameters
#'
#' @param all_ice_fits
#' @param spline_params
#'
#' @return 
#' @export
update_sic <- function(
  all_ice_fits,
  spline_params
){

  ave_ice_fits <- all_ice_fits %>%
    group_by(lon, lat, idx) %>%
    summarise(beta = mean(beta), .groups = "drop")

  ice_updates <- vector(mode = "list", length = 5)
  names(ice_updates) <- paste0("beta", 1:5)

  for (i in 1:5){

    cat(sprintf("\rBeta %i", i))

    ave_ice_fits <- all_ice_fits %>%
      filter(idx == i) %>%
      group_by(lon, lat) %>%
      summarise(beta = mean(beta), .groups = "drop")

    coords <- ave_ice_fits %>%
      dplyr::select(lon, lat) %>%
      unique()

    D <- rdist.earth(as.matrix(coords), R = 1) 

    C <- D %>% 
      wendland(6, 3, 1, 1e-06) %>%
      Matrix()

    var_ice_fits <- all_ice_fits %>%
      group_by(lon, lat, idx) %>%
      summarise(var_beta = var(beta), .groups = "drop") %>%
      filter(idx == i)

    K <- Matrix(diag(sqrt(var_ice_fits$var_beta)))

    V_ice <- K %*% C %*% K

    Mu_ice <- ave_ice_fits %>% pull(beta) %>% Matrix()
    Mu_prior <- Matrix(rep(spline_params$prior_exp[i], nrow(coords)))

    model_names <- unique(all_ice_fits$model)
    m <- length(model_names)
    alpha <- 0.8
    n <- nrow(coords)

    inv_bit <- solve(
      alpha * V_ice + (1-alpha)/(2*m) * V_ice + 1/m * Diagonal(n, rep(0.001, n))
    )

    E_adj <- Mu_prior + alpha * V_ice %*% inv_bit %*% (Mu_ice - Mu_prior)
    V_adj <- alpha * V_ice - alpha^2 * V_ice %*% inv_bit %*% V_ice

    E_adj[which(as.numeric(E_adj) < 0)] <- 0

    ice_updates[[i]]$E_adj <- E_adj
    ice_updates[[i]]$V_adj <- V_adj

  }

  return(ice_updates)

}