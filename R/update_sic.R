
#' Calculates BL update of the SIC parameters
#'
#' @param all_ice_fits sic coefficient projections from project_sic_coefs()
#' @param spline_params chosen spline parameters. This is a list with elements
#' bounds, knots, degree, and prior_exp
#'
#' @return list with sic updates for each parameter
#' @export
calculate_sic_update <- function(
  sst_update, spline_params, sic_prior_params, betais, Xstar, sic_data
){

  base::cat(base::sprintf("\r Setting up matrices            "))

  m <- sst_update$simulation$m
  coords <- sst_update$simulation$coords
  ns <- sst_update$simulation$n
  nl <- length(spline_params$prior_exp)
  k <- nl * (m-1)

  # ###################################
  # VARIANCE MATRICES NEED TO BE FIGURED OUT
  var_beta_tbd <- diag(diag(cov(t(as.matrix(do.call(cbind, betais$betais))))))
  var_Rbeta <- 0.5 * var_beta_tbd
  var_Mbeta <- 0.5 * var_beta_tbd

  
  var_Rhat <- 0.05 * var_beta_tbd
  # ###################################

  R_list <- rep(list(var_Rbeta), m)
  R_list[[m+1]] <- matrix(0, ncol = k, nrow = k)

  varM <- do.call(
    rbind,
    do.call(cbind, rep(list(var_Mbeta), m+1)) %>%
      list() %>% rep(m+1)
  )
  varB <- varM + bdiag(R_list)
  varR <- bdiag(bdiag(rep(list(var_Rhat), m)), bdiag(rep(list(var_Rbeta), m)))

  Bhat <- rbind(do.call(rbind, betais$betais), matrix(rep(0, k * m)))
  
  Xhat <- rbind(
    cbind(diag(rep(1, k * m)), matrix(0, nrow = k * m, ncol = k)),
    cbind(-diag(rep(1, k * m)), kronecker(matrix(rep(1, m)), diag(rep(1, k))))
  )

  Psi_star <- bind_cols(
      sst_update$simulation$means %>% dplyr::select(-sst), 
      tibble(sst = Xstar)
    ) %>%
    arrange(time, lon, lat) %>%
    mutate(sst = ifelse(sst < spline_params$bounds[1], spline_params$bounds[1], sst)) %>%
    create_psii(spline_params)    

  base::cat(base::sprintf("\r Calculating first update            "))

  # Updates
  V1inv <- solve(Xhat %*% varB %*% t(Xhat) + varR)
  
  adj_exp_B <- varB %*% t(Xhat) %*% V1inv %*% Bhat
  adj_var_B <- varB - varB %*% t(Xhat) %*% V1inv %*% Xhat %*% varB

  adj_exp_Mbeta <- tail(as.numeric(adj_exp_B), k)
  adj_var_Mbeta <- adj_var_B[(m*k+1):((m+1)*k), (m*k+1):((m+1)*k)]

  Mx <- Psi_star %*% (Theta %*% adj_exp_Mbeta + betais$beta_mean)

  base::cat(base::sprintf("\r Calculating second update                 "))

  ptmp <- sic_prior_params$corW_params

  corW <- coords %>%
    dplyr::select(lon, lat) %>%
    arrange(lon, lat) %>%
    as.matrix() %>%
    fields::rdist.earth(R = 1) %>% 
    wendland(ptmp[1], ptmp[2], ptmp[3], ptmp[4]) %>%
    Matrix::Matrix()

  Hy <- generate_Hy(coords, ns)

  ptmp <- sic_prior_params$varU_params

  varUi <- coords %>%
    arrange(lon, lat) %>%
    as.matrix() %>%
    fields::rdist.earth(R = 1) %>% 
    wendland(ptmp[1], ptmp[2], ptmp[3], ptmp[4]) %>%
    Matrix::Matrix()

  varU <- bdiag(varUi, varUi, varUi, varUi, varUi)

  varW <- Matrix(diag(sic_data$sd)) %*% corW %*% Matrix(diag(sic_data$sd))
  varDisc <- Psi_star %*% 
    (varU + Theta %*% adj_var_Mbeta %*% t(Theta)) %*% 
    t(Psi_star)
  V2_inv <- solve(Hy %*% varDisc %*% t(Hy) + varW + diag(rep(1e-8, nrow(varW))))

  base::cat(base::sprintf("\r Calculating second update (exp)            "))

  E_ice_update <- Mx + varDisc %*% t(Hy) %*% V2_inv %*% 
    (Matrix(sic_data$ice_meas) - (Hy %*% Mx))

  base::cat(base::sprintf("\r Calculating second update (var)            "))
# check math here
  tmp1 <- varU %*% t(Psi_star) %*% t(Hy)
  tmp2 <- tmp1 %*% V2_inv %*% t(tmp1)
  tmp3 <- varU - tmp2
  V_ice_update <- Psi_star %*% tmp3 %*% t(Psi_star)

  update_tbl <- sst_update$simulation$means %>%
    arrange(time, lon, lat) %>%
    mutate(
      E = as.numeric(E_ice_update),
      E = ifelse(E > 1, 1, E),
      E = ifelse(E < 0, 0, E),
      V = diag(V_ice_update)
    )

  list(
    data = sic_data,
    E = E_ice_update,
    V = V_ice_update,
    update_tbl = update_tbl
  )

}