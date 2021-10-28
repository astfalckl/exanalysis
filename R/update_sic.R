
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

  base::cat(base::sprintf("\r Variance Matrices            "))

  m <- sst_update$simulation$m
  coords <- sst_update$simulation$coords
  ns <- sst_update$simulation$n
  nl <- length(spline_params$prior_exp)

  # ###################################
  # VARIANCE MATRICES NEED TO BE FIGURED OUT
  var_beta_tbd <- diag(diag(cov(t(as.matrix(do.call(cbind, betais$betais))))))
  var_Rhat <- 0.05 * var_beta_tbd
  var_Rbeta <- 0.5 * var_beta_tbd
  var_Mbeta <- 0.5 * var_beta_tbd
  # ###################################

  R_list <- rep(list(var_Rbeta), m)
  R_list[[m+1]] <- matrix(0, ncol = nl*m, nrow = nl*m)

  varM <- do.call(
    rbind,
    do.call(cbind, rep(list(var_Mbeta), m+1)) %>%
      list() %>% rep(m+1)
  )
  varB <- varM + bdiag(R_list)
  varR <- bdiag(bdiag(rep(list(var_Rhat), m)), bdiag(rep(list(var_Rbeta), m)))

  Bhat <- rbind(do.call(rbind, betais$betais), matrix(rep(0, nl * m^2)))
  
  Xhat <- rbind(
    cbind(diag(rep(1, nl * m^2)), matrix(0, nrow = nl*m^2, ncol = nl*m)),
    cbind(-diag(rep(1, nl * m^2)), kronecker(matrix(rep(1, m)), diag(rep(1, nl*m))))
  )

  Psi_star <- sst_update$simulation$data %>%
    filter(model == model_names[1]) %>%
    arrange(time, lon, lat) %>%
    dplyr::select(-sst, -model) %>%
    left_join(
      bind_cols(
        sst_update$simulation$means, 
        tibble(Xstar = Xstar)
      )
    ) %>%
    mutate(sst = ifelse(sst < -1.92, -1.92, sst)) %>%
    create_psii(spline_params)

  base::cat(base::sprintf("\r First Update            "))

  # Updates
  V1inv <- solve(Xhat %*% varB %*% t(Xhat) + varR)
  
  adj_exp_B <- varB %*% t(Xhat) %*% V1inv %*% Bhat
  adj_var_B <- varB - varB %*% t(Xhat) %*% V1inv %*% Xhat %*% varB

  adj_exp_Mbeta <- tail(as.numeric(adjB), nl*m)
  adj_var_Mbeta <- adj_var_B[(nl*m*m+1):(nl*m*(m+1)), (nl*m*m+1):(nl*m*(m+1))]

  Mx <- Psi_star %*% (Theta %*% adj_exp_Mbeta + betais$beta_mean)

  base::cat(base::sprintf("\r Second Update            "))

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

  # sic_data <- sic_data %>%
  #   mutate(sd = ifelse(sd == 0.5, 1, sd))

  varW <- Matrix(diag(sic_data$sd)) %*% corW %*% Matrix(diag(sic_data$sd))
  varDisc <- Psi_reality %*% varU %*% t(Psi_reality)
  V2_inv <- solve(Hy %*% varDisc %*% t(Hy) + varW + diag(rep(1e-8, nrow(varW))))

  base::cat(base::sprintf("\r Second Update E            "))

  E_ice_update <- Mx + varDisc %*% t(Hy) %*% V2_inv %*% 
    (Matrix(sic_data$ice_meas) - (Hy %*% Mx))

  tmp1 <- varU %*% t(Psi_reality) %*% t(Hy)
  tmp2 <- tmp1 %*% V2_inv %*% t(tmp1)
  tmp3 <- varU - tmp2

  base::cat(base::sprintf("\r Second Update V            "))

  V_ice_update <- Psi_reality %*% tmp3 %*% t(Psi_reality)
  # V_ice_update <- varDisc - varDisc %*% t(Hy) %*% V2_inv %*% Hy %*% varDisc

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