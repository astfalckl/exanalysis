
#' Calculates BL update of the SIC parameters
#'
#' @param all_ice_fits sic coefficient projections from project_sic_coefs()
#' @param spline_params chosen spline parameters. This is a list with elements
#' bounds, knots, degree, and prior_exp
#'
#' @return list with sic updates for each parameter
#' @export
calculate_sic_update <- function(
  sst_update, spline_params, sic_prior_params, 
  betais, Xstar, sic_data,
  calculate_ind_updates = TRUE
){

  base::cat(base::sprintf("\r Setting up matrices            "))

  m <- sst_update$simulation$m
  model_names <- sst_update$simulation$data$model %>% unique()
  coords <- sst_update$simulation$coords
  ns <- sst_update$simulation$n
  nl <- length(spline_params$prior_exp)
  k <- nl * (m-1)

  var_beta <- diag(
    diag(
      cov(
        base::t(
          as.matrix(
            do.call(cbind, betais$betais)
          )
        )
      )
    )
  )

  var_Rbeta <- 0.5 * var_beta
  var_Mbeta <- 0.5 * var_beta

  R_list <- rep(list(var_Rbeta), m)
  R_list[[m+1]] <- matrix(0, ncol = k, nrow = k)

  varM <- do.call(
    rbind,
    do.call(cbind, rep(list(var_Mbeta), m+1)) %>%
      list() %>% rep(m+1)
  )
  varB <- varM + Matrix::bdiag(R_list)
  varR <- Matrix::bdiag(
    Matrix::bdiag(var_Rhats), 
    Matrix::bdiag(rep(list(var_Rbeta), m))
  )

  Bhat <- rbind(do.call(rbind, betais$betais), matrix(rep(0, k * m)))
  
  Xhat <- rbind(
    cbind(diag(rep(1, k * m)), matrix(0, nrow = k * m, ncol = k)),
    cbind(-diag(rep(1, k * m)), kronecker(matrix(rep(1, m)), diag(rep(1, k))))
  )

  Psi_star <- dplyr::bind_cols(
      sst_update$simulation$means, 
      dplyr::tibble(Xstar = Xstar)
    ) %>%
    dplyr::arrange(time, lon, lat) %>%
    dplyr::mutate(
      sst = ifelse(sst < spline_params$bounds[1], spline_params$bounds[1], sst)
    ) %>%
    create_psii(spline_params)    

  base::cat(base::sprintf("\r Calculating first update            "))

  # Updates
  V1inv <- Matrix::solve(Xhat %*% varB %*% Matrix::t(Xhat) + varR)
  
  adj_exp_B <- varB %*% Matrix::t(Xhat) %*% V1inv %*% Bhat
  adj_var_B <- varB - varB %*% Matrix::t(Xhat) %*% V1inv %*% Xhat %*% varB

  adj_exp_Mbeta <- tail(as.numeric(adj_exp_B), k)
  adj_var_Mbeta <- adj_var_B[(m*k+1):((m+1)*k), (m*k+1):((m+1)*k)]

  if(calculate_ind_updates){

    adj_exp_spline <- lapply(1:m, function(i){
      betais$Theta %*% adj_exp_B[((i-1)*60+1):(i*60)] + betais$beta_mean
    }) %>% purrr::reduce(cbind)

    adj_var_spline <- lapply(1:m, function(i){
      base::cat(base::sprintf("\r Calculating individual updates %i/%i         ", i, m))
      Matrix::diag(betais$Theta %*% adj_var_B[((i-1)*60+1):(i*60), ((i-1)*60+1):(i*60)] %*% Matrix::t(betais$Theta))
    }) %>% purrr::reduce(cbind)

    adj_exp_spline_tbl <- bind_cols(
      lapply(1:nl, function(i){
      coords %>% dplyr::arrange(lon, lat)
    }) %>% bind_rows(),
      dplyr::as_tibble(as.matrix(adj_exp_spline))
    )

    adj_var_spline_tbl <- bind_cols(
      lapply(1:nl, function(i){
      coords %>% dplyr::arrange(lon, lat)
    }) %>% bind_rows(),
      dplyr::as_tibble(as.matrix(adj_var_spline))
    )  

  }

  Mx <- Psi_star %*% (betais$Theta %*% adj_exp_Mbeta + betais$beta_mean)

  base::cat(base::sprintf("\r Calculating second update                 "))

  ptmp <- sic_prior_params$corW_params

  corW <- coords %>%
    dplyr::select(lon, lat) %>%
    dplyr::arrange(lon, lat) %>%
    as.matrix() %>%
    fields::rdist.earth(R = 1) %>% 
    wendland(ptmp[1], ptmp[2], ptmp[3], ptmp[4]) %>%
    Matrix::Matrix()

  Hy <- generate_Hy(coords, ns)

  ptmp <- sic_prior_params$varU_params

  varUi <- coords %>%
    dplyr::arrange(lon, lat) %>%
    as.matrix() %>%
    fields::rdist.earth(R = 1) %>% 
    wendland(ptmp[1], ptmp[2], ptmp[3], ptmp[4]) %>%
    Matrix::Matrix()

  varU <- Matrix::bdiag(varUi, varUi, varUi, varUi, varUi)

  varW <- Matrix::Matrix(diag(sic_data$sd)) %*% corW %*% Matrix::Matrix(diag(sic_data$sd))
  varDisc <- Psi_star %*% 
    (varU + betais$Theta %*% adj_var_Mbeta %*% Matrix::t(betais$Theta)) %*% 
    Matrix::t(Psi_star)
  V2_inv <- Matrix::solve(Hy %*% varDisc %*% Matrix::t(Hy) + varW + diag(rep(1e-8, nrow(varW))))

  base::cat(base::sprintf("\r Calculating second update (exp)            "))

  E_ice_update <- Mx + varDisc %*% Matrix::t(Hy) %*% V2_inv %*% 
    (Matrix::Matrix(sic_data$ice_meas) - (Hy %*% Mx))

  base::cat(base::sprintf("\r Calculating second update (var)            "))
# check math here
  tmp1 <- varU %*% Matrix::t(Psi_star) %*% Matrix::t(Hy)
  tmp2 <- tmp1 %*% V2_inv %*% Matrix::t(tmp1)
  tmp3 <- varU - tmp2
  V_ice_update <- Psi_star %*% tmp3 %*% Matrix::t(Psi_star)

  update_tbl <- sst_update$simulation$means %>%
    dplyr::arrange(time, lon, lat) %>%
    dplyr::mutate(
      Eadj1 = as.numeric(Mx),
      Vadj1 = Matrix::diag(varDisc),
      Eadj2 = as.numeric(E_ice_update),
      Vadj2 = Matrix::diag(V_ice_update),
      Eadj1 = ifelse(Eadj1 > 1, 1, Eadj1),
      Eadj1 = ifelse(Eadj1 < 0, 0, Eadj1),
      Eadj2 = ifelse(Eadj2 > 1, 1, Eadj2),
      Eadj2 = ifelse(Eadj2 < 0, 0, Eadj2)
    )

  if(calculate_ind_updates){
     
    list(
      data = sic_data,
      adj_exp_spline_tbl = adj_exp_spline_tbl,
      adj_var_spline_tbl = adj_var_spline_tbl,
      E = E_ice_update,
      V_param_update = tmp3,
      V = V_ice_update,
      update_tbl = update_tbl
    ) %>% return()

  } else {
    
    list(
      data = sic_data,
      E = E_ice_update,
      V_param_update = tmp3,
      V = V_ice_update,
      update_tbl = update_tbl
    )

  }

}