
#' Calculate Psi Matrix
#'
#' @param model_tmp a tibble of MME member SST and SIC
#' @param spline_params prior spline specifications
#'w
#' @return returns Matrix object
#' @export
create_psii <- function(model_tmp, spline_params){
  ntime <- 12
  ns <- nrow(model_tmp %>% dplyr::filter(time == 1))
  nl <- length(spline_params$prior_exp)

  options(warn = -1)
  psi <- do.call(
    rbind,
    lapply(1:12, function(j){
      zero_idx <- which(
        model_tmp %>% dplyr::filter(time == j) %>% 
          dplyr::arrange(lon, lat) %>% 
          dplyr::pull(sst) > 0
      )

      phi_tmp <- calc_X(
        model_tmp %>% 
          dplyr::filter(time == j) %>% 
          dplyr::arrange(lon, lat) %>% 
          dplyr::pull(sst), 
        spline_params
      )

      phi_tmp[zero_idx, ] <- 0

      do.call(cbind,
        lapply(1:nl, function(i){
          diag(phi_tmp[,i])
        })
      ) %>% Matrix::Matrix()
    })
  )
  options(warn = 0)
  return(psi)
}

#' Calculate Theta Matrix
#'
#' @param all_fits 
#' @param idx_select 
#'
#' @return returns Matrix object
#' @export
create_theta <- function(all_fits, idx_select){
  proj_values <- all_fits %>%
    dplyr::filter(idx == idx_select) %>%
    tidyr::pivot_wider(names_from = model, values_from = beta) %>%
    dplyr::arrange(lon, lat) %>%
    dplyr::select(-lon, -lat, -idx) %>%
    as.matrix()

  proj_mean <- base::rowMeans(proj_values)

  m <- ncol(proj_values)

  list(
    mean = proj_mean,
    theta = base::svd(proj_values - proj_mean)$u[, 1:(m-1)],
    lambda = base::svd(proj_values - proj_mean)$d[1:(m-1)] 
  )
}
