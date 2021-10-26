
#' Calculate Psi Matrix
#'
#' @param model_tmp a tibble of MME member SST and SIC
#' @param spline_params prior spline specifications
#'w
#' @return returns Matrix object
#' @export
create_psii <- function(model_tmp, spline_params){
  Y <- model_tmp$ice
  ntime <- 12
  ns <- nrow(model_tmp %>% filter(time == 1))
  nl <- length(spline_params$prior_exp)


  psi <- do.call(
    rbind,
    lapply(1:12, function(j){
      zero_idx <- which(
        model_tmp %>% filter(time == j) %>% arrange(lon, lat) %>% pull(sst) > 0
      )

      phi_tmp <- calc_X(
        model_tmp %>% filter(time == j) %>% arrange(lon, lat) %>% pull(sst), 
        spline_params
      )

      phi_tmp[zero_idx, ] <- 0

      do.call(cbind,
        lapply(1:nl, function(i){
          diag(phi_tmp[,i])
        })
      ) %>% Matrix()
    })
  )

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
    filter(idx == idx_select) %>%
    pivot_wider(names_from = model, values_from = beta) %>%
    arrange(lon, lat) %>%
    dplyr::select(-lon, -lat, -idx) %>%
    as.matrix()

  proj_mean <- rowMeans(proj_values)

  list(
    mean = proj_mean,
    theta = svd(proj_values - proj_mean)$u,
    lambda = svd(proj_values - proj_mean)$d 
  )
}
