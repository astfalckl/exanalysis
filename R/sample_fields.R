
#' Generates a sic field sample from sampled SVD indexes
#'
#' @param sst_update an updated sst object from calculate_sst_update()
#' @param ice_updates an updated sic object from update_sic()
#' @param spline_params chosen spline parameters. This is a list with elements
#' bounds, knots, degree, and prior_exp
#' @param svd_sample sampled indexes of the sst SVDs
#'
#' @return a list comprising of a tibble with the sampled field and the SVD 
#' indexes
#' @export
sample_fields <- function(
  sst_update, 
  ice_updates, 
  spline_params, 
  svd_sample
){

  sst <- idx <- lat <- lon <- beta1 <- beta2 <- beta3 <- beta4 <- beta5 <- NULL
  time <- ice <- NULL

  sst_sample <- sst_update$E + 
    base::kronecker(
      sst_update$pre_mat %*% base::as.matrix(svd_sample), 
      base::matrix(1, nrow = sst_update$simulation$ntime)
    )

  sample <- sst_update$simulation$means %>%
    dplyr::select(-sst) %>%
    dplyr::mutate(sst_sample = base::as.numeric(sst_sample))

  E_adj_all <- base::lapply(1:5, function(i){

    sst_update$simulation$coords %>%
      dplyr::mutate(
        idx = i,
        E_adj = base::as.numeric(ice_updates[[i]]$E_adj)
      )

  }) %>% dplyr::bind_rows()

  ice_wide <- E_adj_all %>%
    dplyr::mutate(idx = paste0("beta", idx)) %>%
    tidyr::pivot_wider(c(lat, lon), names_from = "idx", values_from = "E_adj")

  sampled_fields <- dplyr::left_join(sample, ice_wide, by = c("lat", "lon")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ice = 
      exanalysis::calc_ice(
        spline_params, sst_sample, 
        c(beta1, beta2, beta3, beta4, beta5)
      )
    ) %>%
    dplyr::select(lat, lon, time, sst_sample, ice) %>%
    dplyr::mutate(
      ice = base::ifelse(ice > 1, 1, ice),
      ice = base::ifelse(ice < 0, 0, ice)
    ) %>%
    dplyr::ungroup()

   return(
    base::list(
      sampled_fields = sampled_fields,
      svd_idx = svd_sample
    )
  )

}