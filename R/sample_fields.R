
#' Calculates BL update of the SIC parameters
#'
#' @param
#'
#' @return 
#' @export
sample_fields <- function(
  sst_update, 
  ice_updates, 
  spline_params, 
  svd_sample
){

  sst_sample <- sst_update$E + 
    kronecker(
      sst_update$pre_mat %*% as.matrix(svd_sample), 
      matrix(1, nrow = sst_update$simulation$ntime)
    )

  sample <- sst_update$simulation$means %>%
    dplyr::select(-sst) %>%
    mutate(sst_sample = as.numeric(sst_sample))

  E_adj_all <- lapply(1:5, function(i){

    sst_update$simulation$coords %>%
      mutate(
        idx = i,
        E_adj = as.numeric(ice_updates[[i]]$E_adj)
      )

  }) %>% bind_rows()

  ice_wide <- E_adj_all %>%
    mutate(idx = paste0("beta", idx)) %>%
    pivot_wider(c(lat, lon), names_from = "idx", values_from = "E_adj")


  spline_params

  sampled_fields <- left_join(sample, ice_wide, by = c("lat", "lon")) %>%
    rowwise() %>%
    mutate(ice = 
      calc_ice(
        spline_params, sst_sample, 
        c(beta1, beta2, beta3, beta4, beta5)
      )
    ) %>%
    dplyr::select(lat, lon, time, sst_sample, ice) %>%
    mutate(
      ice = ifelse(ice > 1, 1, ice),
      ice = ifelse(ice < 0, 0, ice)
    ) %>%
    ungroup()

   return(
    list(
      sampled_fields = sampled_fields,
      svd_idx = svd_sample
    )
  )

}