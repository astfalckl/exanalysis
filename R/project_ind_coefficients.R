
#' Projects individual model SIC onto parameters of the spline basis
#'
#' @param sst_update
#' @param spline_params
#'
#' @return 
#' @export
project_sic_coefs <- function(
  sst_update,
  spline_params
){

  cat(sprintf("\r Setting up data"))

  model_names <- sst_update$simulation$sst$model %>% unique()

  model_bounds <- sst_update$simulation$sst %>%
    group_by(model) %>%
    summarise(
      min = min(sst),
      max = max(sst)
    ) %>%
    mutate(
      diff = max - min,
      scale = (max - spline_params$bounds[1])/diff
    )

  data_scaled <- sst_update$simulation$sst %>%
    left_join(model_bounds, by = "model") %>%
    rowwise() %>%
    mutate(sst = (sst - min) * scale + spline_params$bounds[1]) %>%
    ungroup() %>%
    dplyr::select(-min, -max, -diff, -scale) %>%
    filter(sst < spline_params$bounds[2])

  cat(sprintf("\r Projecting Individual Ice Coefficients"))

  ice_fits <- data_scaled %>%
    group_by(lat, lon, model) %>%
    summarise(beta = fit_spline(sst, ice, spline_params))

  ice_fits <- bind_cols(ice_fits %>% dplyr::select(-beta), ice_fits$beta) 

  prior <- spline_params$prior_exp

  all_ice_fits <- bind_cols(
    expand_grid(df = sst_update$simulation$coords, idx = 1:5, model = model_names)$df,
    expand_grid(df = sst_update$simulation$coords, idx = 1:5, model = model_names)[,c(2,3)]
  ) %>%
    left_join(ice_fits, by = c("lat", "lon", "model", "idx")) %>%
    rowwise() %>%
    mutate(
      beta = ifelse(beta > 1.5, 1.5, beta),
      beta = ifelse(is.na(beta), prior[idx], beta)
    )

  return(all_ice_fits)

}