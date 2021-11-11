
#' Projects sst/sic data at each location onto the spline basis coefficients
#'
#' @param sst_update an updated sst object from calculate_sst_update()
#' @param spline_params chosen spline parameters. This is a list with elements
#' bounds, knots, degree, and prior_exp
#'
#' @return a tibble of all of the spline basis coefficients at each location
#' @export
project_sic_coefs <- function(
  sst_update,
  spline_params
){

  base::cat(base::sprintf("\r Generating spatial bases          "))

  model_names <- sst_update$simulation$data$model %>% base::unique()

  ice_fits <- sst_update$simulation$data %>%
    dplyr::filter(sst < spline_params$bounds[2]) %>%
    dplyr::group_by(lat, lon, model) %>%
    dplyr::summarise(
      beta = fit_spline(sst, ice, spline_params),
      .groups = "keep"
    )

  ice_fits <- dplyr::bind_cols(ice_fits %>% dplyr::select(-beta), ice_fits$beta) 

  prior <- spline_params$prior_exp

  smooth <- fields::rdist.earth(as.matrix(sst_update$simulation$coords), R = 1) %>% 
    wendland(10, 4, 1, 1e-06) %>%
    Matrix::Matrix()

  W <- smooth/Matrix::rowSums(smooth)

  all_ice_fits <- dplyr::bind_cols(
    tidyr::expand_grid(
      df = sst_update$simulation$coords, idx = 1:5, model = model_names
    )$df,
    tidyr::expand_grid(
      df = sst_update$simulation$coords, idx = 1:5, model = model_names
    )[,c(2,3)]
  ) %>%
    dplyr::left_join(ice_fits, by = c("lat", "lon", "model", "idx")) %>%
    dplyr::group_by(idx, model) %>%
    dplyr::arrange(lon, lat) %>%
    dplyr::summarise(
      lon = lon, lat = lat, beta = as.numeric(W %*% beta), 
      .groups = "keep"
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(beta = ifelse(is.na(beta), spline_params$prior[idx], beta)) %>%
    dplyr::arrange(lon, lat) %>%
    dplyr::ungroup()

  return(all_ice_fits)

}
# project_sic_coefs <- function(
#   sst_update,
#   spline_params
# ){

#   model <- sst <- lat <- lon <- ice <- idx <- NULL

#   base::cat(base::sprintf("\r Setting up data"))

#   model_names <- sst_update$simulation$data$model %>% base::unique()

#   model_bounds <- sst_update$simulation$data %>%
#     dplyr::group_by(model) %>%
#     dplyr::summarise(
#       min = base::min(sst),
#       max = base::max(sst)
#     ) %>%
#     dplyr::mutate(
#       diff = max - min,
#       scale = (max - spline_params$bounds[1])/diff
#     )

#   data_scaled <- sst_update$simulation$data %>%
#     dplyr::left_join(model_bounds, by = "model") %>%
#     dplyr::rowwise() %>%
#     dplyr::mutate(sst = (sst - min) * scale + spline_params$bounds[1]) %>%
#     dplyr::ungroup() %>%
#     dplyr::select(-min, -max, -diff, -scale) %>%
#     dplyr::filter(sst < spline_params$bounds[2])

#   base::cat(base::sprintf("\r Projecting Individual Ice Coefficients"))

#   ice_fits <- data_scaled %>%
#     dplyr::group_by(lat, lon, model) %>%
#     dplyr::summarise(beta = fit_spline(sst, ice, spline_params))

#   ice_fits <- dplyr::bind_cols(ice_fits %>% dplyr::select(-beta), ice_fits$beta) 

#   prior <- spline_params$prior_exp

#   all_ice_fits <- dplyr::bind_cols(
#     tidyr::expand_grid(
#       df = sst_update$simulation$coords, idx = 1:5, model = model_names
#     )$df,
#     tidyr::expand_grid(
#       df = sst_update$simulation$coords, idx = 1:5, model = model_names
#     )[,c(2,3)]
#   ) %>%
#     dplyr::left_join(ice_fits, by = c("lat", "lon", "model", "idx")) %>%
#     dplyr::rowwise() %>%
#     dplyr::mutate(
#       beta = base::ifelse(beta > 1.5, 1.5, beta),
#       beta = base::ifelse(base::is.na(beta), prior[idx], beta)
#     )

#   return(all_ice_fits)

# }