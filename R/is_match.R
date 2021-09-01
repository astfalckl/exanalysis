
#' Tests NROY membership
#' 
#' A history matching helper function that assesses if a plausibility sample is
#' part of the NROY space of sea-ice concentrations
#'
#' @param sampled_ice output of sic field from sample_fields()
#' @param wsi_area_south mean Southern winter sea ice area
#' @param wsi_area_north mean Northern winter sea ice area
#' @param coords_area grid-cell areas. This tibble should ship with the package
#'
#' @return Either TRUE or FALSE 
#' @export
is_nroy <- function(sampled_ice, wsi_area_south, wsi_area_north, coords_area){

  south_ice_area <- sampled$sampled_fields %>%
    dplyr::filter(time == 8) %>%
    dplyr::filter(lat <= 0) %>%
    dplyr::left_join(coords_area, by = c("lon", "lat")) %>%
    dplyr::mutate(area_ice = ice * area) %>%
    dplyr::pull(area_ice) %>%
    base::sum()

  north_ice_area <- sampled$sampled_fields %>%
    dplyr::filter(time == 2) %>%
    dplyr::filter(lat >= 0) %>%
    dplyr::left_join(coords_area, by = c("lon", "lat")) %>%
    dplyr::mutate(area_ice = ice * area) %>%
    dplyr::pull(area_ice) %>%
    base::sum()

  sample <- c(south_ice_area, north_ice_area)
  obs <- c(wsi_area_south, wsi_area_north)

  match <- base::all(
    c(
    sample[1] > obs[1] * 0.75 & sample[1] < obs[1] * 1.25,
    sample[2] > obs[2] * 0.95 & sample[2] < obs[2] * 1.25
    )
  )

  return(match)

}