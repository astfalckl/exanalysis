#' Calculates var[M(Xs)]
#'
#' @param model_data model_data object that ships with the package
#'
#' @return matrix
#' @export
calculate_Ms <- function(model_data){

  varMs <- matrix(0, nrow = ns, ncol = ns)
  for (i in 1:12){
    for (j in 1:12){  

    cat(sprintf("\rIteration i: %i, j: %i     ", i, j))

    covi <- model_data$data %>%
      dplyr::select(-ice) %>%
      pivot_wider(values_from = sst, names_from = model) %>%
      filter(time == i) %>%
      arrange(lon, lat) %>%
      dplyr::select(-lat, -lon, -time) %>%
      as.matrix() %>% t()

    covj <- model_data$data %>%
      dplyr::select(-ice) %>%
      pivot_wider(values_from = sst, names_from = model) %>%
      filter(time == j) %>%
      arrange(lon, lat) %>%
      dplyr::select(-lat, -lon, -time) %>%
      as.matrix() %>% t()

    cov_tmp <- cov(covi, covj)

    varMs <- varMs + 1/144 * cov_tmp

    }
  }

  varMs

}