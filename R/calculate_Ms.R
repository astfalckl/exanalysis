#' Calculates var[M(Xs)]
#'
#' @param model_data model_data object that ships with the package
#'
#' @return matrix
#' @export
calculate_Ms <- function(model_data){

  varMs <- base::matrix(0, nrow = model_data$n, ncol = model_data$n)
  count <- 0
  for (i in 1:12){
    for (j in 1:12){  

    count <- count + 1

    base::cat(
      base::sprintf(
        "\r Calculating var[M(Xs)] (%.0f%%)           ", 100*count/144
      )
    )

    covi <- model_data$data %>%
      dplyr::select(-ice) %>%
      tidyr::pivot_wider(values_from = sst, names_from = model) %>%
      dplyr::filter(time == i) %>%
      dplyr::arrange(lon, lat) %>%
      dplyr::select(-lat, -lon, -time) %>%
      base::as.matrix() %>% base::t()

    covj <- model_data$data %>%
      dplyr::select(-ice) %>%
      tidyr::pivot_wider(values_from = sst, names_from = model) %>%
      dplyr::filter(time == j) %>%
      dplyr::arrange(lon, lat) %>%
      dplyr::select(-lat, -lon, -time) %>%
      base::as.matrix() %>% base::t()

    cov_tmp <- stats::cov(covi, covj)

    varMs <- varMs + 1/144 * cov_tmp

    }
  }

  varMs

}