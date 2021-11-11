#' Generates the SIC incidence matrix
#'
#' @param coords A sst_sim object containing the sst simulations
#' @param ns A sst_data object containing the proxy data
#'
#' @return Returns a matrix
#' @export
generate_Hy <- function(coords, ns){

  NS_id <- coords %>%
    dplyr::arrange(lon, lat) %>%
    dplyr::mutate(id = ifelse(lat < 0, "S", "N"))

  south_idx <- which(NS_id$id == "S")
  north_idx <- which(NS_id$id == "N")

  south_incidence <- rep(0, ns)
  north_incidence <- rep(0, ns)

  south_incidence[south_idx] <- 1
  north_incidence[north_idx] <- 1

  south_mat <- Matrix::Diagonal(ns, south_incidence)
  north_mat <- Matrix::Diagonal(ns, north_incidence)

  zero_mat <- Matrix::Matrix(matrix(rep(0, ns^2), nrow = ns))

  cbind(
    zero_mat, north_mat, zero_mat, zero_mat, zero_mat, zero_mat,
    zero_mat, south_mat, zero_mat, zero_mat, zero_mat, zero_mat
  )

}