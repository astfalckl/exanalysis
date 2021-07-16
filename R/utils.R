
#' C4-Wendland covariance function
#' 
#' Calculates the C4-Wendland covariance matrix given an input distance matrix
#'
#' @param D Distance matrix
#' @param tau Length-scale
#' @param alpha Roughness
#' @param kappa Amplitude parameter
#' @param delta Nugget term
#'
#' @return Returns a covariance matrix
#' @export
wendland <- function(D, tau, alpha, kappa, delta = NULL){
  tmp <- kappa^2 * (1 + tau * alpha * D + (tau^2 - 1)/3 * (alpha * D)^2) * 
    pmax(1 - (alpha * D), 0)^tau

  if(!is.null(delta)) tmp <- tmp + diag(delta, nrow(D))

  Matrix::Matrix(tmp)
}