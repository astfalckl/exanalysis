
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

#' Plots a chosen spline basis
#'
#' @param spline_object Output from calc_X
#'
#' @return A plot of the spline bases
#' @export
plot_spline_basis <- function(spline_object){
  x <- attr(spline_object, "x")
  
  spline_object %>% 
    dplyr::as_tibble() %>%
    dplyr::mutate(x = x) %>%
    dplyr::gather(spline, value, -x) %>%
    ggplot2::ggplot() + 
      ggplot2::geom_line(aes(x = x, y = value, colour = spline)) +
      ggplot2::theme_bw()
    
} 

#' Calculates the I-spline design matrix from a design
#'
#' @param x_in the inputs at which the basis is to be evaluated
#' @param boundary the boundary points
#' @param knots the knot locations
#' @param degree the spline degree
#'
#' @return a spline basis design matrix
#' @export
calc_X <- function(x_in, boundary, knots, degree){

  spline <- (- splines2::iSpline(
        x_in, 
        knot = knots, degree = degree, intercept = TRUE, 
        Boundary.knots = boundary
    )) + 1

  attr(spline, "x") <- x_in
  attr(spline, "boundary") <- boundary

  spline
}