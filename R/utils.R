
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
  
  spline <- value <- NULL

  x <- attr(spline_object, "x")
  
  spline_object %>% 
    dplyr::as_tibble() %>%
    dplyr::mutate(x = x) %>%
    tidyr::gather(spline, value, -x) %>%
    ggplot2::ggplot() + 
      ggplot2::geom_line(ggplot2::aes(x = x, y = value, colour = spline)) +
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
calc_X <- function(x_in, spline_params){

  bounds <- spline_params$bounds
  knots <- spline_params$knots
  degree <- spline_params$degree

  spline <- (- splines2::iSpline(
        x_in, 
        knot = knots, degree = degree, intercept = TRUE, 
        Boundary.knots = bounds
    )) + 1

  attr(spline, "x") <- x_in
  attr(spline, "boundary") <- bounds

  spline
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
fit_spline <- function(sst, ice, spline_params){
  
  X <- calc_X(sst, spline_params)

  beta <- exp(
    stats::optim(
      log(spline_params$prior_exp+0.001), 
      loss, 
      gr = NULL, 
      X, ice, spline_params$prior_exp
    )$par
  )
  
  return(dplyr::tibble(beta = beta, idx = 1:5))

}

loss <- function(log_beta, X, ice, priors){
  beta <- exp(log_beta)
  sum((X %*% beta - ice)^2) + 0.05 * sum((beta - priors)^2)
}

calc_ice <- function(spline_params, sst_value, betas){

  if (sst_value > spline_params$bounds[2]){

    return(0)

  } else if (sst_value < spline_params$bounds[1]){

    return(1)

  } else{

    X <- calc_X(sst_value, spline_params)
    betas <- as.matrix(betas)

    as.numeric(X %*% betas)

  }

}

# priors <- c(0.05, 0.7, 0.15, 0.01, 0.01)

# bench::mark(optim(log(priors), loss, gr = NULL, X, ice, priors)$par)
# bench::mark(nnls(X, tmp$ice)$x)








