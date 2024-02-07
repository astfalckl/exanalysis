
#' Crate a sst_data object
#' 
#' This creates the object that holds the sst_data results, i.e., the Z
#'
#' @param data a tibble of sst data. Requires the columns lat, lon, sst_obs,
#' sd and type ('ann' or 'jfm').
#' @param coords the spatial coordinates of the data
#' @param B a matrix of expected bias (if included). Required to calculate
#' updates.
#' @param varB a matrix of bias variance (if included). Required to calculate
#' updates.
#'
#' @return Returns a sst_data object
#' @export
create_sst_data_object <- function(
  data, coords, B = NULL, varB = NULL
){
  
  n <- nrow(data)

  if(is.null(B)) {B <- matrix(0, nrow = n)}

  if(is.null(varB)) {varB <- Matrix::Matrix(matrix(0, nrow = n, ncol = n))}

  object <- list(data = data, coords = coords, B = B, varB = varB, n = n)

  new_sst_data(object)

}

#' Helper function to assign class to a sst_data object
#'
#' @param object sst_data object from sst_data()
#'
#' @return Returns a sst_data object
#' @export
new_sst_data <- function(object){
  structure(
    object,
    class = "sst_data"
  )
}

#' Print function for sst_data object
#'
#' @param x sst_data object
#' @param ... further arguments passed to or from other methods.
#'
#' @return Prints sst_data object
#' @export
print.sst_data <- function(x, ...){
  utils::str(x)
}