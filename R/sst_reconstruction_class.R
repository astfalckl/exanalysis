
#' Crate a sst_reconstruction object
#' 
#' This creates the object that holds the sst_reconstruction results, i.e., the X
#'
#' @param object List with elements XYZ
#'
#' @return Returns a sst_reconstruction object
#' @export
sst_reconstruction <- function(object){
	
	obj_names <- names(object)

  new_sst_reconstruction(object)

}

#' Helper function to assign class to a sst_reconstruction object
#'
#' @param object sst_reconstruction object from sst_reconstruction()
#'
#' @return Returns a sst_reconstruction object
#' @export
new_sst_reconstruction <- function(object){
  structure(
    object,
    class = "sst_reconstruction"
  )
}

#' Print function for sst_reconstruction object
#'
#' @param x sst_reconstruction object
#' @param ... further arguments passed to or from other methods.
#'
#' @return Prints sst_reconstruction object
#' @export
print.sst_reconstruction <- function(x, ...){
  utils::str(x)
}

#' @export
calculate_svds <- function(obj, ...) {
  UseMethod("calculate_svds")
}

#' Calculates the top n SVDs from an sst_reconstruction object
#'
#' @param obj an sst_reconstruction object
#' @param n the number of SVDs
#'
#' @return a list comprising of a tibble with the sampled field and the SVD 
#' indexes
#' @export
calculate_svds.sst_reconstruction <- function(obj, n = 20){

	sst <- lat <- lon <- NULL
	
	svds <- irlba::irlba(obj$Vs, nv = n, right_only = TRUE)

	svd_scaled <- svds$v %*% base::diag(base::sqrt(svds$d))
	base::colnames(svd_scaled) <- base::paste0(
		"SVD", 
		stringr::str_pad(1:n, 2,"left", "0")
	)

	svd_long <- obj$simulation$coords %>%
	  dplyr::bind_cols(dplyr::as_tibble(svd_scaled)) %>%
	  tidyr::gather(svd, sst, -lat, -lon)

	obj$pre_mat <- svd_scaled
	obj$svds <- svd_long
	obj$n_svds <- n

	return(obj)

}