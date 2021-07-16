
#' Crate a sst_sim object
#' 
#' This creates the object that holds the sst_sim results, i.e., the X
#'
#' @param object List with elements XYZ
#'
#' @return Returns a sst_sim object
#' @export
sst_sim <- function(object){
	
	obj_names <- names(object)

  new_sst_sim(object)

}

#' Helper function to assign class to a sst_sim object
#'
#' @param object sst_sim object from sst_sim()
#'
#' @return Returns a sst_sim object
#' @export
new_sst_sim <- function(object){
  structure(
    object,
    class = "sst_sim"
  )
}

#' Print function for sst_sim object
#'
#' @param x sst_sim object
#' @param ... further arguments passed to or from other methods.
#'
#' @return Prints sst_sim object
#' @export
print.sst_sim <- function(x, ...){
  utils::str(x)
}