
#' Crate a sst_data object
#' 
#' This creates the object that holds the sst_data results, i.e., the Z
#'
#' @param object List with elements XYZ
#'
#' @return Returns a sst_data object
#' @export
sst_data <- function(object){
	
	obj_names <- names(object)

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
#' @param object sst_data object
#'
#' @return Prints sst_data object
#' @export
print.sst_data <- function(object){
  utils::str(object)
}