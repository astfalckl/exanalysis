
#' Crate a sst_sim object
#' 
#' This creates the object that holds the sst_sim results, i.e., the X
#'
#' @param data a tibble containing the simulation data. Expected column names
#' are lat, lon, time, sst, ice and model. These are explained further
#' in the package README.
#' @param means the spatio-temporal SST means, i.e. averages over the MME for
#' each location in space and time.
#' @param coords the unique spatial lat/lon coords.
#'
#' @return Returns a sst_sim object
#' @export
create_sst_sim_object <- function(data, means, coords){
  
  n <- nrow(coords)
  ntime <- unique(data$time) %>% length()
  m <- unique(data$model) %>% length()

  object <- list(sst = data, means = means, coords = coords, n = n, 
    ntime = ntime, m = m)

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