#' Monthly averages of SST and SIC from PMIP
#' 
#' A collection of SST and SIC Simulations from PMIP 3 and 4.
#' 
#' @format A sst_sim object of PMIP outputs
"model_data"

#' PMIP SST Simulations
#' 
#' A collection of SST Simulations from PMIP 3 and 4. This list is not 
#' exhaustive but is what was available to the project at the time of 
#' publication.
#' 
#' @format A sst_sim object of PMIP outputs
"pmip_sst"

#' SST Proxy Data
#' 
#' A collection of proxy based SST measurements from the MARGO project.
#' 
#' @format A sst_data object of PMIP outputs
"margo_sst"

#' Coastline shapefile
#' 
#' Shapefile of global coastlines for plotting.
#' 
#' @format a shapefile
"coastlines"

#' Grid-cell areas
#' 
#' Grid-cell areas for the PMIP ocean grid. Used in calculation of sea-ice areas
#' 
#' @format a tibble
"coords_area"

#' Winter sea-ice extents
#' 
#' Winter sea-ice extents for Antarctica from Gersonde (2005)
#' 
#' @format a tibble
#' @source Gersonde, R., Crosta, X., Abelmann, A., & Armand, L. (2005). 
#' Sea-surface temperature and sea ice distribution of the Southern Ocean at 
#' the EPILOG Last Glacial Maximum—a circum-Antarctic view based on siliceous 
#' microfossil records. Quaternary science reviews, 24(7-9), 869-896.
"wsi"