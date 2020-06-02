#' Maximum extent of reservoirs in Ceará
#'
#' @docType data
#' @usage data(res_max)
#'
#' @references  Jean-Francois Pekel, Andrew Cottam, Noel Gorelick, Alan S. Belward, High-resolution mapping of global surface water and its long-term changes. Nature 540, 418-422 (2016). (doi:10.1038/nature20584)
#' @source https://global-surface-water.appspot.com/download
"res_max"


#' Otto-basins level 12 for Ceará
#'
#' @docType data
#' @usage data(otto)
#'
#' @references Lehner, B., Verdin, K., Jarvis, A. (2008): New global hydrography derived from spaceborne elevation data. Eos, Transactions, AGU, 89(10): 93-94.
#' @source https://hydrosheds.org/downloads
"otto"


#' River network with upstream cell number for Ceará
#'
#' @docType data
#' @usage data(riv)
#'
#' @references Lehner, B., Verdin, K., Jarvis, A. (2008): New global hydrography derived from spaceborne elevation data. Eos, Transactions, AGU, 89(10): 93-94.
#' @source https://hydrosheds.org/downloads
"riv"

#' Nodes of the river network with upstream cell number for Ceará. Can be computed with `riv2nodes`
#'
#' @docType data
#' @usage data(nodes)
#'
#' @references Lehner, B., Verdin, K., Jarvis, A. (2008): New global hydrography derived from spaceborne elevation data. Eos, Transactions, AGU, 89(10): 93-94.
#' @source https://hydrosheds.org/downloads
"nodes"

#' A graph of the class `igraph` containing flow direction for the HydroSHEDS river network dataset in Ceará.
#'
#' @docType data
#' @usage data(river_graph)
#'
#' @references Lehner, B., Verdin, K., Jarvis, A. (2008): New global hydrography derived from spaceborne elevation data. Eos, Transactions, AGU, 89(10): 93-94.
#' @source https://hydrosheds.org/downloads
"river_graph"

#' A graph of the class `igraph` containing flow direction for the HydroSHEDS subbasin dataset in Ceará.
#'
#' @docType data
#' @usage data(otto_graph)
#'
#' @references Lehner, B., Verdin, K., Jarvis, A. (2008): New global hydrography derived from spaceborne elevation data. Eos, Transactions, AGU, 89(10): 93-94.
#' @source https://hydrosheds.org/downloads
"otto_graph"


#' A graph of the class `igraph`: routing of strategic and non-strategic reservoirs in Ceará.
#'
#' @docType data
#' @usage data(reservoir_graph)
#'
#' @source https://hydrosheds.org/downloads
#' @source https://global-surface-water.appspot.com/download
"reservoir_graph"
