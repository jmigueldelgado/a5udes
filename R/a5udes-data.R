#' Geometry of reservoirs in Ceará as imported from jrc
#'
#' @docType data
#' @usage data(reservoir_geometry_raw)
#'
#' @references  Jean-Francois Pekel, Andrew Cottam, Noel Gorelick, Alan S. Belward, High-resolution mapping of global surface water and its long-term changes. Nature 540, 418-422 (2016). (doi:10.1038/nature20584)
#' @source https://global-surface-water.appspot.com/download
"reservoir_geometry_raw"

#' Geometry of reservoirs in Ceará after processing network topology
#'
#' @docType data
#' @usage data(reservoir_geometry)
#'
#' @references  Jean-Francois Pekel, Andrew Cottam, Noel Gorelick, Alan S. Belward, High-resolution mapping of global surface water and its long-term changes. Nature 540, 418-422 (2016). (doi:10.1038/nature20584)
#' @source https://global-surface-water.appspot.com/download
"reservoir_geometry"

#' Catchments of Otto-level 12 for Ceará
#'
#' @docType data
#' @usage data(catchment_geometry)
#'
#' @references Lehner, B., Verdin, K., Jarvis, A. (2008): New global hydrography derived from spaceborne elevation data. Eos, Transactions, AGU, 89(10): 93-94.
#' @source https://hydrosheds.org/downloads
"catchment_geometry"


#' River network with upstream cell number for Ceará
#'
#' @docType data
#' @usage data(river_geometry)
#'
#' @references Lehner, B., Verdin, K., Jarvis, A. (2008): New global hydrography derived from spaceborne elevation data. Eos, Transactions, AGU, 89(10): 93-94.
#' @source https://hydrosheds.org/downloads
"river_geometry"

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
#' @usage data(catchment_graph)
#'
#' @references Lehner, B., Verdin, K., Jarvis, A. (2008): New global hydrography derived from spaceborne elevation data. Eos, Transactions, AGU, 89(10): 93-94.
#' @source https://hydrosheds.org/downloads
"catchment_graph"


#' A graph of the class `igraph`: routing of strategic and non-strategic reservoirs in Ceará.
#'
#' @docType data
#' @usage data(reservoir_graph)
#'
#' @source https://hydrosheds.org/downloads
#' @source https://global-surface-water.appspot.com/download
"reservoir_graph"

#' A graph of the class `tidygraph`. Good for manipulating attributes of reservoirs as a graph.
#'
#' @docType data
#' @usage data(reservoir_tidygraph)
#'
#' @source https://hydrosheds.org/downloads
#' @source https://global-surface-water.appspot.com/download
"reservoir_tidygraph"

#' A graph of the class `tidygraph` containing flow direction derived from the HydroSHEDS hydrologically conditioned DEM in Ceará with a resolution of 30 arcsec.
#'
#' @docType data
#' @usage data(flow_direction_tidygraph)
#'
#' @references Lehner, B., Verdin, K., Jarvis, A. (2008): New global hydrography derived from spaceborne elevation data. Eos, Transactions, AGU, 89(10): 93-94.
#' @source https://hydrosheds.org/downloads
"flow_direction_tidygraph"
