


#' Split river network into disjoint graphs
#' @param riv a sf dataframe with a topologicaly valid river network. Use for example the HydroSheds dataset.
#' @return riv_n a sf dataframe with a topologicaly valid river network with a membership label for each disjoint graph.
#' @importFrom sf st_touches
#' @importFrom igraph graph.adjlist components
#' @importFrom dplyr mutate arrange
#' @importFrom magrittr %>%
#' @export
split_river_network <- function(riv){
  touching_list=st_touches(riv)
  adjacency_list=st_touches(riv)

  # g = graph.adjlist(touching_list)
  g = graph.adjlist(adjacency_list)
  c = components(g)

  riv_n=mutate(riv,membership=as.factor(c$membership)) %>%
    arrange()
  return(riv_n)
}

#' Select disjoint river network from set of river networks based on reach id
#' @param reach_id an integer obtained from columne `ARCID` of `data(riv)`
#' @param riv_all a list of disjoint river networks obtained from `split_river_network()`.
#' @return riv_i a disjoint river network
#' @importFrom dplyr filter pull
#' @export
select_disjoint_river <- function(reach_id,riv_all)
{
  riverid=filter(riv_all,ARCID==reach_id) %>% pull(membership)
  riv_i = filter(riv_all,membership==riverid)
  return(riv_i)
}


#' calculate contributing river network to a given river reach
#' @param reach_id from `data(riv)`
#' @param riv_i a subset of river reaches from `data(riv)`
#' @param graph a graph of class igraph produced with `riv2graph()`
#' @return riv_upstr the contributing river network
#' @importFrom dplyr slice
#' @importFrom igraph all_simple_paths
#' @export
river_upstream <- function(reach_id,riv_i,graph)
{
  riv_upstr = which(riv_i$ARCID==reach_id) %>%
    all_simple_paths(graph,from=.,mode='in') %>%
    unlist %>%
    unique %>%
    slice(riv_i,.)
  list_upstr = which(riv_i$ARCID==reach_id) %>%
    all_simple_paths(graph,from=.,mode='in')

  if(length(list_upstr)>0)
  {
    riv_upstr = list_upstr %>%
      unlist %>%
      unique %>%
      slice(riv_i,.)
  } else
  {
    riv_upstr = filter(riv_i,ARCID==reach_id)
  }

  return(riv_upstr)
}
