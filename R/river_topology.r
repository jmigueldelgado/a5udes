
#' Obtain graph nodes from HydroSheds river network
#'
#' Extract the nodes of the HydroSheds river network.
#' @param riv a sf dataframe with a topologicaly valid river network. Each linestrings is ordered from upstream (first point of the linestring) to downstream (last point of the linestring), just like in the HydroSheds dataset
#' @return nodes a sf dataframe of points marking the nodes of the river network defined as the inlet of each river reach.
#' @importFrom sf st_dimension st_set_geometry st_geometry_type st_line_sample st_linestring st_cast st_point
#' @importFrom dplyr filter bind_rows bind_cols
#' @importFrom magrittr %>% %<>%
#' @export
riv2nodes <- function(riv){

  line2point = function(geom){
    if(st_geometry_type(geom)=='LINESTRING') {
      geom_out=st_line_sample(geom,sample=0)
    } else {
      geom_out=st_point()
    }
    return(st_sf(geom_out))
  }

  geom_list=as.list(riv$geometry)
  nodes=lapply(geom_list,line2point) %>% bind_rows %>% bind_cols(.,st_set_geometry(riv,NULL))

  cat('filtering invalid geometries\n')
  nodes %<>% filter(!is.na(st_dimension(.))) %>%
    st_cast(., "POINT", group_or_split = FALSE) %>% st_set_crs(st_crs(riv))
  return(nodes)
}


#' Calculate graph object based on river network
#' @param nodes_i a sf dataframe of points marking the nodes of the river network defined as the inlet of each river reach. This must be only one tree. It won't work with a forest.
#' @param riv_i a sf dataframe with a topologicaly valid river network. This must be only one tree. It won't work with a forest.
#' @return g a igraph object
#' @importFrom sf st_touches st_set_geometry
#' @importFrom igraph graph.adjlist components
#' @importFrom tidygraph as_tbl_graph activate
#' @importFrom dplyr %>% left_join mutate
#' @export
riv2graph <- function(nodes_i,riv_i){
  touch=st_touches(nodes_i,riv_i,sparse=FALSE)
  rownames(touch)=nodes_i$HYRIV_ID
  colnames(touch)=riv_i$HYRIV_ID
  diag(touch)=FALSE

  g=graph_from_adjacency_matrix(t(touch), mode='directed') %>%
    as_tbl_graph %>%
    activate(nodes) %>%
    left_join(st_set_geometry(riv_i,NULL) %>% mutate(name=as.character(HYRIV_ID)))

  return(g)
}



#' Split river network into disjoint graphs
#' @param riv a sf dataframe with a topologicaly valid river network. Use for example the HydroSheds dataset obtained by `data(river_geometry)`.
#' @return riv_n a sf dataframe with a topologicaly valid river network with a membership label for each disjoint graph.
#' @importFrom sf st_touches
#' @importFrom igraph graph.adjlist components
#' @importFrom dplyr mutate arrange
#' @importFrom magrittr %>%
#' @export
split_river_network <- function(riv){
  # touching_list=st_touches(riv)
  adjacency_list=st_touches(riv)

  # g = graph.adjlist(touching_list)
  g = graph.adjlist(adjacency_list)
  c = components(g)

  riv_n=mutate(riv,membership=as.factor(c$membership)) %>%
    arrange()
  return(riv_n)
}

#' Select disjoint river network from set of river networks based on reach id
#' @param reach_id an integer obtained from columne `UPLAND_SKM` of `data(river_geometry)`
#' @param riv_all a list of disjoint river networks obtained from `split_river_network()`.
#' @return riv_i a disjoint river network
#' @importFrom dplyr filter pull
#' @export
select_disjoint_river <- function(reach_id,riv_all)
{
  riverid=filter(riv_all,HYRIV_ID==reach_id) %>% pull(membership)
  riv_i = filter(riv_all,membership==riverid)
  return(riv_i)
}


#' calculate contributing river network to a given river reach
#' @param reach_id from `data(river_geometry)`
#' @param riv_i a subset of river reaches of class sf from `data(river_geometry)`
#' @param graph a graph of class igraph produced with `riv2graph()`
#' @return riv_upstr the contributing river network
#' @importFrom dplyr slice
#' @importFrom igraph all_simple_paths
#' @export
river_upstream <- function(reach_id,riv_i,graph)
{
  riv_upstr = which(riv_i$HYRIV_ID==reach_id) %>%
    all_simple_paths(graph,from=.,mode='in') %>%
    unlist %>%
    unique %>%
    slice(riv_i,.)
  list_upstr = which(riv_i$HYRIV_ID==reach_id) %>%
    all_simple_paths(graph,from=.,mode='in')

  if(length(list_upstr)>0)
  {
    riv_upstr = list_upstr %>%
      unlist %>%
      unique %>%
      slice(riv_i,.)
  } else
  {
    riv_upstr = filter(riv_i,HYRIV_ID==reach_id)
  }

  return(riv_upstr)
}
