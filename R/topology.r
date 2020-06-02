


#' Obtain graph nodes from HydroSheds river network
#'
#' Extract the nodes of the HydroSheds river network.
#' @param riv a sf dataframe with a topologicaly valid river network. Each linestrings is ordered from upstream (first point of the linestring) to downstream (last point of the linestring), just like in the HydroSheds dataset
#' @return nodes a sf dataframe of points marking the nodes of the river network defined as the inlet of each river reach.
#' @importFrom sf st_geometry_type st_line_sample st_linestring st_cast
#' @importFrom dplyr filter
#' @importFrom magrittr %>% %<>%
#' @export
riv2nodes <- function(riv){

  nodes=riv
  for(i in seq(1,nrow(nodes)))
  {
    if(st_geometry_type(riv[i,])=='LINESTRING')
    {
      nodes$geometry[i]=st_line_sample(riv[i,],sample=0)
    }
    else
    {
      nodes$geometry[i]=st_linestring()
    }
  }

  valid=st_geometry_type(nodes)=='MULTIPOINT'

  nodes %<>%
    filter(valid) %>%
    st_cast(., "POINT", group_or_split = FALSE)

  return(nodes)
}


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

  g = graph.adjlist(touching_list)
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


#' Calculate graph object based on river network
#' @param nodes_i a sf dataframe of points marking the nodes of the river network defined as the inlet of each river reach. This must be only one tree. It won't work with a forest.
#' @param riv_i a sf dataframe with a topologicaly valid river network. This must be only one tree. It won't work with a forest.
#' @return g a igraph object
#' @importFrom sf st_touches
#' @importFrom igraph graph.adjlist components
#' @export
riv2graph <- function(nodes_i,riv_i){
  touch=st_touches(nodes_i,riv_i)
  for(i in seq(1,length(touch))){
    touch[[i]]=setdiff(touch[[i]],i)
  }
  g=graph.adjlist(touch, mode='in')
  return(g)
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

#' Allocate each reservoir to nearest river reach within a given subbasin
#' @param riv_i a subset of river reaches from `data(riv)`
#' @return res_max_subset subset of the reservoir data frame with the respective attributed river reach and distance to river reach
#' @importFrom sf st_nearest_feature
#' @export
allocate_reservoir_to_river <- function(riv_i)
{
  otto_subset = st_intersects(otto,st_union(riv_i),sparse=FALSE) %>% filter(otto,.)
  res_max_subset = st_intersects(res_max,st_union(otto_subset),sparse=FALSE) %>% filter(res_max,.)
  res_max_subset = mutate(res_max_subset,`nearest river`=NA,`distance to river`=NA)
  for(i in seq(1,nrow(res_max_subset)))
  {
    riv_inters <- st_intersects(res_max_subset[i,],riv_i,sparse=FALSE) %>%
      filter(riv_i,.) %>%
      filter(UP_CELLS==max(UP_CELLS))

    if(nrow(riv_inters)==0)
    {
      otto_k=st_intersects(otto,res_max_subset[i,],sparse=FALSE) %>% filter(otto,.)
      riv_k = st_buffer(otto_k,-1000) %>%
      st_union %>%
      st_intersects(riv_i,.,sparse=FALSE) %>%
      filter(riv_i,.)

      if(nrow(riv_k)>0){
        res_max_subset$`nearest river`[i] = st_nearest_feature(res_max_subset[i,],riv_k) %>%
        riv_k$ARCID[.]

        res_max_subset$`distance to river`[i] = st_distance(res_max_subset[i,],filter(riv_k,ARCID==res_max_subset$`nearest river`[i]))
      }
    } else {
      res_max_subset$`nearest river`[i] = riv_inters$ARCID
      res_max_subset$`distance to river`[i] = 0
    }
  }
  return(res_max_subset)
}
