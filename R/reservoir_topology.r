#' Reverse the edge direction of a directed graph
#' @param graph a igraph directed graph
#' @return reversed a graph with reversed direction
#' @importFrom igraph is.directed get.data.frame
#' @export
graph_reverse_direction <- function (graph) {
  if (!is.directed(graph))
    return(graph)
  e <- get.data.frame(graph, what="edges")
  ## swap "from" & "to"
  neworder <- 1:length(e)
  neworder[1:2] <- c(2,1)
  e <- e[neworder]
  names(e) <- names(e)[neworder]
  reversed=graph.data.frame(e, vertices = get.data.frame(graph, what="vertices"))
  return(reversed)
}


#' Allocate each reservoir to nearest river reach within a given subbasin
#' @param riv_i a subset of river reaches from `data(river_geometry)`
#' @param reservoirs a set of polygons or multipolygons of class `sf` to be matched with the river network. Defaults to the northeast Brazil reservoir dataset
#' @return res_geom subset of the reservoir data frame with the respective attributed river reach and distance to river reach
#' @importFrom sf st_nearest_feature
#' @export
allocate_reservoir_to_river <- function(riv_i,reservoirs=reservoir_geometry)
{
  otto_subset = st_intersects(catchment_geometry,st_union(riv_i),sparse=FALSE) %>% filter(catchment_geometry,.)
  res_geom = st_intersects(reservoirs,st_union(otto_subset),sparse=FALSE) %>% filter(reservoirs,.)
  res_geom = mutate(res_geom,`nearest river`=NA,`distance to river`=NA)
  for(i in seq(1,nrow(res_geom)))
  {
    riv_inters <- st_intersects(res_geom[i,],riv_i,sparse=FALSE) %>%
      filter(riv_i,.)

    # if reservoir intersects a river junction assume the downstreammost river reach as the nearest river
    if(nrow(riv_inters)>1) {
      riv_inters=riv_inters %>% filter(UPLAND_SKM==max(UPLAND_SKM))
    }

    if(nrow(riv_inters)==0)
    {
      otto_k=st_intersects(catchment_geometry,res_geom[i,],sparse=FALSE) %>% filter(catchment_geometry,.)
      riv_k = st_buffer(otto_k,-1000) %>%
      st_union %>%
      st_intersects(riv_i,.,sparse=FALSE) %>%
      filter(riv_i,.)

      if(nrow(riv_k)>0){
        res_geom$`nearest river`[i] = st_nearest_feature(res_geom[i,],riv_k) %>%
        riv_k$HYRIV_ID[.]

        res_geom$`distance to river`[i] = st_distance(res_geom[i,],filter(riv_k,HYRIV_ID==res_geom$`nearest river`[i]))
      }
    } else {
      res_geom$`nearest river`[i] = riv_inters$HYRIV_ID
      res_geom$`distance to river`[i] = 0
    }
  }
  return(res_geom %>% filter(!is.na(`nearest river`)))
}



#' Build reservoir topology starting from a river graph, river geometry and reservoir location
#'
#' This function builds the topology of the reservoir network. It starts by identifying which reservoirs
#' are located over the river network (strategic reservoirs) and builds their topology by filling the `res_down` column.
#' Then it looks at reservoirs outside the river network (in general smaller reservoirs and here called non-strategic)
#' and assigns them to a strategic reservoir (with `sf::st_nearest_feature()`) in case there is one in the river reach
#' or simply assigns the next downstream strategic reservoir as `res_down`
#'
#' @param res_geom is  a subset of `data(reservoir_geometry)` that can be obtained from the function `allocate_reservoir_to_river()`
#' @param riv_geom is a river geometry that can be created with `select_disjoint_river()`
#' @param riv_graph is a river graph created with `riv2graph()` based on riv_geom
#' @return the column ```res_down``` in the geospatial dataframe ```res_geom```
#' @importFrom sf st_set_geometry
#' @importFrom igraph all_simple_paths degree V
#' @importFrom dplyr %>% arrange coalesce right_join group_by group_split mutate select filter bind_rows left_join distinct
#' @export
build_reservoir_topology = function(res_geom,riv_geom,riv_graph){

  # add downstreamness (sorting within catchment) and UPLAND_SKM (sorting across catchments) columns
  res_geom_topo = res_geom %>% mutate(res_down=NA,downstreamness=NA,UPLAND_SKM=NA) %>% select(id_jrc,`nearest river`,`distance to river`,res_down,downstreamness,UPLAND_SKM,area_max)

  # group after nearest river reach ID
  res_geom_list=res_geom_topo %>% group_by(`nearest river`) %>% group_split(.keep=TRUE)

  # loop on river reach ID
  for(i in seq(1,length(res_geom_list))){
    strategic = res_geom_list[[i]] %>% filter(`distance to river`==0)
    non_strategic = res_geom_list[[i]] %>% filter(`distance to river`>0)

    riv_l=filter(riv_geom,HYRIV_ID==res_geom_list[[i]]$`nearest river`[1])
    if(nrow(strategic)>1){
      strategic_df=sort_n_strategic(strategic,riv_l)
      non_strategic_df=sort_non_strategic(strategic,non_strategic,riv_l)
    } else if(nrow(strategic)==1) {
      strategic_df=sort_1_strategic(strategic,riv_l)
      non_strategic_df=sort_non_strategic(strategic,non_strategic,riv_l)
    } else if(nrow(strategic)==0) {
        strategic_df = st_set_geometry(strategic,NULL)
        non_strategic_df = st_set_geometry(non_strategic,NULL) %>%
          mutate(UPLAND_SKM=riv_l$UPLAND_SKM[1])
    }
    res_geom_list[[i]]=bind_rows(strategic_df,non_strategic_df)
  }


  res_all=bind_rows(res_geom_list)

  leaves = which(degree(riv_graph, v = V(riv_graph), mode = "in")==0) %>%
    names(.)

  res_nas_filled=list()

  for(l in seq(1:length(leaves))){

    riv_downstr <- all_simple_paths(riv_graph,from=leaves[l],mode='out') %>%
      unlist %>% names(.) %>% unique

    strategic_nas=res_all %>%
      filter(`nearest river` %in% as.integer(riv_downstr)) %>%
      filter(is.na(res_down)) %>%
      filter(`distance to river`==0)

    non_strategic_df=res_all %>%
      filter(`nearest river` %in% as.integer(riv_downstr)) %>%
      filter(`distance to river`>0) %>%
      arrange(UPLAND_SKM,downstreamness) %>%
      tidyr::fill(res_down,.direction='up')

    strategic_df = strategic_nas %>%
      arrange(UPLAND_SKM,downstreamness) %>%
      mutate(res_down=lead(id_jrc))

    res_nas_filled[[l]] = bind_rows(strategic_df,non_strategic_df)
  }

  res_topo=bind_rows(res_nas_filled) %>%
    distinct(id_jrc,.keep_all=TRUE) %>%
    right_join(res_all,by='id_jrc') %>%
    mutate(res_down=coalesce(res_down.x,res_down.y)) %>%
    select(id_jrc,res_down)

  res_geom_out=left_join(res_geom,res_topo)

  return(res_geom_out)
}



#' Helper function for building topology or reservoirs
#'
#' Sorts non-strategic reservoirs by assigning them to nearest strategic whenever there is one
#'
#' @param strategic is a group of `res_geom_list` from `build_reservoir_topology()`. It is obtained by grouping by `nearest river`
#' @param non_strategic is a group of `res_geom_list` from `build_reservoir_topology()`. It is obtained by grouping by `nearest river`
#' @param riv_l is the river linestring corresponding to the group `res_geom_list` being handled ie the `nearest river`
#' @return non_strategic_df a dataframe with no geometry attributes with a ```res_down``` column filled where possible
#' @importFrom sf st_nearest_feature st_set_geometry
#' @importFrom dplyr %>% slice pull mutate
#' @export
sort_non_strategic = function(strategic,non_strategic,riv_l){
  upcells=riv_l$UPLAND_SKM[1]
  nn=st_nearest_feature(non_strategic,strategic)
  strat_ids=st_set_geometry(strategic,NULL) %>% slice(nn) %>% pull(id_jrc)
  non_strategic_df=st_set_geometry(non_strategic,NULL) %>%
    mutate(res_down=strat_ids)

  return(non_strategic_df)
}

#' Helper function for building topology of reservoirs
#'
#' Sorts strategic reservoirs whenever there is only one on the river network
#'
#' @param strategic is a group of `res_geom_list` from `build_reservoir_topology()`. It is obtained by grouping by `nearest river`
#' @param riv_l is the river linestring corresponding to the group `res_geom_list` being handled ie the `nearest river`
#' @return strategic_df a dataframe with no geometry attributes with a ```UP_CELLS``` column filled where possible
#' @importFrom sf st_set_geometry
#' @importFrom dplyr %>% mutate
sort_1_strategic = function(strategic,riv_l){
  upcells=riv_l$UPLAND_SKM[1]
  strategic_df=st_set_geometry(strategic,NULL) %>%
    mutate(UPLAND_SKM=upcells)

  return(strategic_df)
}

#' Helper function for building topology of reservoirs
#'
#' Sorts strategic reservoirs whenever there is more than one on the river network
#'
#' @param strategic is a group of `res_geom_list` from `build_reservoir_topology()`. It is obtained by grouping by `nearest river`
#' @param riv_l is the river linestring corresponding to the group `res_geom_list` being handled ie the `nearest river`
#' @return strategic_df a dataframe with no geometry attributes with a ```downstreamness``` and `UP_CELLS` column filled where possible
#' @importFrom sf st_set_geometry st_line_sample st_cast st_sf st_intersects
#' @importFrom dplyr %>% mutate row_number
sort_n_strategic = function(strategic,riv_l){
  upcells=riv_l$UPLAND_SKM[1]
  strategic_df=st_set_geometry(strategic,NULL)
  points <- st_line_sample(riv_l, n = 1000) %>%
    st_cast("POINT") %>%
    st_sf()
  inter <- st_intersects(strategic, points)

  for(i in seq(1,length(inter))){
    if(length(inter[[i]]>0)){
      strategic_df = strategic_df %>%
        mutate(
          downstreamness=ifelse(
            row_number()==i,
            max(inter[[i]]),
            downstreamness
            ),
          UPLAND_SKM=upcells)
    }
  }
  return(strategic_df)
}
