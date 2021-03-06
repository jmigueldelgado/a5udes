#' Reverse the edge direction of a directed graph
#' @param graph a igraph directed graph or a tbl_graph
#' @return reversed a graph with reversed direction
#' @importFrom igraph is.directed get.data.frame
#' @importFrom tidygraph tbl_graph activate
#' @importFrom dplyr mutate select as_tibble
#' @export
graph_reverse_direction <- function (graph) {
  if (is.tbl_graph(graph)) {
    if(!with_graph(graph,graph_is_directed())){
      stop('graph must be directed in order to be reversable')
    }
    edge_tbl=graph %>% activate(edges) %>% as_tibble %>% mutate(new_to=from, new_from=to) %>% mutate(from=new_from,to=new_to) %>% dplyr::select(from,to) %>% as_tibble
    node_tbl=graph %>% activate(nodes) %>% as_tibble
    reversed=tbl_graph(nodes=node_tbl,edges=edge_tbl)
  } else if(is.igraph(graph)){
    if (!is.directed(graph)){
      stop('graph must be directed in order to be reversable')
    }
    e <- get.data.frame(graph, what="edges")
    ## swap "from" & "to"
    neworder <- 1:length(e)
    neworder[1:2] <- c(2,1)
    e <- e[neworder]
    names(e) <- names(e)[neworder]
    reversed=graph.data.frame(e, vertices = get.data.frame(graph, what="vertices"))
  } else stop("input must be of class tbl_graph or igraph")
  return(reversed)
}


#' Allocate each reservoir to nearest river reach within a given subbasin
#' @param riv_i a subset of river reaches from `data(river_geometry)`
#' @param reservoirs a set of polygons or multipolygons of class `sf` to be matched with the river network. Defaults to the northeast Brazil reservoir dataset
#' @return res_geom subset of the reservoir data frame with the respective attributed river reach and distance to river reach
#' @importFrom sf st_nearest_feature st_intersects st_filter st_buffer st_distance st_sf
#' @importFrom progress progress_bar
#' @importFrom dplyr filter mutate tibble left_join bind_rows
#' @importFrom purrr map
#' @importFrom furrr future_map
#' @export
allocate_reservoir_to_river <- function(riv_i,reservoirs=reservoir_geometry_raw,catchments=catchment_geometry)
{
  print('preparing data for analysis and filtering out catchments and reservoir outside river network\n')
  otto_subset = st_intersects(catchments,st_union(riv_i),sparse=FALSE) %>% filter(catchments,.)
  res_geom = st_intersects(reservoirs,st_union(otto_subset),sparse=FALSE) %>% filter(reservoirs,.)

  pb <- progress_bar$new(total = nrow(res_geom))

  get_nearest_and_id = function(id_and_geom) {

    pb$tick()
    Sys.sleep(0.1)
    geom=id_and_geom$geometry %>% st_sfc(.,crs=st_crs(res_geom))
    riv_inters = riv_i %>% st_filter(geom)
    if(nrow(riv_inters)>1) {
      riv_inters=riv_inters %>% filter(UPLAND_SKM==max(UPLAND_SKM))
    }

    if(nrow(riv_inters)==0) {
      otto_k = otto_subset %>% st_filter(geom)
      riv_k = st_buffer(otto_k,-1000) %>%
        st_union %>%
        st_intersects(riv_i,.,sparse=FALSE) %>%
        filter(riv_i,.)

      if(nrow(riv_k)>0){
        nearest_riv = st_nearest_feature(geom,riv_k) %>%
          riv_k$HYRIV_ID[.]
        distance2riv = st_distance(geom,filter(riv_k,HYRIV_ID==nearest_riv)) %>% as.numeric
      }
    } else {
      nearest_riv = riv_inters$HYRIV_ID
      distance2riv = 0
    }
    return(tibble(id_jrc=id_and_geom$id_jrc,`nearest river`=nearest_riv,`distance to river`=distance2riv))
  }

  geom_ls = res_geom %>% dplyr::select(id_jrc) %>% purrr::transpose(.)
  if("furrr" %in% (.packages())){
    plan(multiprocess)
    map_out=geom_ls %>% future_map(get_nearest_and_id,.progress=TRUE)
  } else {
      map_out=geom_ls %>% map(get_nearest_and_id)
  }

  map_out %>%
    bind_rows %>%
    right_join(res_geom) %>%
    filter(!is.na(`nearest river`)) %>%
    st_sf %>%
    return

}



#' Build reservoir topology starting from a river graph, river geometry and reservoir location
#'
#' This function builds the topology of the reservoir network. Depends on `data(river_graph)` and `data(river_geometry)`. It starts by identifying which reservoirs
#' are located over the river network (strategic reservoirs) and builds their topology by filling the `res_down` column.
#' Then it looks at reservoirs outside the river network (in general smaller reservoirs and here called non-strategic)
#' and assigns them to a strategic reservoir (with `sf::st_nearest_feature()`) in case there is one in the river reach
#' or simply assigns the next downstream strategic reservoir as `res_down`
#'
#' @param res_geom is  a subset of `data(reservoir_geometry)` that can be obtained from the function `allocate_reservoir_to_river()`
#' @return the column ```res_down``` in the geospatial dataframe ```res_geom```
#' @importFrom sf st_set_geometry
#' @importFrom igraph all_simple_paths degree V
#' @importFrom dplyr %>% arrange coalesce right_join group_by group_split mutate select filter bind_rows left_join distinct
#' @export
build_reservoir_topology = function(res_geom){

  # add downstreamness (sorting within catchment) and UPLAND_SKM (sorting across catchments) columns
  res_geom_topo = res_geom %>% mutate(res_down=NA,downstreamness=NA,UPLAND_SKM=NA) %>% select(id_jrc,`nearest river`,`distance to river`,res_down,downstreamness,UPLAND_SKM,area_max)

  # group after nearest river reach ID
  res_geom_list=res_geom_topo %>% group_by(`nearest river`) %>% group_split(.keep=TRUE)



  # loop on river reach ID
  if("furrr" %in% (.packages())){
    plan(multiprocess)
    res_all=res_geom_list %>% future_map(sort_reservoirs) %>% bind_rows
  } else {
    res_all=res_geom_list %>% map(sort_reservoirs) %>% bind_rows
  }

  g = as.igraph(river_graph)

  leaves = which(degree(g, v = V(g), mode = "in")==0) %>%
    names(.)

  res_nas_filled=list()
  for(l in seq(1:length(leaves))){

    riv_downstr <- all_simple_paths(g,from=leaves[l],mode='out') %>%
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
    select(id_jrc,res_down,downstreamness)

  res_geom_out=left_join(res_geom,res_topo)

  return(res_geom_out)
}


#' Helper function for building topology or reservoirs
#'
#' Sets sorting attributes to all reservoirs by assigning them to nearest strategic whenever there is one and running one of the helper functions below
#'
#' @param res_geom_i is a subset of `data(reservoir_geometry)` group-split by `nearest river`
#' @return df a dataframe with sorting attributes
#' @importFrom dplyr %>% mutate filter
#' @export
sort_reservoirs = function(res_geom_i){
  strategic = res_geom_i %>% filter(`distance to river`==0)
  non_strategic = res_geom_i %>% filter(`distance to river`>0)

  riv_l=filter(river_geometry,HYRIV_ID==res_geom_i$`nearest river`[1])
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
  bind_rows(strategic_df,non_strategic_df) %>% return
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
