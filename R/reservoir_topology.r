





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
      filter(riv_i,.) %>%
      filter(UP_CELLS==max(UP_CELLS))

    if(nrow(riv_inters)==0)
    {
      otto_k=st_intersects(catchment_geometry,res_geom[i,],sparse=FALSE) %>% filter(catchment_geometry,.)
      riv_k = st_buffer(otto_k,-1000) %>%
      st_union %>%
      st_intersects(riv_i,.,sparse=FALSE) %>%
      filter(riv_i,.)

      if(nrow(riv_k)>0){
        res_geom$`nearest river`[i] = st_nearest_feature(res_geom[i,],riv_k) %>%
        riv_k$ARCID[.]

        res_geom$`distance to river`[i] = st_distance(res_geom[i,],filter(riv_k,ARCID==res_geom$`nearest river`[i]))
      }
    } else {
      res_geom$`nearest river`[i] = riv_inters$ARCID
      res_geom$`distance to river`[i] = 0
    }
  }
  return(res_geom)
}



#' create_reservoir_topology
#'
#' This function identifies which reservoirs are part of the river network and which reservoir do they drain into
#' @param res_geom is  a subset of `data(reservoir_geometry)` that can be obtained from the function `allocate_reservoir_to_river()`
#' @param riv_graph is a river graph created with `riv2graph()` based on riv_geom
#' @param riv_geom is a river geometry possibly created with created with `select_disjoint_river()`
#' @return the column ```res_down``` in the geospatial dataframe ```res_geom```
#' @importFrom sf st_line_sample st_cast st_sf st_buffer st_intersection st_write
#' @importFrom igraph all_simple_paths degree V
#' @importFrom dplyr %>%
#' @export
create_reservoir_topology = function(res_geom,riv_graph,riv_geom){

  res_geom$res_down <- NA
  res_geom$downstreamness <- NA
  res_geom$UP_CELLS <- NA

  res_geom_topo = res_geom %>% select(id_jrc,`nearest river`,`distance to river`,res_down,downstreamness)

  res_geom_list=res_geom_topo %>% group_by(`nearest river`) %>% group_split(keep=TRUE)

  i=1
  for(i in seq(1,length(res_geom_list))){
    strategic = res_geom_list[[i]] %>% filter(`distance to river`==0)
    non_strategic = res_geom_list[[i]] %>% filter(`distance to river`>0)

    riv_l=filter(riv_geom,ARCID==res_geom_list[[i]]$`nearest river`[1])
    if(nrow(strategic)>1){
      strategic_df=sort_n_strategic(strategic,riv_l)
      non_strategic_df=sort_non_strategic(strategic,non_strategic,riv_l)
    } else if(nrow(strategic)==1) {
      strategic_df=sort_1_strategic(strategic,riv_l)
      non_strategic_df=sort_non_strategic(strategic,non_strategic,riv_l)
    } else if(nrow(strategic)==0) {
        strategic_df = st_set_geometry(strategic,NULL)
        non_strategic_df = st_set_geometry(non_strategic,NULL) %>%
          mutate(UP_CELLS=riv_l$UP_CELLS[1])
    }
    res_geom_list[[i]]=bind_rows(strategic_df,non_strategic_df)
  }


  res_all=bind_rows(res_geom_list)

  strategic_nas=res_all %>% filter(is.na(res_down)) %>% filter(`distance to river`==0)
  non_strategic_nas=res_all %>% filter(is.na(res_down)) %>% filter(`distance to river`>0)

  leaves = which(degree(riv_graph, v = V(riv_graph), mode = "in")==0) %>%
    names(.)

  res_nas_filled=list()

  for(l in seq(1:length(leaves))){

    riv_downstr <- all_simple_paths(riv_graph,from=leaves[l],mode='out') %>%
      unlist %>% names(.) %>% unique

    strategic_df = strategic_nas %>%
      arrange(UP_CELLS,downstreamness) %>%
      mutate(res_down=lead(id_jrc))

    res_nas_filled[[l]] = bind_rows(strategic_df,non_strategic_nas) %>%
      arrange(UP_CELLS,downstreamness) %>%
      tidyr::fill(res_down,.direction='up')
  }

  res_topo=bind_rows(res_nas_filled) %>%
    right_join(res_all,by='id_jrc') %>%
    mutate(res_down=coalesce(res_down.x,res_down.y)) %>%
    select(id_jrc,res_down)



  sort_non_strategic = function(strategic,non_strategic,riv_l){
    upcells=riv_l$UP_CELLS[1]
    nn=st_nearest_feature(non_strategic,strategic)
    strat_ids=strategic_df %>% slice(nn) %>% pull(id_jrc)
    strategic_df = st_set_geometry(strategic,NULL)
    non_strategic_df=st_set_geometry(non_strategic,NULL) %>%
      mutate(res_down=strat_ids)

    return(non_strategic_df)
  }

  sort_1_strategic = function(strategic,riv_l){
    upcells=riv_l$UP_CELLS[1]
    strategic_df=st_set_geometry(strategic,NULL) %>%
      mutate(UP_CELLS=upcells)

    return(strategic_df)
  }

  sort_n_strategic = function(strategic,riv_l){
    upcells=riv_l$UP_CELLS[1]
    strategic_df=st_set_geometry(strategic,NULL)
    points <- st_line_sample(riv_l, n = 1000) %>%
      st_cast("POINT") %>%
      st_sf()
    inter <- st_intersects(strategic, points)

    for(i in seq(1,length(inter))){
      if(length(inter[[i]]>0)){
        strategic_df = strategic_df %>% mutate(downstreamness=ifelse(row_number()==i,max(inter[[i]]),downstreamness),UP_CELLS=upcells)
      }
    }
    return(strategic_df)
  }

  return(res_topo)
}
