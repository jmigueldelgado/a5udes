





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



#' Routing reservoirs across the river network
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
route_reservoir_along_river <- function(res_geom,riv_geom=river_geometry,riv_graph){

  res_geom$res_down <- NA
  strategic_df <- st_set_geometry(res_geom,NULL) %>%
    filter(`distance to river`==0) %>%
    select(id_jrc,`nearest river`,`distance to river`,res_down)

  dups = strategic_df %>% group_by(`nearest river`) %>% filter(n()>1) %>% ungroup
  riv_reach = dups %>%
    distinct(`nearest river`) %>%
    rename(ARCID=`nearest river`) %>%
    left_join(riv_geom,by.y=ARCID,by.x=`nearest river`) %>%
    st_set_geometry('geometry') %>%
    st_set_crs(st_crs(riv_geom))

  res_in_reach=list()

  for(d in seq(1,nrow(riv_reach))){

      riv_l = riv_reach[d,]

      strat_downstr = res_geom %>% filter(`nearest river` == riv_l$ARCID)
      strat_downstr_df=st_set_geometry(strat_downstr,NULL)
      points <- st_line_sample(riv_l, n = 1000) %>%
        st_cast("POINT") %>%
        st_sf()
      inter <- st_intersects(strat_downstr, points)

      r=list()
      for(i in seq(1,length(inter))){
        if(length(inter[[i]]>0)){
          r[[i]] <- strat_downstr_df[i,] %>% mutate(sample_max=max(inter[[i]]))
        }
      }
      res_in_reach[[d]]=bind_rows(r) %>%
        filter(is.finite(sample_max))
  }

  res_in_reach=bind_rows(res_in_reach)

  riv_df=st_set_geometry(riv_geom,NULL)

  g <- riv_graph
  leaves = which(degree(g, v = V(g), mode = "in")==0) %>%
    names(.)


  res_on_paths=list()

  for(l in seq(1:length(leaves))){

    riv_downstr <- all_simple_paths(g,from=leaves[l],mode='out') %>%
      unlist %>% names(.) %>% unique
    riv_l <- riv_df %>% filter(ARCID %in% riv_downstr)

    res_on_paths[[l]] = inner_join(res_in_reach,riv_l,by=c('nearest river'='ARCID')) %>%
      arrange(desc(UP_CELLS),desc(sample_max)) %>%
      mutate(res_down=lag(id_jrc)) %>%
      select(id_jrc,res_down)
  }

  tmp=bind_rows(res_on_paths) %>% distinct(id_jrc,.keep_all=TRUE)

  res_geom=left_join(res_geom,tmp,by='id_jrc') %>%
    mutate(res_down=coalesce(res_down.x,res_down.y)) %>%
    select(-res_down.x,-res_down.y)

  return(res_geom)
}



#' Routing reservoirs to the river network
#'
#' This function identifies which reservoir is outside the river network and which river reach is closer and within the same catchment
#' @param res_geom is  a subset `data(reservoir_geometry)` that can be obtained from the function `allocate_reservoir_to_river()`
#' @return completes the column ```res_down``` in the geospatial dataframe ```reservoir_geometry```
#' @importFrom sf st_distance
#' @importFrom igraph all_simple_paths
#' @importFrom dplyr %>%
#' @export

route_reservoir_to_river <- function(res_geom){
  strategic <- res_geom[res_geom$`distance to river`==0,]
  non_strat <- res_geom[res_geom$`distance to river`>0,]
  g <- river_graph

  for(n in 1:nrow(non_strat)){

    if(n %in% c(2500,5000,7500,10000,12500,15000,16000,17500,20000)){
      print(paste(Sys.time(),n, "reservoirs done"))}

    strat_downstr <- strategic[strategic$`nearest river` %in% non_strat$`nearest river`[n],]

    if(nrow(strat_downstr)==0){

      riv_downstr <- all_simple_paths(g,from=rownames(river_geometry[river_geometry$ARCID==non_strat$`nearest river`[n],]),mode='out') %>%
        unlist %>% unique
      riv_l <- river_geometry[riv_downstr,]
      strat_downstr <- subset(strategic, `nearest river` %in% riv_l$ARCID)
    }

    if(nrow(strat_downstr)>1){
      strat_downstr <- strat_downstr[st_distance(strat_downstr, non_strat[n,]) == min(st_distance(strat_downstr, non_strat[n,])),]
    }

    if(nrow(strat_downstr)==1){
      res_geom$res_down[res_geom$id_jrc==non_strat$id_jrc[n]] <- strat_downstr$id_jrc
    }else{
      res_geom$res_down[res_geom$id_jrc==non_strat$id_jrc[n]] <- NA
    }
  }

  return(res_geom)
}
