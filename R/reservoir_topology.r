





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
  strategic_df <- st_set_geometry(strategic,NULL) %>%
    filter(`distance to river`==0) %>%
    select(id_jrc,`nearest river`,`distance to river`,res_down)

  riv_df=st_set_geometry(riv_geom,NULL)

# for-loop through leaves ####
  g <- riv_graph
  leaves = which(degree(g, v = V(g), mode = "in")==0) %>%
    names(.)

  res_on_paths=list()
  for(l in 1:length(leaves)){

    if(l %in% c(500,1000,1500)){print(paste(Sys.time(),l, "of", length(leaves), "leaves done"))}

    # get path from given leaf to root
    riv_downstr <- all_simple_paths(g,from=leaves[l],mode='out') %>%
      unlist %>% names(.) %>% unique
    riv_l <- riv_df %>% filter(ARCID %in% riv_downstr)

    # get all reservoirs on this path and sort them according to UP_CELLS
    res_on_paths[[l]] = inner_join(strategic_df,riv_l,by=c('nearest river'='ARCID')) %>%
      arrange(desc(UP_CELLS)) %>%
      mutate(res_down=lag(id_jrc)) %>%
      select(id_jrc,res_down)
  }

  # first step in attributing upstream-downstream relationship: following UP_CELLS
  tmp=bind_rows(res_on_paths) %>% distinct(id_jrc,.keep_all=TRUE)
  res_geom=left_join(res_geom,tmp,by='id_jrc') %>%
    mutate(res_down=coalesce(res_down.x,res_down.y)) %>%
    select(-res_down.x,-res_down.y)

# for-loop through river reaches with multiple reservoirs ####
  print(paste(Sys.time(), "start correcting order where multiple reservoirs on one river reach"))
  dup <- strategic_df[which(duplicated(strategic_df$`nearest river`)),]
  multiple_res <- res_geom[res_geom$`nearest river` %in% dup$`nearest river` & res_geom$`distance to river`==0,]

  for(d in 1:length(unique(dup$`nearest river`))){
    riv_l <- river_geometry[river_geometry$ARCID==dup$`nearest river`[d],]
    strat_downstr <- subset(multiple_res, `nearest river` == riv_l$ARCID)

    points <- st_line_sample(riv_l, n = 200)
    points <- st_cast(points, "POINT")
    points <- st_sf(points)
    points$sample <- 1:200
    points <- st_buffer(points, dist = 100)

    inter <- st_intersection(strat_downstr, points)
    r <- data.frame(id_jrc = unique(inter$id_jrc))

    for(i in 1:nrow(r)){
      r$sample_max[i] <- max(inter$sample[inter$id_jrc== r$id_jrc[i]])
    }
# correct res_down in res_geom for this river reach
    res_geom$res_down[res_geom$id_jrc==r$id_jrc[nrow(r)]] <- strat_downstr$res_down[!(strat_downstr$res_down %in% strat_downstr$id_jrc)]
    res_geom$res_down[res_geom$res_down %in% strat_downstr$id_jrc] <- r$id_jrc[1]

    for(i in 1:(nrow(r)-1)){
      res_geom$res_down[res_geom$id_jrc==r$id_jrc[i]] <- r$id_jrc[i+1]
    }
  }

  res_geom$res_down[is.na(res_geom$res_down)] <- -1

  print(paste(Sys.time(), "finished!"))
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
