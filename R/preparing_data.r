


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



#' Allocate each reservoir to nearest river reach within a given subbasin
#' @param riv_i a subset of river reaches from `data(riv)`
#' @return reservoir_geometry_subset subset of the reservoir data frame with the respective attributed river reach and distance to river reach
#' @importFrom sf st_nearest_feature
#' @export
allocate_reservoir_to_river <- function(riv_i)
{
  otto_subset = st_intersects(otto,st_union(riv_i),sparse=FALSE) %>% filter(otto,.)
  reservoir_geometry_subset = st_intersects(reservoir_geometry,st_union(otto_subset),sparse=FALSE) %>% filter(reservoir_geometry,.)
  reservoir_geometry_subset = mutate(reservoir_geometry_subset,`nearest river`=NA,`distance to river`=NA)
  for(i in seq(1,nrow(reservoir_geometry_subset)))
  {
    riv_inters <- st_intersects(reservoir_geometry_subset[i,],riv_i,sparse=FALSE) %>%
      filter(riv_i,.) %>%
      filter(UP_CELLS==max(UP_CELLS))

    if(nrow(riv_inters)==0)
    {
      otto_k=st_intersects(otto,reservoir_geometry_subset[i,],sparse=FALSE) %>% filter(otto,.)
      riv_k = st_buffer(otto_k,-1000) %>%
      st_union %>%
      st_intersects(riv_i,.,sparse=FALSE) %>%
      filter(riv_i,.)

      if(nrow(riv_k)>0){
        reservoir_geometry_subset$`nearest river`[i] = st_nearest_feature(reservoir_geometry_subset[i,],riv_k) %>%
        riv_k$ARCID[.]

        reservoir_geometry_subset$`distance to river`[i] = st_distance(reservoir_geometry_subset[i,],filter(riv_k,ARCID==reservoir_geometry_subset$`nearest river`[i]))
      }
    } else {
      reservoir_geometry_subset$`nearest river`[i] = riv_inters$ARCID
      reservoir_geometry_subset$`distance to river`[i] = 0
    }
  }
  return(reservoir_geometry_subset)
}


#' Routing reservoirs to the river network
#'
#' This function identifies which reservoir is outside the river network and which river reach is closer and within the same catchment
#' @param reservoir_geometry_subset is  a subset `data(reservoir_geometry)` that can be obtained from the function `allocate_reservoir_to_river()`
#' @return completes the column ```res_down``` in the geospatial dataframe ```reservoir_geometry```
#' @importFrom sf st_distance
#' @importFrom igraph all_simple_paths
#' @importFrom dplyr %>%
#' @export

route_reservoir_to_river <- function(reservoir_geometry_subset){
  strategic <- reservoir_geometry_subset[reservoir_geometry_subset$`distance to river`==0,]
  non_strat <- reservoir_geometry_subset[reservoir_geometry_subset$`distance to river`>0,]
  g <- river_graph

  for(n in 1:nrow(non_strat)){

    if(n %in% c(2500,5000,7500,10000,12500,15000,16000,17500,20000)){
      print(paste(Sys.time(),n, "reservoirs done"))}

    strat_downstr <- strategic[strategic$`nearest river` %in% non_strat$`nearest river`[n],]

    if(nrow(strat_downstr)==0){

      riv_downstr <- all_simple_paths(g,from=rownames(riv[riv$ARCID==non_strat$`nearest river`[n],]),mode='out') %>%
        unlist %>% unique
      riv_l <- riv[riv_downstr,]
      strat_downstr <- subset(strategic, `nearest river` %in% riv_l$ARCID)
    }

    if(nrow(strat_downstr)>1){
      strat_downstr <- strat_downstr[st_distance(strat_downstr, non_strat[n,]) == min(st_distance(strat_downstr, non_strat[n,])),]
    }

    if(nrow(strat_downstr)==1){
      reservoir_geometry_subset$res_down[reservoir_geometry_subset$id_jrc==non_strat$id_jrc[n]] <- strat_downstr$id_jrc
    }else{
      reservoir_geometry_subset$res_down[reservoir_geometry_subset$id_jrc==non_strat$id_jrc[n]] <- NA
    }
  }

  return(reservoir_geometry_subset)
}


#' Routing reservoirs across the river network
#'
#' This function identifies which reservoirs are part of the river network and which reservoir do they drain into
#' @param reservoir_geometry_subset is  a subset `data(reservoir_geometry)` that can be obtained from the function `allocate_reservoir_to_river()`
#' @return the column ```res_down``` in the geospatial dataframe ```reservoir_geometry_subset```
#' @importFrom sf st_line_sample st_cast st_sf st_buffer st_intersection st_write
#' @importFrom igraph all_simple_paths degree V
#' @importFrom dplyr %>%
#' @export
route_reservoir_along_river <- function(reservoir_geometry_subset){
  reservoir_geometry_subset$res_down <- NA
  strategic <- reservoir_geometry_subset[reservoir_geometry_subset$`distance to river`==0,]

# for-loop through leaves ####
  g <- river_graph
  leaves = which(degree(g, v = V(g), mode = "in")==0)

  for(l in 1:length(leaves)){

    if(l %in% c(500,1000,1500)){print(paste(Sys.time(),l, "of", length(leaves), "leaves done"))}

    riv_downstr <- all_simple_paths(g,from=leaves[l],mode='out') %>%
    unlist %>% unique
    riv_l <- riv[riv_downstr,]
    strat_downstr <- subset(strategic, `nearest river` %in% riv_l$ARCID)

    if(nrow(strat_downstr) > 1){
      first <- strat_downstr[1,]
      last <- strat_downstr[nrow(strat_downstr),]

      if(max(first$UP_CELLS) > max(last$UP_CELLS)){
        strat_downstr <- strat_downstr[nrow(strat_downstr):1,]
        }

      for(s in 1:nrow(strat_downstr)){
          reservoir_geometry_subset$res_down[reservoir_geometry_subset$id_jrc == strat_downstr$id_jrc[s]] <- strat_downstr$id_jrc[s+1]
      }
    }
  }

# for-loop through river reaches with multiple reservoirs ####
  print(paste(Sys.time(), "start correcting order where multiple reservoirs on one river reach"))
  dup <- strategic[which(duplicated(strategic$`nearest river`)),]
  multiple_res <- reservoir_geometry_subset[reservoir_geometry_subset$`nearest river` %in% dup$`nearest river` & reservoir_geometry_subset$`distance to river`==0,]

  for(d in 1:length(unique(dup$`nearest river`))){
    riv_l <- riv[riv$ARCID==dup$`nearest river`[d],]
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
# correct res_down in reservoir_geometry_subset for this river reach
    reservoir_geometry_subset$res_down[reservoir_geometry_subset$id_jrc==r$id_jrc[nrow(r)]] <- strat_downstr$res_down[!(strat_downstr$res_down %in% strat_downstr$id_jrc)]
    reservoir_geometry_subset$res_down[reservoir_geometry_subset$res_down %in% strat_downstr$id_jrc] <- r$id_jrc[1]

    for(i in 1:(nrow(r)-1)){
      reservoir_geometry_subset$res_down[reservoir_geometry_subset$id_jrc==r$id_jrc[i]] <- r$id_jrc[i+1]
    }
  }

  reservoir_geometry_subset$res_down[is.na(reservoir_geometry_subset$res_down)] <- -1

  print(paste(Sys.time(), "finished!"))
  return(reservoir_geometry_subset)
}
