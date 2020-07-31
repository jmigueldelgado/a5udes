library(a5udes)
library(dplyr)
library(sf)
library(tidygraph)
library(igraph)
library(progress)
library(furrr)

CE=osmdata::getbb(place_name='Ceará', format_out = "sf_polygon") %>% slice(1)

catch=st_read('./data/hybas_sa_lev12_v1c')
catch_ce=catch %>% st_filter(CE)
rm(catch)

catchment_geometry = catch_ce %>% dplyr::select(HYBAS_ID,NEXT_DOWN,SUB_AREA,UP_AREA) %>% st_transform(32724)
save(catchment_geometry,file='data/catchment_geometry.RData')

catchment_graph=catch2graph(catchment_geometry)
save(catchment_graph,file='data/catchment_graph.RData')
