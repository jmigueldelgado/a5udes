library(a5udes)
library(dplyr)
library(sf)
library(tidygraph)
library(igraph)
library(progress)
library(furrr)

CE=osmdata::getbb(place_name='Ceará', format_out = "sf_polygon") %>% slice(1)

riv=st_read('./data/HydroRIVERS_v10_sa_shp')
riv_ce=riv %>% st_filter(CE)
rm(riv)

river_geometry=riv_ce %>% dplyr::select(HYRIV_ID,NEXT_DOWN,CATCH_SKM,UPLAND_SKM,HYBAS_L12) %>% st_transform(32724)
save(river_geometry,file='data/river_geometry.RData')

nodes=riv2nodes(river_geometry)
river_graph=riv2graph(nodes,river_geometry)
save(river_graph,file='data/river_graph.RData')
