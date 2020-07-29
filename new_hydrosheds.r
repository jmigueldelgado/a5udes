library(a5udes)
library(dplyr)
library(sf)
library(tidygraph)
library(igraph)
library(progress)
library(furrr)

# CE=osmdata::getbb(place_name='Ceará', format_out = "sf_polygon") %>% slice(1)

# catch=st_read('/home/delgado/Downloads/hydrosheds-317f5f6b1c07745a6086/hybas_sa_lev12_v1c')
# catch_ce=catch %>% st_filter(CE)
# rm(catch)
#
# catchment_geometry = catch_ce %>% dplyr::select(HYBAS_ID,NEXT_DOWN,SUB_AREA,UP_AREA) %>% st_transform(32724)
#
# save(catchment_geometry,file='data/catchment_geometry.RData')
#
# catchment_graph=catch2graph(catchment_geometry)
#
# save(catchment_graph,file='data/catchment_graph.RData')
# riv=st_read('/home/delgado/Downloads/HydroRIVERS_v10_sa_shp/HydroRIVERS_v10_sa_shp')
# riv_ce=riv %>% st_filter(CE)
# rm(riv)
#
# names(riv_ce)
#
# river_geometry=riv_ce %>% dplyr::select(HYRIV_ID,NEXT_DOWN,CATCH_SKM,UPLAND_SKM,HYBAS_L12) %>% st_transform(32724)
# save(river_geometry,file='data/river_geometry.RData')

# nodes=riv2nodes(river_geometry)
# river_graph=riv2graph(nodes,river_geometry)
# save(river_graph,file='data/river_graph.RData')

res_geom=allocate_reservoir_to_river(river_geometry)

res_geom=build_reservoir_topology(reservoir_geometry,river_geometry,river_graph)

reservoir_tidygraph = res_geom %>%
  st_set_geometry(NULL) %>%
  dplyr::select(id_jrc,res_down) %>%
  filter(res_down>0) %>%
  rename(to=id_jrc,from=res_down) %>%
  as_tbl_graph

res_geom_df=reservoir_geometry %>%
  st_set_geometry(NULL) %>%
  dplyr::select(id_jrc,area_max,SUB_AREA,`distance to river`,`nearest river`,`UP_CELLS`) %>%
  mutate(name=as.character(id_jrc))

res_tidy=reservoir_tidygraph %>%
  left_join(res_geom_df)

## not a tree! forest!! need to loop over trees first



g=res_tidy %>%
  mutate(rank=map_dfs_back_int(node_is_root(),unreachable=TRUE,.f=function(node,rank,...) {
    return(rank)
  }))

g


neigh = g %>% activate(nodes) %>%
  arrange(desc(rank)) %>%
  mutate(neighborhood =   local_members(order = 1,mode='out',mindist=1)) %>%
  as_tibble

neigh %>% head

contrib=vector(mode='list',length=nrow(neigh))

# for(i in seq(1,nrow(neigh))) {

  if(neigh$`distance to river`[i]>0){
    contrib[[i]]=neigh$area_max[i]*0.3664
  } else {
    riv_l=filter(river_geometry,ARCID==neigh$`nearest river`[i])


  }


  # }
riv_l=res_geom[1,] %>% pull()
res_geom[1,]
