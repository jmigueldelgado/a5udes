library(a5udes)
library(dplyr)
library(sf)
library(tidygraph)
library(ggraph)
library(igraph)
library(progress)
library(furrr)

CE=osmdata::getbb(place_name='Ceará', format_out = "sf_polygon") %>% slice(1)

res_geom=allocate_reservoir_to_river(river_geometry)
reservoir_geometry=build_reservoir_topology(res_geom)
save(reservoir_geometry,file='data/reservoir_geometry.RData')reservoir_tidygraph


reservoir_tidygraph = reservoir_geometry %>%
  st_set_geometry(NULL) %>%
  dplyr::select(id_jrc,res_down) %>%
  filter(res_down>0) %>%
  rename(to=id_jrc,from=res_down) %>%
  as_tbl_graph
save(reservoir_tidygraph,file='data/reservoir_tidygraph.RData')

# reservoir_tidygraph %>% morph(to_local_neighborhood,2,order=1) %>%
#   crystallise %>% .$graph %>% .[[1]] %>%
#   ggraph(.)+geom_node_label(aes(label=name))+geom_edge_bend0()
#
# reservoir_tidygraph %>% activate(nodes) %>% as_tibble %>% nrow

#
# res_geom_df=reservoir_geometry %>%
#   st_set_geometry(NULL) %>%
#   dplyr::select(id_jrc,area_max,SUB_AREA,`distance to river`,`nearest river`,`UP_CELLS`) %>%
#   mutate(name=as.character(id_jrc))
#
# res_tidy=reservoir_tidygraph %>%
#   left_join(res_geom_df)
#
# ## not a tree! forest!! need to loop over trees first
#
#
#
# g=res_tidy %>%
#   mutate(rank=map_dfs_back_int(node_is_root(),unreachable=TRUE,.f=function(node,rank,...) {
#     return(rank)
#   }))
#
# g
#
#
# neigh = g %>% activate(nodes) %>%
#   arrange(desc(rank)) %>%
#   mutate(neighborhood =   local_members(order = 1,mode='out',mindist=1)) %>%
#   as_tibble
#
# neigh %>% head
#
# contrib=vector(mode='list',length=nrow(neigh))
#
# # for(i in seq(1,nrow(neigh))) {
#
#   if(neigh$`distance to river`[i]>0){
#     contrib[[i]]=neigh$area_max[i]*0.3664
#   } else {
#     riv_l=filter(river_geometry,ARCID==neigh$`nearest river`[i])
#
#
#   }
#
#
#   # }
# riv_l=res_geom[1,] %>% pull()
# res_geom[1,]
