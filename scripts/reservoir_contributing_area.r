library(a5udes)
library(dplyr)
library(sf)
library(tidygraph)

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
