library(a5udes)
library(dplyr)
library(sf)
library(tidygraph)


# rebuild topology and create graph WITHOUT NAs!!!

res_geom_df=reservoir_geometry %>%
  st_set_geometry(NULL) %>%
  dplyr::select(id_jrc,area_max,`distance to river`,`nearest river`,`UP_CELLS`)

res_tidy=reservoir_tidygraph %>%
  activate(edges) %>%
  filter(!is.na(to)) %>%
  filter(!is.na(from)) %>%
  activate(nodes) %>%
  filter(!is.na(name)) %>%
  rename(id_jrc=name) %>%
  left_join(res_geom_df)

g=res_tidy %>% graph_reverse_direction(.) %>%
  as_tbl_graph %>% mutate(rank=map_dfs_back_int(node_is_root(),.f=function(node,rank,...) {
    return(rank)
  }))

neigh = g %>% activate(nodes) %>%
  arrange(desc(rank)) %>%
  mutate(neighborhood =   local_members(order = 1,mode='out',mindist=1)) %>%
  as_tibble


riv_l=res_geom[1,] %>% pull()
res_geom[1,]
