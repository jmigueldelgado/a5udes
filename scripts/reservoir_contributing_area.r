library(a5udes)
library(dplyr)
library(sf)
library(tidygraph)
library(ggraph)
library(igraph)

res_geom_df=reservoir_geometry %>%
  st_set_geometry(NULL) %>%
  dplyr::select(id_jrc,area_max,`distance to river`,`nearest river`) %>%
  rename(name=id_jrc)



river_geometry %>% names

# calculate area contribution of each river reach to the catchment

riv_graph_tbl=river_graph %>% activate(nodes) %>% as_tibble

parent_sum=river_graph %>%
  graph_reverse_direction %>%
  mutate(parent=bfs_parent(mode='out')) %>%
  as_tibble %>%
  group_by(parent) %>%
  summarise(parent_area=sum(UPLAND_SKM)) %>%
  filter(!is.na(parent)) %>%
  mutate(name = riv_graph_tbl$name[.$parent]) %>%
  select(-parent)

riv_g2=river_graph %>%
  left_join(parent_sum) %>%
  mutate(riv_contr_area=ifelse(is.na(parent_area),UPLAND_SKM,UPLAND_SKM-parent_area))
riv_g2 %>% as_tibble %>% head

riv_df=riv_g2 %>% activate(nodes) %>% as_tibble %>%
  dplyr::select(HYRIV_ID,riv_contr_area)

g=reservoir_graph %>%
  graph_reverse_direction %>%
  activate(nodes) %>%
  left_join(res_geom_df) %>%
  left_join(riv_df,by=c('nearest river'='HYRIV_ID')) %>%
  mutate(own_contr_area=ifelse(`distance to river`==0,riv_contr_area/xxxxx,area_max*0.3664))


g_tbl = g %>% as_tibble

parent_sum=g %>%
  mutate(parent=bfs_parent(mode='out')) %>%
  as_tibble %>%
  group_by(parent) %>%
  summarise(parent_area=sum(total_contr_area)) %>%
  filter(!is.na(parent)) %>%
  mutate(name = g_tbl$name[.$parent]) %>%
  select(-parent)

g2=g %>%
  left_join(parent_sum) %>%
  mutate(contr_area=ifelse(is.na(parent_area),own_contr_area,UPLAND_SKM-riv_contr_area+own_contr_area))  # solve all reservoirs without parent

g2 %>% as_tibble %>% filter(contr_area<0)


# plot example

sub_g=g %>% convert(to_local_neighborhood,node=which(.N()$name == "34560"), order = 2, mode = "out")
sub_g %>%  ggraph(.) +
geom_node_point() +
geom_edge_link0() +
geom_node_label(aes(label = name)) # repel=TRUE

# define neighborhood upstream of each reservoir

neigh = g %>% activate(nodes) %>%
  arrange(desc(rank)) %>%
  mutate(neighborhood =   local_members(order = 1,mode='out',mindist=1)) %>%
  as_tibble



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
