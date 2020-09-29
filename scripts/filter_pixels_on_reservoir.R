library(raster)
library(a5udes)
library(rgdal)
library(ggplot2)
library(dplyr)
library(tidygraph)
library(ggraph)

example_id=36304

flows_from=raster("data/flows_from.tif")


res1=as(filter(reservoir_geometry_raw,id_jrc==example_id),'Spatial')

plot(res1)

flows_from_1=raster::crop(flows_from,res1)


if(length(flows_from_1)>1){
  flows_from_1 <- raster::mask(flows_from_1, res1)
}

raster_ids_in_reservoir=flows_from_1 %>% as.data.frame %>% filter(!is.na(flows_from))

nodes_inside_reservoir=flow_direction_tidygraph %>%
  activate('edges') %>%
  filter(to %in% raster_ids_in_reservoir$flows_from) %>%
  activate(nodes) %>%
  mutate(G=centrality_degree()) %>%
  filter(G>0)



new_root=flows_from_1[1,1]

split_tree = flow_direction_tidygraph %>%
  activate('edges') %>%
  filter(to!=new_root) %>% # dissect
  activate('nodes') %>%
  mutate(group=as.factor(group_components())) # attribute group label

tree1=split_tree %>%
  activate('nodes') %>%
  group_by(group) %>%
  filter(any(value==new_root)) %>%
  ungroup






## example of dissecting a tree by cutting one edge and splitting into components

### create a tree
tree=create_tree(10, 3, directed = TRUE, mode = "out")

### dissect and split tree
new_root=3
split_tree = tree %>%
  activate('edges') %>%
  filter(to!=new_root) %>% # dissect
  activate('nodes') %>%
  mutate(group=as.factor(group_components())) # attribute group label

### plot
ggraph(split_tree, layout ='dendrogram') +
  geom_edge_link(arrow = arrow(length = unit(4, 'mm'))) +
  geom_node_point(size=5,aes(color=group))
