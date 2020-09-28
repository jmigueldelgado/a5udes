library(raster)
library(a5udes)
library(rgdal)
library(ggplot2)
library(dplyr)
library(tidygraph)
library(ggraph)

head(reservoir_geometry_raw)
flows_from=raster("data/flows_from.tif")

res1=as(reservoir_geometry_raw[1,],'Spatial')

flows_from_1=raster::crop(flows_from,res1)


if(length(flows_from_1)>1){
  flows_from_1_mask <- raster::mask(flows_from_1, res1)
}

flows_from_1[1,1]




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
