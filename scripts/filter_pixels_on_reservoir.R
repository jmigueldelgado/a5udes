library(a5udes)
library(sf)
library(dplyr)
library(ggraph)
library(tidygraph)

reservoir_geometry %>% head

res_id=34181
res_id=3
example_id=res_id

sf_polygon=filter(reservoir_geometry_raw,id_jrc==res_id)

flow_graph=flow_graph_in_polygon(sf_polygon)
sink_from_graph(flow_graph)



reservoir_on_graph = reservoir_geometry_raw %>% head %>% split(.,seq(nrow(.))) %>% purrr::map(function(...) {
  # acces current row and process whatever you need to do
  current = st_sf(...)

  # return
  current %>% mutate(sink=flow_graph_in_polygon(.) %>% sink_from_graph())
  }) %>%
  bind_rows()

### but is the reservoir on the river network or outside? In other words, does it
### store upstream pixels or only a part of the local pixel?

ggraph::ggraph(flow_graph,layout='fr') +
  geom_edge_link(arrow = arrow(length = unit(4, 'mm'))) +
  geom_node_label(aes(label = node_is_sink()), repel = TRUE)

ggraph::ggraph(flow_graph,layout='fr') +
  geom_edge_link(arrow = arrow(length = unit(4, 'mm'))) +
  geom_node_label(aes(label = node_topo_order()), repel = TRUE)
#
# ggraph::ggraph(flow_direction_i,layout='fr') +
#   geom_edge_link(arrow = arrow(length = unit(4, 'mm'))) +
#   geom_node_label(aes(label = node_is_root()), repel = TRUE)
#
# ggraph::ggraph(flow_direction_i,layout='fr') +
#   geom_edge_link(arrow = arrow(length = unit(4, 'mm'))) +
#   geom_node_label(aes(label = node_is_sink()), repel = TRUE)

# get outlet with node_is_sink() and store it in table for each reservoir





subgraph_i=flow_direction_tidygraph %>%
  morph(to_subgraph, value %in% raster_ids_in_reservoir$flows_from, subset_by = 'nodes') %>%
  crystallise %>%
  .$graph %>%
  .[[1]]

subgraph_i=flow_direction_tidygraph %>%
  morph(to_local_neighborhood, value %in% raster_ids_in_reservoir$flows_from, subset_by = 'nodes') %>%





nodes_inside_reservoir=flow_direction_tidygraph %>%
  # activate('edges') %>%
  tidygraph::filter_edges(to %in% raster_ids_in_reservoir$flows_from)# %>%
  # activate(nodes)

nodes_inside_reservoir

plot(nodes_inside_reservoir)

nodes_inside_reservoir

   %>%
  mutate(G=centrality_degree()) %>%
  filter(G>0)

plot(nodes_inside_reservoir)

new_root=flows_from_in_reservoir[1,1]

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
