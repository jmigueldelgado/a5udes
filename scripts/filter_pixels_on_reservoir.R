library(a5udes)



res_id=34181
example_id=res_id

outlet=outlet_in_condem(res_id)

outlet

raster_i=paste0('data/raster_',example_id,'.tif')
raster_to_i=paste0('data/raster_to_',res_id,'.tif')
poly_i=paste0('data/poly_',res_id,'.gpkg')


try(file.remove(raster_to_i))
try(file.remove(raster_i))
try(file.remove(poly_i))
try(file.remove(paste0(strsplit(raster_to_i,'[.]')[[1]][1],'_all_touched.tif')))
try(file.remove(paste0(strsplit(raster_i,'[.]')[[1]][1],'_all_touched.tif')))


# call gdalwarp -of GTiff -cutline /home/delgado/proj/buhayra/buhayra/auxdata/wm_utm_demo.gpkg -cl wm_utm_demo -crop_to_cutline -wo CUTLINE_ALL_TOUCHED=TRUE /home/delgado/proj/a5udes/data/flows_from.tif /tmp/processing_8df776f0abd34f0781b0ac296178c1fe/c9e05e71e65d4d9591abe83ac4a1f273/OUTPUT.tif
# with flag -wo CUTLINE_ALL_TOUCHED=TRUE

gdalUtils::gdalwarp(srcfile=raster_i,
  dstfile=paste0(strsplit(raster_i,'[.]')[[1]][1],'_all_touched.tif'),
  cutline=poly_i,
  cl=strsplit(poly_i,'[/.]')[[1]][2],
  crop_to_cutline=TRUE,
  wo=list('CUTLINE_ALL_TOUCHED=TRUE'),
  verbose=TRUE)

gdalUtils::gdalwarp(srcfile=raster_to_i,
  dstfile=paste0(strsplit(raster_to_i,'[.]')[[1]][1],'_all_touched.tif'),
  cutline=poly_i,
  cl=strsplit(poly_i,'[/.]')[[1]][2],
  crop_to_cutline=TRUE,
  wo=list('CUTLINE_ALL_TOUCHED=TRUE'),
  verbose=TRUE)

pixels_from=raster::raster(paste0(strsplit(raster_i,'[.]')[[1]][1],'_all_touched.tif')) %>%
  as.data.frame %>% rename(node=1) %>%
  filter(!is.na(node))

pixels_to=raster::raster(paste0(strsplit(raster_to_i,'[.]')[[1]][1],'_all_touched.tif')) %>%
  as.data.frame %>% rename(node=1) %>%
  filter(!is.na(node))
nodes=tibble(name=pixels_from$node)

match_from=match(pixels_from$node,nodes$name)
match_to=match(pixels_to$node,nodes$name)


edges = tibble(from=match_from[!is.na(match_to)],to=match_to[!is.na(match_to)])

flow_direction_i=tbl_graph(nodes=nodes, edges = edges, directed = TRUE,node_key='name')


# ggraph::ggraph(flow_direction_i,layout='fr') +
#   geom_edge_link(arrow = arrow(length = unit(4, 'mm'))) +
#   geom_node_label(aes(label = node_is_isolated()), repel = TRUE)
#
# ggraph::ggraph(flow_direction_i,layout='fr') +
#   geom_edge_link(arrow = arrow(length = unit(4, 'mm'))) +
#   geom_node_label(aes(label = node_is_root()), repel = TRUE)
#
# ggraph::ggraph(flow_direction_i,layout='fr') +
#   geom_edge_link(arrow = arrow(length = unit(4, 'mm'))) +
#   geom_node_label(aes(label = node_is_sink()), repel = TRUE)

# get outlet with node_is_sink() and store it in table for each reservoir

outlet=flow_direction_i %>%
  mutate(sink=node_is_sink()) %>%
  activate(nodes) %>%
  as_tibble %>%
  filter(sink) %>%
  pull(name)

try(file.remove(raster_to_i))
try(file.remove(poly_i))
try(file.remove(paste0(strsplit(raster_to_i,'[.]')[[1]][1],'_all_touched.tif')))


########################





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
