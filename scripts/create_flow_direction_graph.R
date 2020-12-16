library(raster)
library(rgdal)
library(ggplot2)
library(dplyr)
library(tidygraph)
library(ggraph)

# use flows_to.py to generate flows_to.tif and flows_from.tif

raster_flows_to=raster("data/flows_to.tif")
# save(raster_flows_to,file='data/raster_flows_to.RData')
flows_to_df=as.data.frame(raster_flows_to,xy=TRUE) %>% rename(value=flows_to)

raster_flows_from=raster("data/flows_from.tif")
# save(raster_flows_from,file='data/raster_flows_from.RData')
flows_from_df=as.data.frame(raster_flows_from,xy=TRUE) %>% rename(value=flows_from)



edges = bind_cols(dplyr::select(flows_from_df,value) %>% rename(to=value),dplyr::select(flows_to_df,value) %>% rename(from=value)) %>%
  filter(from>0)

nodes=tibble(value=union(edges$from,edges$to)) %>% left_join(flows_to_df)

flow_direction_tidygraph=tbl_graph(nodes=nodes, edges = edges, directed = TRUE)

save(flow_direction_tidygraph,file='data/flow_direction_tidygraph.RData')
