Topology of a reservoir network with HydroSHEDS
================
JM Delgado
2019-05-15

Global datasets of rivers and reservoirs
========================================

Rivers
------

We use HydroSHEDS as a consistent global river dataset. The resolution is about 90 m around the equator (3 arc-seconds).

> Lehner, B., Verdin, K., Jarvis, A. (2008): New global hydrography derived from spaceborne elevation data. Eos, Transactions, AGU, 89(10): 93-94.

For example in Ceará:

``` r
plot(riv["UP_CELLS"])
```

![](topology_reservoir_network_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r

# ggplot(riv) + geom_sf(aes(color=UP_CELLS))
```

Since the river network dataset of HydroSHEDS has valid topology, we can operate on it with package `igraph`, using graph theory. We can for example obtain **all disjoint components of the graph**:

``` r
riv_split = split_river_network(riv)
riv_split %>%
  filter(membership==6) %>%
  select(UP_CELLS) %>%
  plot
```

![](topology_reservoir_network_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r

split_river_network
#> function (riv) 
#> {
#>     touching_list = st_touches(riv)
#>     g = graph.adjlist(touching_list)
#>     c = components(g)
#>     riv_n = mutate(riv, membership = as.factor(c$membership)) %>% 
#>         arrange()
#>     return(riv_n)
#> }
#> <bytecode: 0x55f669615920>
#> <environment: namespace:assimReservoirs>
```

Selecting a reach somewhere within the Jaguaribe river basin:

``` r
reach_id=140877
riv_jagua = select_disjoint_river(reach_id,riv_split)
plot(riv_i["UP_CELLS"])
```

![](topology_reservoir_network_files/figure-markdown_github/unnamed-chunk-3-1.png)

Reservoirs
----------

We use the JRC global surface water dataset.

> Jean-Francois Pekel, Andrew Cottam, Noel Gorelick, Alan S. Belward, High-resolution mapping of global surface water and its long-term changes. Nature 540, 418-422 (2016). (<doi:10.1038/nature20584>)

Resolution is 30 m. We run `allocate_reservoir_to_river(riv)`, `Routing()` and `Routing_non_strat()` in advance in order to create a graph. Then we can obtain any

``` r

id='34487'

neighbors(reservoir_graph,id,'out')
#> + 1/21461 vertex, named, from 6ff78c6:
#> [1] 37380
neighbors(reservoir_graph,id,'in')
#> + 7/21461 vertices, named, from 6ff78c6:
#> [1] 34309 34481 34336 34509 34422 34512 34275
sub=all_simple_paths(reservoir_graph,from=id,mode='in') %>%
  unlist %>%
  unique

Vsubgraph = induced_subgraph(reservoir_graph, sub ,impl='auto')

plot(Vsubgraph)
```

![](topology_reservoir_network_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r

vertices = V(Vsubgraph)  

filter(res_max,res_max$id_jrc %in% vertices$name) %>%
  ggplot(.) + geom_sf()+geom_sf_label(aes(label=id_jrc),nudge_x = 500, nudge_y =- 500)
```

![](topology_reservoir_network_files/figure-markdown_github/unnamed-chunk-4-2.png)
