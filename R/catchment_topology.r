#' Calculate graph object based on catchment network
#' @param catch a sf dataframe from HydroSheds.
#' @return g a igraph object
#' @importFrom magrittr `%>%`
#' @importFrom tidygraph as_tbl_graph activate
#' @importFrom sf st_set_geometry
#' @importFrom dplyr select rename left_join mutate
#' @export
catch2graph <- function(catch){
  if(class(catch)[1]=='sf'){
    catch_sf=st_set_geometry(catch,NULL)
  } else {
    catch_sf=catch
  }

  g=catch_sf %>%
    dplyr::select(HYBAS_ID,NEXT_DOWN,SUB_AREA,UP_AREA) %>%
    rename(from=HYBAS_ID,to=NEXT_DOWN) %>%
    as_tbl_graph

  attr=catch_sf %>%
    dplyr::select(HYBAS_ID,SUB_AREA,UP_AREA) %>%
    mutate(name=as.character(HYBAS_ID))

  g=g %>% activate(nodes) %>% left_join(attr)

  return(g)
}
