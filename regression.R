library(readr)
library(ggplot2)
library(dplyr)
library(a5udes)
x=read_tsv('AvsBH') %>% filter_all(all_vars(!is.na(.)))

names(x)=c('BH','Amax')



ggplot(x %>% filter(Amax<100,BH<100)) + geom_point(aes(x=Amax,y=BH)) + stat_smooth(method=lm,aes(x=Amax,y=BH))
x %>% filter(Amax<100,BH<100) %>% mutate(fr=BH/Amax) %>% pull(fr) %>% mean



ggplot(x %>% filter(Amax<100,BH<100) %>% mutate(fr=BH/Amax)) + geom_histogram(aes(x=fr))

library(tidygraph)
library(sf)
res_area=reservoir_geometry %>%
  st_set_geometry(NULL) %>%
  select(id_jrc,area_max) %>%
  rename(name=id_jrc)

reservoir_tidygraph = reservoir_tidygraph %>%
  activate(nodes) %>%
  mutate(name=as.integer(name)) %>%
  left_join(res_area)

reservoir_tidygraph %>% activate(nodes) %>% mutate(area_max=area_max/10000) %>% mutate(Abasin=area_max*0.3664)
