library(readr)
library(ggplot2)
library(dplyr)
x=read_tsv('AvsBH') %>% filter_all(all_vars(!is.na(.)))

names(x)=c('BH','Amax')



ggplot(x %>% filter(Amax<100,BH<100)) + geom_point(aes(x=Amax,y=BH)) + stat_smooth(method=lm,aes(x=Amax,y=BH))
x %>% filter(Amax<100,BH<100) %>% mutate(fr=BH/Amax) %>% pull(fr) %>% mean



ggplot(x %>% filter(Amax<100,BH<100) %>% mutate(fr=BH/Amax)) + geom_histogram(aes(x=fr))
