library(tidyverse)

########load file
dfp = read.csv('irrigation_depth_raw.csv')
#
dfp$Years = factor(dfp$Years)
dfp$depth = factor(dfp$depth)

##aov_function
aov_test = function(data){
  model <- aov(value ~ depth, data = data)
  out <- agricolae::duncan.test(model, "depth")
  aa = out$groups
  aa$group = row.names(aa)
  return(aa)
}
#### Statistics

aov_year = dfp %>%
  pivot_longer(-c(1:2),values_to = 'value',names_to = 'yield_comp') %>% 
  group_by(yield_comp,Years) %>% 
  nest %>% 
  mutate(sig = map(data,~aov_test(data = .))) %>% 
  unnest(sig) %>% 
  mutate(sig = paste(round(value,1),groups,sep = '')) %>% 
  dplyr::select(Years,group,yield_comp,sig) %>% 
  pivot_wider(names_from = yield_comp,values_from = sig) %>% 
  dplyr::select(1:2,'Shoots_per',	'Inflorescences_per_shoot',	'Flowers_per_inflorescence',
                'Pods_per_inflorescence','Seeds_per_pod',	'Seed_yield')


print(aov_year)
