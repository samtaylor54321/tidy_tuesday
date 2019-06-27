library(tidyverse)
library(rsample)

bootstraps <- datasets::iris %>% 
  bootstraps(times=100) %>% 
  unnest(map(splits, as.data.frame)) %>% 
  group_by(Species, id) %>% 
  summarize_at(vars(Sepal.Length:Petal.Width), mean) %>% 
  summarise(Sepal.Length_low_boot = quantile(Sepal.Length, probs=0.025),
            Sepal.Length_high_boot = quantile(Sepal.Length, probs=0.975),
            Sepal.Width_low_boot = quantile(Sepal.Width, probs=0.025),
            Sepal.Width_high_boot = quantile(Sepal.Width, probs=0.975),
            Petal.Length_low_boot = quantile(Petal.Length, probs=0.025),
            Petal.Length_high_boot = quantile(Petal.Length, probs=0.975),
            Petal.Width_low_boot = quantile(Petal.Width, probs=0.025),
            Petal.Width_high_boot = quantile(Petal.Width, probs=0.975)
            ) %>% 
  gather(key, value, -Species) %>% 
  separate(key,c('key','boot'), sep='_') %>% 
  spread(boot, value)
  
iris %>% 
  group_by(Species) %>% 
  summarise_at(vars(Sepal.Length:Petal.Width), mean) %>% 
  gather(key, value, -Species) %>% 
  left_join(bootstraps, by=c('Species','key')) %>% 
  ggplot(aes(x=str_to_title(Species), y=value)) + geom_point(aes(color=Species), show.legend = F) + coord_flip() +
  facet_wrap(~key) + 
  geom_errorbar(aes(ymin=low,ymax=high)) +
  labs(x="",
       title ="Iris plots with bootstrapped error bars",
       y="Measurement Value")






"Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width" 