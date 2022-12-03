library(ggmosaic)

predictors_2  |> 
  ggplot(aes(x = habitat, fill = extinct)) + 
  geom_bar(position = 'stack') + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 



predictors_2  |> 
  ggplot(aes(x = threat, fill = extinct)) + 
  geom_bar(position = 'stack') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


predictors_2  |> 
  ggplot(aes(x = use, fill = extinct)) + 
  geom_bar(position = 'stack') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


i <- predictors_2 |> 
  mutate(endemic = case_when(endemic == 'Yes' ~ 'Endemic',
                             endemic == 'No' ~ 'Not Endemic'),
         extinct = case_when(extinct == 1 ~ 'Extinct',
                             extinct == 0 ~ 'Extant')) |> 
         ggplot() +
  geom_mosaic(aes(x = product(endemic), fill = extinct)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(x = 'Endemism',
       y = 'Extinct',
       title = 'Extinction by Endemism') + 
  theme_minimal() 
i
ggsave('extendmosaic.png', path = here('figs/'), bg = 'white')
  

