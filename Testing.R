## Testing:

library(gapminder)

population_data <- gapminder %>% 
  filter(continent == 'Americas') %>%
  mutate(country = as.character(country),
         year = paste0("'", str_sub(year, 3, 4))) %>% 
  select(country, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp) 


youtube %>% 
  select(brand,year,view_count) %>%
  group_by(brand,year) %>% 
  summarise(Views=sum(view_count,na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(year = paste0("'", str_sub(year, 3, 4))) %>% 
  pivot_wider(names_from = year,values_from = Views)->heatmap


library(patchwork)

youtube %>% 
  ggplot(aes(like_count,view_count, color=brand))+
  geom_jitter(na.rm = TRUE)+
  geom_smooth(na.rm = TRUE, se=FALSE)+
  scale_y_continuous(labels=scales::number_format(scale = 100))+
  scale_x_continuous(labels=scales::number_format(scale = 100))+
  scale_color_manual(values = my_pal)+
  theme_minimal() +
  theme(legend.position = "none")+
  ylab("View Count")+
  xlab("Like Count")->p1

youtube %>% 
  ggplot(aes(like_count,view_count, color=brand))+
  geom_point(na.rm = TRUE)+
  geom_smooth(na.rm = TRUE, se=FALSE)+
  scale_y_log10(labels=scales::number_format(scale=1000))+
  scale_x_log10(labels=scales::number_format(scale=1000))+
  scale_color_manual(values = my_pal)+
  theme_minimal() +
  theme(legend.title = element_blank())+
  ylab("") +
  xlab("Like Count") -> p2

p1+p2
