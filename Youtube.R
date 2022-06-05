#### Load in Data ####

# youtube <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')
# write.csv(youtube, "youtube.csv")

library(tidyverse)
library(corrplot)

read.csv(file = "youtube.csv")->youtube

#### Create Theme ####

# general theme
theme_set(theme_void(base_family = "Verdana"))

theme_update(
  axis.text.x = element_text(color = "black", face = "bold", size = 26, 
                             margin = margin(t = 6)),
  axis.text.y = element_text(color = "black", size = 22, hjust = 1, 
                             margin = margin(r = 6), family = "Verdana"),
  axis.line.x = element_line(color = "black", size = 1),
  panel.grid.major.y = element_line(color = "grey90", size = .6),
  plot.background = element_rect(fill = "white", color = "white"),
  plot.margin = margin(rep(20, 4))
)


## theme for horizontal charts
theme_flip <-
  theme(
    axis.text.x = element_text(face = "plain", family = "Verdana", size = 22),
    axis.text.y = element_text(face = "bold", family = "Verdana", size = 26),
    panel.grid.major.x = element_line(color = "grey90", size = .6),
    panel.grid.major.y = element_blank(),
    legend.position = "top", 
    legend.text = element_text(family = "Verdana", size = 18),
    legend.title = element_text(face = "bold", size = 18, margin = margin(b = 25))
  )

## custom colors
my_pal <- rcartocolor::carto_pal(n = 8, name = "Bold")[c(1, 3, 7, 2,4,6,8,9,5)]

#### Wrangling + Visualization ####
## Quick Overview:
youtube %>% 
  skimr::skim()

## Like to view ration
youtube %>% filter(year == 2020) %>% 
  select(brand, title, where(is.numeric)) %>% 
  na.omit() %>% 
  group_by(brand,title) %>% 
  summarise(l_to_v_ratio =like_count/view_count) %>% 
  mutate(label=scales::percent(l_to_v_ratio,accuracy = 0.01))-> tt

## GGplot of performance by brand
tt %>% 
  ggplot(aes(reorder(brand,-l_to_v_ratio), l_to_v_ratio,fill=brand))+
  geom_col(width=.8)+
  scale_y_continuous(expand =c(0,0),label=scales::percent)+
  scale_fill_manual(values = my_pal, guide="none")+
  theme(axis.text.x = element_text(angle=45,vjust=1,hjust=1))+
  ylab("Like to Views Ratio")+
  xlab("")


## Correlation plot
youtube %>% 
  select(where(is.numeric),-X,-category_id,-favorite_count) %>% 
  na.omit() %>% 
  cor() -> res

corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45) -> p




#### 