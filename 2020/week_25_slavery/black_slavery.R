#Load libraries
library(tidyverse)
theme_set(theme_minimal())

#Load data
slave_routes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv')

#Data preparation
  #Get min/max year to complete years without transport
slave_routes %>% summarize(min(year_arrival),max(year_arrival))
  #Transform data
slave_routes_cum <- slave_routes %>% 
  select(year_arrival,n_slaves_arrived) %>% 
  complete(year_arrival = 1514:1866,fill = list(n_slaves_arrived = 0)) %>% 
  group_by(year_arrival) %>% 
  summarize(sum_slaves = sum(n_slaves_arrived,na.rm = TRUE)) %>% 
  mutate(cumsum_slaves = cumsum(sum_slaves)) %>% 
  arrange(year_arrival)

#Events to be shown in graph
event_highlights <- tribble(~year,~happening,~vjust,
  1501,"The Spanish king allows enslaved Africans\n into Spain's American colonies.",0.5,
  1619,"First Africans in the English North American colonies,\n purchased from a Duch slaver",0.5,
  1663,"In South Carolina every new white settler is granted acres\nfor each black slave he or she brings into the colony",0.5,
  1865,"Thirteenth Amendment in 1865, finally abolished slavery throughout USA.",1.6)

#Visualization
ggplot(slave_routes_cum,aes(x = year_arrival,y = cumsum_slaves)) +
  geom_col(width = 1,alpha = 0.75,color = "#6d6d6d" ) +
  geom_linerange(aes(ymax = cumsum_slaves,ymin = cumsum_slaves - sum_slaves ),color = "red") +
  geom_vline(data = event_highlights,aes(xintercept = year),linetype = "dashed",color = "#222222",alpha = 0.5) +
  geom_text(data = event_highlights,aes(x = year,y = 5e+06,label = happening,vjust = vjust),
    angle = 90,size = 2, hjust = 1,color = "#222222") +
  scale_y_continuous(breaks = c(0,1e+06,5e+06),labels = c("","1 million\n slaves","5 million\n slaves")) +
  labs(title = "Enslaved Africans",
    subtitle = "Cumulative number of slaves transported in voyages.<br>
    Partly missing data, many died in the transport.<br>
     <span style='color:#d91d1b'> Red </span>: number of new slaves transported each year ",
    x = "Arrival year", 
    y = "",
    caption = "#TidyTuesday\n@perspectivalean") +
  theme(plot.subtitle = ggtext::element_markdown(size = 10, hjust = 0.5, vjust = 1, color = "#6d6d6d",margin = ggplot2::margin(0,10,10,0)))


ggsave("output/slaves.jpg",dpi = 600,width = 31.75/1.5,height = 17.85/1.5,units = c("cm"))
