#Load packages
library(tidyverse)
library(ggtext)
library(glue)
theme_set(theme_minimal())
#Load data
astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

#Data wrangling
nationality_mix <- astronauts %>% group_by(mission_title,year_of_mission) %>% 
  summarize(n_nationalities = n_distinct(nationality),
    nationalities = paste0(unique(nationality),collapse = " | ")) %>% 
  ungroup() %>% 
  arrange(year_of_mission,nationalities) %>% 
  group_by(year_of_mission) %>% 
  mutate(y = 1:n()) %>% 
  mutate( flag_icon = case_when(
      n_nationalities == 1 & nationalities == "U.S.S.R/Russia" ~ "icons/russia.png",
      n_nationalities == 1 & nationalities == "U.S." ~ "icons/us.png",
      n_nationalities > 1 & str_detect(nationalities, "U.S.S.R/Russia") & !str_detect(nationalities,"U\\.S\\.($| \\|)") ~ "icons/russia_other.png",
      n_nationalities > 1 & str_detect(nationalities, "U\\.S\\.($| \\|)") & !str_detect(nationalities,"U.S.S.R/Russia") ~ "icons/us_other.png",
      n_nationalities > 1 & str_detect(nationalities, "U\\.S\\.($| \\|)") & str_detect(nationalities,"U.S.S.R/Russia") ~ "icons/russia_us.png", 
      TRUE ~ "icons/other.png" 
    ),
    flag_html = glue("<img src='{flag_icon}'width='9'/>"))
#Visualization
arrows <- tribble(~x1,~x2,~y1,~y2,~curvature,
  1978.2,1977,1.5,15,0.1,
  1995.5,2000,13.5,20,-0.3)

ggplot(nationality_mix,aes(x = year_of_mission,y = y)) +
  geom_richtext( aes(label = flag_html),fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt")) +
  geom_curve(data = arrows, mapping = aes(x = x1,y = y1,xend = x2,yend = y2),
    arrow = arrow(length = unit(0.07, "inch"),ends = "first"), 
    size = 0.4,,curvature = 0.3) +
  annotate("text",x = 1978,y = 15,label = "Vladimir Remex is the first cosmonaut\n from a country other than the Soviet Union\n or the United States",size = 2.5,vjust = 0,hjust = 1) +
  annotate("text",x = 2000,y = 20,label = "Norman Thagard, first American to ride to space\n on board a Russian vehicle, in 1995.\nFor Mir-18 mission.",size = 2.5,vjust = 0.5,hjust = 0) +
  labs(title = "Sharing rides: Astronaut edition",
    subtitle = "Analyzing the nationality of astronauts for space launches.\nBlack color represents other countries rather than U.S. & U.S.S.R/Russia.\nHalf flags represent multiple nationalities in a mission",
    x = "Mission year",
    y = "",
    caption = "@perspectivalean\n#TidyTuesday\nData:Space launches[The Economist]") +
  theme( panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(),
    axis.text.y = element_blank())

ggsave("output/astronauts_github.png",plot = last_plot(),width = 31.75/1.5,height = 20/1.5,units = c("cm"),dpi = 1200)
