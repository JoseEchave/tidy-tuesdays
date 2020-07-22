
library(jechaveR)
library(tidyverse)
library(ggtext)
library(glue)
theme_set(theme_minimal())


#Load data

animal_outcomes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_outcomes.csv')



#Analysis

rehomed_animals <- animal_outcomes %>% 
  filter(outcome == "Rehomed") %>% 
  mutate(animal_type_fct = case_when(animal_type == "Cats"~ "Cats",
    animal_type == "Dogs" ~ "Dogs",
    TRUE ~ "Other Animals")) %>% 
  group_by(animal_type_fct,year) %>% 
  summarize(total_fct = sum(Total,na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(animal_type_fct) %>% 
  arrange(animal_type_fct,year) %>% 
  mutate(cumulative_rehomed = cumsum(total_fct),
    icon = case_when(animal_type_fct == "Cats"~ "cat.png",
      animal_type_fct == "Dogs" ~ "dog.png",
      TRUE ~ ""),
    icon_html = glue::glue("<img src='{icon}'width='15'/>"))




#Final plot

ggplot(rehomed_animals %>% filter(animal_type_fct %in% c("Cats","Dogs")),aes(x = year, y = cumulative_rehomed)) +
  geom_richtext( aes(label = icon_html),fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt")) +
  scale_x_continuous(breaks = c(2000,2010,2018)) +
  scale_y_continuous(breaks = c(0,200000,400000),labels = c("0","200 000\n animals rehomed","400 000\n animals rehomed")) +
  labs(x = "Year",y = "",
    title = "Life saving australians",
    subtitle = "Cumulative dogs and cats rehomed by RSPCA.\nRSPCA helps rehoming neglected and unwanted animals\n through their shelters",
    caption = "@perspectivalean\n#TidyTuesday")

ggsave("output/animals_aus.png",plot = last_plot(),width = 30/1.5,height = 17.5/1.5,units = c("cm"),dpi = 1200)



