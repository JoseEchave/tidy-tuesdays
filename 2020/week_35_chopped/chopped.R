#Load packages
library(tidyverse)
library(jechaveR)
library(tidytext)
library(ggraph)
library(tidygraph)
library(ggtext)

#Load data
chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')

#Functions

select_cols_df <- function(col_name){
  
  chopped %>% 
    mutate(plate = col_name) %>% 
    select(ingredients = col_name,plate,series_episode) 
}

extract_with_what <- function(bigram){
  text_df %>% 
    separate_rows(ingredients, sep = ", ") %>% 
    filter(str_detect(ingredients,bigram)) %>% 
    mutate(with_what = str_extract(ingredients,glue::glue(".+((?= {bigram}))"))) %>% 
    filter(!is.na(with_what)) %>% 
    pull(with_what) %>% 
    unique() 
}

#Analysis
text_df <- rbind(select_cols_df("appetizer"),select_cols_df("entree"),select_cols_df("dessert"))

bigram <- text_df %>% 
  mutate(plate = as_factor(plate),text = ingredients,series_episode) %>% 
  unnest_tokens(bigram,text,token = "ngrams",n = 2) 

unique_episodes <- NROW(unique(chopped$series_episode))

bigram_pct <- bigram %>%
  group_by(bigram) %>% 
  summarize(pct = n_distinct(series_episode)/unique_episodes) %>% 
  arrange(desc(pct)) %>% 
  head(1000) %>% 
  mutate(with = map(bigram,extract_with_what),
    with_n = map_int(with,NROW))

bigram_unnest <- bigram_pct %>% 
  unnest(with)

graph_df <- bigram_unnest %>% 
  filter(with %in% c("red","black","white","blue","green")) %>% 
  group_by(with,bigram) %>% 
  summarize(n = n()) %>% 
  as_tbl_graph()

#Final plot

ggraph(graph_df,layout = "gem") + 
  geom_edge_link(aes(alpha = n,
    start_cap = label_rect(node1.name),
    end_cap = label_rect(node2.name),
    color = node1.name),
    show.legend = FALSE,
    arrow = arrow(length = unit(2, 'mm')),
    width = 0.5) + 
  geom_edge_loop(colour = "#cccccc") +
  geom_node_text(aes(label = name),
    show.legend = FALSE,size = 2.5) +
  scale_edge_colour_manual(values = c("#222222", #Black
    "#2585ca", #Blue
    "#20bc40", #Green
    "#d91d1b", #Red
    "#FFFFFF")) + #White
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#cccccc",color = "#cccccc"),
    panel.background = element_rect(fill = "#cccccc",color =  "#cccccc"),
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()) + 
  labs(title = "Ingredients are <span style='color:#222222'>c</span><span style='color:#2585ca'>o</span><span style='color:#20bc40'>l</span><span style='color:#d91d1b'>o</span><span style='color:#FFFFFF'>r</span>ful",
    subtitle = "Selected bigrams from ingredients used in TV show Chopped preceded by a color",
    caption = "@assesslife",
    x = "",
    y = "")

ggsave("output/chopped_ingredients_github.png",plot = last_plot(),width = 31.75/1.5,height = 17.85/1.5,units = c("cm"),dpi = 1200)
