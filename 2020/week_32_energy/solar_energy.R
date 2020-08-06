#Load packages 
library(tidyverse)
library("rnaturalearthdata")
library("rnaturalearth")


#Load data
energy_types <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv')
country_totals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/country_totals.csv')
centroids <- read_csv("data/country_centroids_az8.csv") %>% select(iso_a2,Longitude,Latitude)
world <- ne_countries(scale = "medium", returnclass = "sf")

#Analysis
total_country <- energy_types %>% 
  filter(level == "Level 1") %>% 
  group_by(country) %>% 
  summarize(total_avg = (sum(`2016`) + sum(`2017`) + sum(`2018`))/3)

solar <- energy_types %>% 
  filter(type == "Solar") %>% 
  left_join(total_country, by = c("country")) %>% 
  mutate(avg_solar = (`2016` + `2017` + `2018`)/3,
    country = case_when(country == "UK" ~ "GB",
      country == "EL" ~ "GR",
      TRUE ~ country),
    vjust_value = case_when(country %in% c("UA","SI","BA","AT","HR") ~ 0,
      TRUE ~ 1)) %>% 
  left_join(centroids,by = c("country" = "iso_a2")) %>% 
  mutate(pct_solar = avg_solar/total_avg)

#plot
ggplot() +
  geom_sf(data = world %>% filter(iso_a2 %in% solar$country),fill = "#eff7fa") +
  coord_sf(xlim = c(-10, 80), ylim = c(30,70)) +
  geom_point(data = solar,aes(x = Longitude, y = Latitude ,color = pct_solar)) +
  #Percentage doubled to get more width in plot and see the labels better.
  geom_segment(data = solar, aes(x = Longitude,y = Latitude,yend = Latitude,xend = 50 + pct_solar*200),alpha = 0.2,linetype = "dashed",color = "#606060") +
  geom_segment(data = solar, aes(x = 50,y = Latitude,yend = Latitude,xend = 50 + pct_solar*200),color = "#222222") +
  geom_text(data = solar %>% filter(pct_solar > 0),aes(x = 50 + pct_solar*200,y = Latitude,label = country,vjust = vjust_value),hjust = 0,size = 2.5,color = "#606060") +
  annotate(geom = "text", x = 5, y = 32, label = "*Average percentage of solar energy (2017-19)\n Latitude based on country centroids\nCountries with 0% don't have a bar",size = 2,color = "#606060") +
  scale_x_continuous(breaks = c(50, 60,70),
    labels = c("0% \nSolar energy*", "5%","10%")) +
  scale_y_continuous( breaks = c(30),
    labels = c("")) +
  scale_color_gradient2(low = "#00222e",high = "#feb24c") +
  guides(color = FALSE) +
  labs(x = "",y = "",
    title = "Sun is up?",
    caption = "@perspectivalean\n#TidyTuesday\nData:Eurostat") +
  theme_minimal() + 
  theme(plot.background = element_rect(fill = "#aadfed",color = "#aadfed"),
    panel.background = element_rect(fill = "#aadfed",color =  "#aadfed"))
ggsave("output/europe_energy_github.png",width = 22,height = 17.5,units = c("cm"),dpi = 300)


