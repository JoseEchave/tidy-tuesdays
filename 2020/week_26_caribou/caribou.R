#Load packages 

library(jechaveR)
library(tidyverse)
library(geosphere)

#read data
locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv')

#Functions
##Calculate orthodromic distance (shortest distance between two points)
distance_points <- function(long1,lat1,long2,lat2){
  distm(c(long1,lat1), c(long2,lat2),fun = distHaversine) %>% 
    as.numeric()
}

#Data transformation
##Calculate activity
locations_calc <- locations %>% 
  group_by(animal_id) %>% 
  arrange(animal_id,timestamp) %>% 
  mutate(next_longitude = lead(longitude,1), 
    next_latitude = lead(latitude,1),
    dif_time = as.numeric(lead(timestamp,1) - timestamp)) %>% 
  filter(!is.na(next_longitude)) %>% 
  #Calculate distance point between coordinate and next coordinate
  mutate(distance = pmap_dbl(list(longitude,latitude,next_longitude,next_latitude),distance_points),
    cum_time = cumsum(dif_time),
    cum_distance = cumsum(distance),
    #Normalize time and distance
    perc_time = cum_time/max(cum_time,na.rm = TRUE),
    perc_distance = cum_distance/max(cum_distance,na.rm = TRUE))

##See the most/less active animal with over 1500 positions
highlight_distance <-  locations_calc %>% 
  mutate(speed = distance/dif_time) %>% 
  group_by(animal_id,season) %>% 
  summarize(total_distance = sum(distance),
    observations = n()) %>% 
  arrange(desc(total_distance)) %>% 
  filter(observations > 1500)

##Choose animals to highlight in plot
max_min_movement <- c("NA_car133","QU_car163")

##Movement summary
locations_calc %>% filter(animal_id %in% max_min_movement) %>% 
  group_by(animal_id) %>% 
  summarize(min(timestamp),max(timestamp),max(cum_time),min(cum_time),max(cum_distance),min(cum_distance))

#Plot
##Text df
labels_df <- tribble(~lon,~lat,~label,
  -121.7,54.97,"QU_car163:Among most still",
  -120.5,54.5,"NA_car133:\nAmong most active Caribou\nMoved 3x more than QU_car163"
)
##Viz
ggplot(locations_calc %>% filter(animal_id %in% max_min_movement)) +
  # geom_sf(data = canada) +
  #coord_sf(xlim = c(-130,-120),ylim = c(52.5,57.5)) +
  coord_fixed() +
  geom_path(aes(x = longitude, y = latitude,group = animal_id,color = perc_time)) +
  scale_color_jechave(palette = "sequential",discrete = FALSE) +
  geom_text(data = labels_df,
    aes(x = lon,y = lat, label = label),
    color = "#f4f4f4",vjust = 0,hjust = 0,size = 2) +
  labs(title = "Lazy or efficient caribou?",
    subtitle = "Extreme movement patterns.\nOnly caribous with over 1500 positions taken\nColor represents time, lighter color indicates end of tracking",
    x = "",
    y = "",
    caption = "#Tidytuesday\n@perspectivalean") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#222222",color = "#222222"),
    legend.background = element_rect(color = NA, fill = "#222222"),  
    legend.key = element_rect(color = "white",  fill = "#222222"), 
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    plot.background = element_rect(color = "#222222", fill = "#222222"),
    plot.title = ggtext::element_markdown(size = 20, hjust = 0.5,color = "#f4f4f4", face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, vjust = 1, color = "#f4f4f4",margin = ggplot2::margin(0,10,10,0)),
    strip.text = element_text(size = 10, face = "bold"),
    plot.caption = element_text(size = 7.5, hjust = 1, color = "#f4f4f4"),
    axis.title = element_text(size = 12,color = "#f4f4f4",face = "bold"),
    legend.position = "none",
    panel.border = element_blank(), 
    axis.line = element_blank(),
    axis.text = element_blank())

ggsave("output/caribou_movements.png",dpi = 900,width = 31.75/1.5,height = 17.85/1.5,units = c("cm"),bg = "#222222")
