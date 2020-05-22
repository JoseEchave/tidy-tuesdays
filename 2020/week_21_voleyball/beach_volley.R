
library(tidyverse)

vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)

#Get winners
team_w <- vb_matches %>% 
  select(player_1 = w_player1,player_2 =w_player2,rank = w_rank,duration,date,gender,circuit,tournament,match_num) %>% 
  mutate(result = "win") 
#Get losers
team_l <- vb_matches %>% 
  select(player_1 = l_player1,player_2 = l_player2,rank = l_rank,duration,date,gender,circuit,tournament,match_num) %>% 
  mutate(result = "lose") 

#Combine teams
teams_long <- rbind(team_w,team_l)

#Get top 10 pairs
top_10_teams_games_played <- teams_long %>% 
  mutate(team = paste0(player_1," & ",player_2)) %>% 
  group_by(team) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  mutate(num = (1:n()))


#Data for ggplot
teams_long_win_lose <- teams_long %>% 
  mutate(team = paste0(player_1," & ",player_2)) %>% 
  arrange(team,date,match_num) %>% 
  group_by(team) %>% 
  mutate(team_game_num = 1:n()) %>% 
  inner_join(top_10_teams_games_played,by = c("team" = "team"))

#Max strike
teams_long_win_lose %>% 
  mutate(count = ifelse(result == "win" & lead(team,1) == team,1,0),
    strike = ave(count,cumsum(count == 0),FUN = cumsum)) %>% 
  ungroup() %>% 
  filter(strike == max(strike,na.rm = TRUE))

arrows <- tibble(x1 = 677,x2 = 750,y1 = 1.1, y2 = 3)

#Plot
ggplot(teams_long_win_lose, aes(x = team_game_num,y = num,color = result)) +
  scale_y_continuous(trans = "reverse") +
  scale_x_continuous(breaks = c(0,250,500,750)) +
  geom_point(size = 2,alpha = 0.75) +
  geom_text(data = ~ .x %>% filter(team_game_num == 1),aes(label = team),
   size = 2.5,hjust = 0,vjust = -0.75) +
  geom_curve(data = arrows, mapping = aes(x = x1,y = y1,xend = x2,yend = y2),
    arrow = arrow(length = unit(0.07, "inch"),ends = "first"), 
    size = 0.4, color = "#00222e",curvature = -0.3,alpha = 0.7) +
  geom_text(aes(x = 750, y = 3, label = "Record winning strike \n 122 consecutive wins"),
    size = 2.5,hjust = 0.4,vjust = 1) +
  labs(x = "Games played (in chronological order)",
    y = "",
    color = "",
    title = "Beach Volleyball winning strike",
    subtitle = "Top 10 teams who played most games together",
    caption = "#TidyTuesday \n@perspectivalean") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank())

#Output
ggsave("output/voleyball_gh.jpg",dpi = 600,width = 31.75/1.5,height = 17.85/1.5,units = c("cm"))
