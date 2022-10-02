library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(nflreadr)
library(ggplot2)

# Load data for the 2021 and 2022 season
pbp <- load_pbp(2021:2022)

# Load depth chart data for 2022 season and create new column
depth <- nflreadr::load_depth_charts(2022)

depth <- depth %>%
  mutate(solo_tackle_1_player_id = gsis_id)

# Refine pbp data to display solo tackle totals for the Atlanta Falcons

solo_tackle_atl <- pbp %>% filter(solo_tackle_1_team == 'ATL') %>%
  select(solo_tackle_1_player_id, solo_tackle_1_player_name, solo_tackle)

solo_tackle_atl$club_code <- 'ATL' 

# Combine depth and solo_tackle_atl into same dataframe

depth_tackles <- left_join(solo_tackle_atl, depth, by = "solo_tackle_1_player_id")

# Generate bar plot to display total solo tackles for players at the Linebacker position

depth_tackles %>%
  group_by(position, depth_position, depth_team, solo_tackle_1_player_name) %>%
  filter(position == 'OLB' | position == 'LB') %>%
  summarise(freq = sum(solo_tackle)) %>%
  arrange(depth_team) %>%
  rename(solo_tackles = freq)%>%
  ggplot(aes(reorder(solo_tackle_1_player_name, solo_tackles), solo_tackles)) +
  geom_bar(stat = "identity", fill = "darkred", width = 0.5) +
  geom_text(aes(label = depth_team)) +
  coord_flip()+
  theme_bw()+
  ggtitle("Atlanta Falcons Solo Tackles",
          subtitle = "Linebackers") +
  labs(y = "Solo Tackles") +
  labs(x = "Player Name")

# What we observe is that player T.Andersen has a value of 1 at two different 
# linebacker positions which throws off the appearance of the chart.
