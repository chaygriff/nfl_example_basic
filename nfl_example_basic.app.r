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
  group_by(solo_tackle_1_player_id, solo_tackle_1_player_name) %>%
  filter(position == 'OLB' | position == 'ILB' | position == 'LB') %>%
  mutate(position = case_when(grepl("OLB", position) ~ "LB", grepl("ILB", position) ~ "LB")) %>%
  summarize(total_tackles = sum(solo_tackle)) %>%
  ungroup() %>%
  ggplot(aes(reorder(solo_tackle_1_player_name, total_tackles), total_tackles)) +
  geom_bar(stat = "identity", fill = "darkred", width = 0.5) +
  geom_text(aes(label = total_tackles), hjust = -0.5) +
  coord_flip()+
  theme_bw()+
  ggtitle("Atlanta Falcons Solo Tackles",
          subtitle = "Linebackers") +
  labs(y = "Solo Tackles") +
  labs(x = "Player Name")


