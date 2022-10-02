library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggtheme)
library(ggExtras)
library(nflreadr)

# Load data for the 2021 and 2022 season
pbp <- load_pbp(2021:2022)

# Load depth chart data for 2022 season
depth <- nflreadr::load_depth_charts(2022)

# Refine pbp data to display solo tackle totals for the Atlanta Falcons

solo_tackle_atl <- pbp %>% filter(solo_tackle_1_team == 'ATL') %>%
  select(solo_tackle_1_player_id, solo_tackle_1_player_name, solo_tackle)
 
solo_tackle_atl$club_code <- 'ATL' 
  
depth <- depth %>%
  mutate(solo_tackle_player_id = gsis_id)
  
# Combine depth and solo_tackle_atl into same dataframe

depth_tackles <- left_join(solo_tackle_atl, depth, by = "solo_tackle_player_id")

# Generate bar plot

depth_tackles %>%
  group_by(position, first_name, last_name, depth_position, depth_team, solo_tackle_player_1_name) %>%
  summarise(freq = sum(solo_tackle)) %>%
  filter(position == 'OLB' | position == 'LB') %>%
  arrange(depth_team) %>%
  rename(solo_tackles = freq) %>%
  ggplot(aes(reorder(solo_tackle_1_player_name, solo_tackles), solo_tackles) +
    geom_bar(stat = "identity", fill = "red") +
    coord_flip() +
    geom_text(aes(label = depth_team)) +
    facet_wrap(~depth_tackles$position)
                      
