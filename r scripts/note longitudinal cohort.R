### note: Feb to april longitudinal 

library(tidyverse)

linked_path <- list.files('steam data', full.names = T)

linked_path <- 
  linked_path[linked_path %>% grepl(pattern = '03_01')]



analysis_df <-
  linked_path %>% 
  map_dfr(
    .f = function(x) read.csv(x, colClasses = 'character')
  )

### cleaning + removing duplicated 

analysis_df <-
  analysis_df %>%
  mutate(
    rank = rank %>% as.numeric(),
    games = games %>% as.numeric(),
    sample_weights = sample_weights %>% as.numeric(),
    playtime_forever = playtime_forever %>% as.numeric(),
    game = ifelse(appid == '1778820', 'T8', 'T7')
  ) %>%
  filter(playtime_forever > 0) %>% ## filter 0 hours players -- steam privacy 
  group_by(steamid, appid) %>%
  summarise_all(
    .f = function(x) x[1] #take the first record to elimiante duplicates 
  ) 

### We have 2898 entries 

### Load in april data 

april_df <-
  'csv_06_4/replays.csv' %>%
  read.csv()

april_df %>% names()
## omit things that aren't dataframe
## R turns it into a list of things 
## We can convert to a huge data table 
## polarisid = ingame tekken 8 id?
## playername is displayed player name 
## OnlineId is steam id:
## Example: 76561198080469737 is Spooky Jun in tekken and 
#



players_steam_id <- 
  bind_rows(
    april_df %>% transmute(steamid = X1pOnlineId, rank = X1pRank),
    april_df %>% transmute(steamid = X2pOnlineId, rank = X2pRank)
  )

players_steam_id <-
  players_steam_id %>%
  group_by(steamid) %>%
  summarise(
    rank_apr = max(rank)
  )

## how many are there

joined_df <-
  analysis_df %>%
  left_join(players_steam_id)

joined_df %>%
  group_by(steamid) %>%
  summarise(
    prop.missing = rank_apr %>% is.na %>% mean
    ) %>%
  summary
## 64% are missing .. pretty good recapture rate of almsot 1/3

joined_df_t8 <-
  joined_df %>%
  filter(game == 'T8') %>%
  mutate(
    rank_diff = rank_apr - rank
  )

joined_df_t8 %>% summary ## most increased rank by 3

improv_tab <- 
  joined_df_t8 %>% 
  group_by(rank) %>%
  summarise(
    rank_diff_mean = rank_diff %>% mean(na.rm = T),
    valid_n = sum(!(rank_diff %>% is.na))
  )

## actually from the data -- people do improve by 2 rank or so above garyu!
## sample gets much higher as things go up


