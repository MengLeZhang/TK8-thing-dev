### note steam api check
## using the list of ranked players in note read json 03_01
library(tidyverse)
library(jsonlite)
library(httr2)

## 
requests <-
  'https://wank.wavu.wiki/api/replays'
# 'https://wank.wavu.wiki/api/replays?before=' ##?before is the only parameter 
# Returns: replays ordered by battle_at descending where battle_at <= before and battle_at > before - 700

out <- jsonlite::fromJSON(requests)

out %>% summary
out$battle_at %>% max ## no idea what this parameter is but default is 1726587151
out

## get rankings for p1 and p2

players_df <-
  data.frame(
    p_nm = c(out$p1_name, out$p2_name),
    polaris_id = c(out$p1_polaris_id, out$p2_polaris_id) %>% as.character(), ## i think the user is used for wavu
    char = c(out$p1_chara_id, out$p2_chara_id),
    rank = c(out$p1_rank, out$p2_rank),
    rating = c(out$p1_rating_before, out$p2_rating_before)
  )

players_df

## For each player take their first recorded instance 

players_df <-
  players_df %>%
  filter(
    !(polaris_id %>% duplicated)
  )


rank_tab <-
  players_df %>%
  group_by(rank) %>%
  summarise(
    rating_q = rating %>% 
      quantile(c(0.25, 0.5, 0.75), na.rm = T) %>% 
      round(0) %>% paste(collapse = '/ '),
    rating = rating %>% mean(na.rm = T),
    n = n ()
  ) %>%
  mutate(
    prop = cumsum(n) / sum(n)
  )

rank_tab
players_df$rating %>% quantile(1:20/20, na.rm = T)
players_df$rating %>% sd(na.rm = T)
## create a time stampe 
time_stamp = Sys.time()

players_df <-
  players_df %>%
  mutate(
    time = time_stamp
  )


## save
dir.create('wavu data')
players_df %>% 
  write_csv(
    file.path(
      'wavu data', sample.int(1000, 1)
    )
  )

#sample.int(1000, 1)


# note: get all data ------------------------------------------------------

all_players_df <-
  list.files('wavu data', full.names = T) %>%
  map_dfr(
    read_csv
  )

## Remove duplicated
all_players_df <-
  all_players_df %>%
  filter(
    !(p_nm %>% duplicated())
  )

## Get distribution 
all_players_df 

all_rank_tab <-
  all_players_df %>%
  group_by(rank) %>%
  summarise(
    rating_q = rating %>% 
      quantile(c(0.25, 0.5, 0.75), na.rm = T) %>% 
      round(0) %>% paste(collapse = '/ '),
    rating = rating %>% mean(na.rm = T) %>% round(0),
    n = n ()
  ) %>%
  mutate(
    prop = cumsum(n) / sum(n)
  )


###
all_rank_tab %>% view()
all_rank_tab %>% write_csv('ranks.csv')

### Azu cena

char_rank_tab <-
  all_players_df %>%
  filter(char == 6)  %>% ## Azucena is 34
  group_by(rank) %>%
  summarise(
    rating_q = rating %>% 
      quantile(c(0.25, 0.5, 0.75), na.rm = T) %>% 
      round(0) %>% paste(collapse = '/ '),
    rating = rating %>% mean(na.rm = T) %>% round(0),
    n = n ()
  ) %>%
  mutate(
    prop = cumsum(n) / sum(n)
  )


char_Tab <- all_players_df %>%
  group_by(char) %>%
  summarise(
    rating_mean = rating %>% mean(na.rm = T) %>% round(0),
    rating_med = rating %>% median(na.rm = T) %>% round(0),
    n = n ()
  )

char_Tab %>% select(rating_mean, n) %>% cor(method = 'spearman') ## HUGE correlation between
## avg rating and character usage
char_Tab %>% select(rating_med, n) %>% cor(method = 'spearman') ## HUGE correlation between


char_Tab %>%
  ggplot(aes(x = n, y= rating_med)) +
  geom_point()
