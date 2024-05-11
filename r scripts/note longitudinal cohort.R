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

cohort_hours_april <-
  'steam data/linked steam data march cohort (2024-05-11 08-55-29).csv' %>%
  read.csv(colClasses = 'character')



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

## join times -----

cohort_hours_april %>% summary
## ah a whole bunch nuber played last 2 weeks
cohort_hours_april <-
  cohort_hours_april %>%
  rename(
    playtime_forever_apr = playtime_forever,
    playtime_2wk_apr = playtime_2weeks
  ) %>%
  mutate(
    playtime_forever_apr = playtime_forever_apr %>% as.numeric,
    playtime_2wk_apr = playtime_2wk_apr %>% as.numeric
  )

analysis_df <-
  analysis_df %>%
  left_join(cohort_hours_april)

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
    prop.missing_rank = rank_apr %>% is.na %>% mean,
    prop.missing_apr = playtime_forever_apr %>% is.na %>% mean
    ) %>%
  summary
## 64% are missing .. pretty good recapture rate of almsot 1/3

joined_df_t8 <-
  joined_df %>%
  filter(game == 'T8') %>%
  mutate(
    rank_diff = rank_apr - rank,
    time_diff = playtime_forever_apr - playtime_forever
  )

joined_df_t8 %>% summary ## most increased rank by 3 but mean increase is 57 extra hours, median = 39
joined_df_t8$rank_diff %>% sd(na.rm = T)
joined_df_t8$time_diff %>% sd(na.rm = T) ## okay good SD for time so that will help any analysis

## Check sample selection bias on times 

## see graph of play time in last 2 week 
joined_df_t8 %>% 
  filter(rank  >= 15) %>%
  ggplot(aes(x = playtime_2wk_apr)) +
  geom_density()

joined_df_t8 %>% 
#  filter(rank  >= 15) %>%
  ggplot(aes(y = playtime_2wk_apr, x = (rank >= 15))) +
  geom_boxplot()

## basically the median player and above and how much they play 
## median for those who were median rank or above is like 20 per 2 weeks or so


joined_df_t8 %>%
  group_by(is.na(rank_diff)) %>%
  summarise(
    time_diff_mean  = time_diff %>% mean(na.rm = T),
    time_diff_median  = time_diff %>% median(na.rm = T),
    time_diff_sd = time_diff %>% sd(na.rm = T),
    playtime_2wk_apr_median = playtime_2wk_apr %>% median(na.rm = T),
    little_time = mean(time_diff < 3, na.rm = T), ## under 3 hours
    n = n()
  )
## okay big selection bias for obvious reasons -- we are less likely to pick up ranks 
## for those tho put less time in 
## good variance though -- we have 575 available data points

joined_df_t8
improv_tab <- 
  joined_df_t8 %>% 
  filter(! (rank_diff %>% is.na)) %>%
  group_by(rank) %>%
  summarise(
    rank_diff_mean = rank_diff %>% mean(na.rm = T),
    time_diff_mean = time_diff %>% mean(na.rm = T),
    valid_n = sum(!(rank_diff %>% is.na))
  )

## Graphing the data ---------------------


joined_df_t8 %>%
  filter(game == 'T8') %>%
  ggplot(aes(x = time_diff, y= rank_diff %>% as.numeric)) +
  geom_point(alpha = 0.1) +
  geom_smooth(
    ## Note: geom_smooth does use weights but throws a misleading error mesg:
    ## see: https://github.com/tidyverse/ggplot2/issues/5053
    
    aes(weight = sample_weights )
  ) +
  xlim(c(0, 400)) +
  xlab('playtime increase (hours)') +
  ylab('rank difference') +
  ggtitle('T8 rank by total playtime') +
  theme(legend.position = 'bottom')

### ooo generally 2 ranks of increase but honestly only an increase under 100 hours

## actually from the data -- people do improve by 2 rank or so above garyu!
## sample gets much higher as things go up
## BUT this is a self selected sample as the time increase is huge! 80 or so hours in purple
## way above median and mean -- 
## data initally collected on 7/3 and hours recollected 20/4
## ~43 days -- so they play liek 2 horus per day
joined_df_t8
lm(rank_diff ~ time_diff, data = joined_df_t8, subset = time_diff > 0, weights = sample_weights) %>% summary ## hilarious no difference 
## so in general people increase their rank by 2 but time spent affected it by 0,007! 

lm(rank_diff ~ time_diff, data = joined_df_t8, subset = (rank > 14 & time_diff > 0), weights = sample_weights) %>% summary
## for Garyu .. time spend increases ranked by 0.004 but overally not really effects 

lm(rank_diff ~ time_diff, data = joined_df_t8, subset = (rank > 14 & time_diff < 80), weights = sample_weights) %>% summary
## okay much more optimistic if we cut to where most of the data is 
## 0.06 ranks per hour but you need to put in the hours

## conclusion pure time spent is not a good predictor of rank climbing beyond a certain amount of time
## something like 0