## Note: rank analysis 

library(tidyverse)


linked_path <- list.files('steam data', full.names = T)

linked_path <- 
  linked_path[linked_path %>% grepl(pattern = '11_05')]



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

## Check of missingness 
missingness_df <-
  analysis_df %>%
  group_by(rank) %>%
  summarise(n = n())
## No real diff in access to steam profiles

## ids of old head players -- anyone with over 100 hours in T7

t7only <- 
  analysis_df %>%
  filter(game == 'T7') %>%
  select(steamid, playtime_forever) %>%
  rename(t7hours = playtime_forever)

analysis_df <-
  analysis_df %>% 
  left_join(t7only) %>% 
  replace_na(
    list(t7hours = 0)
  ) %>%
  mutate(
    t7legacy = 'a. < 100',
    t7legacy = ifelse(t7hours %>% between (100, 800), 'b. 100 - 800', t7legacy),
    t7legacy = ifelse(t7hours > 1000, 'c. > 1000', t7legacy)
  )


library(spatstat)
### Stats 
analysis_df %>%
  group_by(game) %>%
  summarise(
    time_played_mean = weighted.mean(playtime_forever, w = sample_weights),
    time_played_median = weighted.median(playtime_forever, w = sample_weights),
    time_played_mean_2wks = weighted.mean(playtime_2weeks %>% as.numeric, w = sample_weights, na.rm = T),
    time_played_median_2wks = weighted.median(playtime_2weeks, w = sample_weights),
    n = n()
    )
## median is interest   
analysis_df %>%
  split(.$game) %>%
  map(
    .f = function(x) 
      weighted.quantile(x$playtime_forever, w = x$sample_weights, probs=seq(0,1,0.10), na.rm = TRUE) %>% round(0)
  )
##   
# $T7
# 0%   10%   20%   30%   40%   50%   60%   70%   80%   90%  100% 
# 0    13    34    66   112   176   279   442   734  1321 11311 
# 
# $T8
# 0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
# 1   42   69   90  118  141  173  210  247  334 1424 

## in feb 1284 / 1614 T8 players had experience from T7, 79.5%
## in apr 1120/1388 = 80.7% so similar

analysis_df %>%
  filter(game == 'T8') %>%
  ggplot(aes(x = playtime_forever, y= rank %>% as.numeric, colour = t7legacy)) +
  geom_smooth(
    ## Note: geom_smooth does use weights but throws a misleading error mesg:
    ## see: https://github.com/tidyverse/ggplot2/issues/5053
    
    aes(weight = sample_weights )
  ) +
  xlim(c(0, 500)) + # 500 = 6 months or so
  xlab('total playtime (hours)') +
  ylab('rank (15 = Garyu)') +
  ggtitle('T8 rank by total playtime') +
  theme(legend.position = 'bottom') +
  geom_hline(yintercept = 19, alpha = 0.5) + #Flame ruler
  geom_hline(yintercept = 21, alpha = 0.5) # Fujin 

## yup definitely a slower growth for player below 1000 after 100 hours 


### plot by legacy --- 



####----
stat_Tab <- 
  analysis_df %>%
  group_by(rank, game) %>%
  summarise(
    mean_hours = playtime_forever %>% weighted.mean(w = sample_weights),
    sd_hours = playtime_forever %>% sd(),
    n = n()
  )

## filterr out beginner ranks 


lm(rank ~ playtime_forever + I(playtime_forever^2) + t7legacy, 
   analysis_df, 
   subset =(game == 'T8'),
   weights = sample_weights
   ) %>% summary
## Rsquared of 0.32 so pretty darn high 
## 1 hour increases rank by 0,05 


lm(
  rank ~ playtime_forever + t7legacy, #+ I(playtime_forever ^ 2),
  analysis_df,
  subset = (game == 'T8' & playtime_forever < 100),
  weights = sample_weights
) %>% summary
## below 100 hours = 0.1 ranks per hour (moe: 0.11 - 0.09)
## rsq = 0.33 


lm(
  rank ~ playtime_forever + t7legacy, #+ I(playtime_forever ^ 2),
  analysis_df,
  subset = (game == 'T8' & playtime_forever > 100),
  weights = sample_weights
) %>% summary

## above 100 hours it's like 1 hour = 0.012 (95% moe, 0.014 - 0.008) -- much lower Rsquared - 0.033
## no existent relationship basically 
## This is the hard stuck period 
## After Garyu/ red -- it takes like 83 hours per rank -- we need to lab to skip

## is there a soft rank slow down mechanism @garyu
## A: There is indeed a bit jump in ranked point between garyu and orange @ 4.5k -- previous jumps took 2k
## All rank points are ~4-5k to jump until fuijin to raijin (6600)
## 

## Good question is how to get over this grind point?

