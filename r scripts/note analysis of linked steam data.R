## Note: rank analysis 

library(tidyverse)

linked_path <- list.files('steam data', full.names = T)


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

## ids of old head players -- anyone with over 100 hours in T7
legacy_ids <- 
  analysis_df %>%
  filter(
    game == 'T7'
  ) %>%
  filter(
    playtime_forever > 100
  )

legacy_mid_ids <- 
  analysis_df %>%
  filter(
    game == 'T7'
  ) %>%
  filter(
    playtime_forever %>% between(100, 800)
  )


### Stats 
analysis_df %>%
  group_by(game) %>%
  summarise(
    time_played = weighted.mean(playtime_forever, w = sample_weights)
    )

## 1284 / 1614 T8 players had experience from T7, 79.5%

analysis_df %>%
#  filter(rank > 4) %>%
#  filter(!(steamid %in% legacy_ids$steamid))  %>% ##// newer player
#  filter((steamid %in% legacy_ids$steamid))  %>% ##// T7 players 
#  filter(steamid %in% legacy_mid_ids$steamid) %>%  ##// mid level
  ggplot(aes(x = playtime_forever, y= rank %>% as.numeric, colour = game)) +
  geom_smooth(
    ## Note: geom_smooth does use weights but throws a misleading error mesg:
    ## see: https://github.com/tidyverse/ggplot2/issues/5053
    
    aes(weight = sample_weights )
  ) +
  xlim(c(0, 300)) +
  xlab('total playtime (hours)') +
  ylab('rank (15 = Garyu)') +
  ggtitle('T8 rank by total playtime') +
  theme(legend.position = 'bottom')

## if we actually filter to newer player it take way over 100 hours to get garyu
## older players will take ~ 70 hours
## slower growth at 100 hours is real -- and almst linear

stat_Tab <- 
  analysis_df %>%
  group_by(rank, game) %>%
  summarise(
    mean_hours = playtime_forever %>% weighted.mean(w = sample_weights),
    sd_hours = playtime_forever %>% sd(),
    n = n()
  )

## filterr out beginner ranks 


lm(rank ~ playtime_forever + I(playtime_forever^2), 
   analysis_df, 
   subset =(game == 'T8'),
   weights = sample_weights
   ) %>% summary
## Rsquared of 0.32 so pretty darn high 
## 1 hour increases rank by 0,09 (moe ~ 0.8 - 1.0)


lm(
  rank ~ playtime_forever, #+ I(playtime_forever ^ 2),
  analysis_df,
  subset = (game == 'T8' & playtime_forever < 100),
  weights = sample_weights
) %>% summary
## below 100 hours = 0.1 ranks per hour (moe: 0.11 - 0.09)
## rsq = 0.33 


lm(
  rank ~ playtime_forever, #+ I(playtime_forever ^ 2),
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

