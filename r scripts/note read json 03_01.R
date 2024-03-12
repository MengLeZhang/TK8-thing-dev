
library(tidyverse)
library(rjson)

jsons <- list.files('complete_JSONS_03_01', full.names = T)
jsons[1] %>% fromJSON() ## huge number of objects -- each has 999
## sample 200 
#jsons <- jsons %>% sample(200)

march_list <- 
  #fromJSON(file = jsons[1])
  jsons %>%
  map(
    .f = function(x){ 
      out <- 
      tryCatch(
        jsonlite::fromJSON(x), error = function(e) data.frame(error = x) ## create a dataframe called error
#        finally = print('error')
      )
    return(out)
    }
  )

## omit things that aren't dataframe
## R turns it into a list of things 
## We can convert to a huge data table 
## polarisid = ingame tekken 8 id?
## playername is displayed player name 
## OnlineId is steam id:
## Example: 76561198080469737 is Spooky Jun in tekken and 
#


march_df <- 
  march_list %>% bind_rows()
march_df <- march_df$replayDetailList ## nested df 

players <- c(march_df$`1pPlayerName`, march_df$`2pPlayerName`)
players %>% unique
players %>% table %>% sort(decreasing = T) ## whoever is alex played a lot 

march_df_uniques <-
  march_df %>%
  group_by(`1pPlayerName`) %>%
  summarise(
    rank = 
      #sample(`1pRank`)[1],
      max(`1pRank`),
    games = n()
  )

games_tab <-
  march_df_uniques %>%
  group_by(rank) %>%
  summarise(
    mean_games = games %>% mean()
  ) 

games_tab %>%
  ggplot(aes(x = rank, y = mean_games)) +
  geom_point() +
  ylim(c(0,10))
## lots of games for higher ranks which inflates their true numbers 
## ah nvm we can used their hgihest rank
sample(1:3)
march_df_uniques %>% 
  ggplot(aes(x = `rank`, y = after_stat(prop))) +
  geom_bar(
  )
?geom_bar()

march_df_uniques %>% 
  ggplot(aes(x = `rank`)) +
  stat_ecdf()



march_df_uniques %>% 
  filter(rank > 9) %>% #omit first 9 ranks (beginner - cav)
  ggplot(aes(x = `rank`)) +
  stat_ecdf()

## compare w/ original data 
march_df %>%
  filter(`1pRank` > 9) %>% #omit first 9 ranks (beginner - cav)
  ggplot(aes(x = `1pRank`)) +
  stat_ecdf()
## no too different apart from the fact that it makes red ranks look less impressive (bc overcounting higher ranks )
march_df$`1pRank` %>% table
march_df%>% names()
march_df$battleType ## 2 = ranked matches
## ah they didn't randomly sample -- so 
march_df_uniques
march_df$battleType %>% table() ## overwhleming majority is ranked 

## by ranked 
march_df %>%
  filter(battleType == 2) %>%
  group_by(`1pPlayerName`) %>%
  summarise(
    rank = sample(`1pRank`)[1],
    games = n()
  ) %>% 
  filter(rank > 9) %>% #omit first 9 ranks (beginner - cav)
  ggplot(aes(x = `rank`)) +
  stat_ecdf()
# of all ranked it's about the same 

## Check ID for hours 
march_df %>% names()

march_df %>% head
march_df$`1pPlatform` %>% table

## rematches

rematch_tab <- 
  march_df %>%
  group_by(`1pPlayerName`, `2pPlayerName`, battleType) %>%
  summarise(
    matches = n(),
    rank_p1 = `1pRank`[1],
    wins_p1 = sum(winResult == 1),
    wins_p2 = sum(winResult == 2)
  ) %>%
  mutate(
    ft2_complete = max(wins_p1, wins_p2) > 1 #basically did either of them get 2 wins or more
  )

rematch_tab %>% summary
(rematch_tab$ft2_complete) %>% mean(na.rm = T) ## 76.5% of matches are sets of 2 or more



rematch_tab %>%
  group_by(battleType) %>%
  summarise(
    n_matches = n(),
    ft2_complete = ft2_complete %>% mean(na.rm = T)
  )
## very similar rates of rematches betwen QM and ranked -- 77.6% vs 74.8%
# 1= QM, 2 = RM, 3 = group match, 4 = player match
## if player matches are the eligible set (91.6) then 74.8/ 91.6 = 81.2% of eligible matches aren't FT2

ft2_rank_tab <-
  rematch_tab %>%
  group_by(battleType, rank_p1) %>%
  summarise(
    n_matches = n(),
    ft2_complete = ft2_complete %>% mean(na.rm = T)
  )

ft2_rank_tab %>%
  filter(battleType == 2) %>%
  ggplot(
    aes(x = rank_p1, y = ft2_complete)
  ) +
  geom_point() +
  ylab('% completed FT2 (1 = 100%)') +
  xlab('Player 1 rank') +
  ggtitle(
    'Completed FT2 % in ranked by P1 rank'
  )

match_tab$matches %>% table()

## Extract steam idea 

steam_id_checks <-
  march_df %>%
  filter(
    `1pPlatform` == 3
  ) %>%
  group_by(`1pPlayerName`) %>%
  summarise(
    steamid = `1pOnlineId`[1],
    rank = 
      #sample(`1pRank`)[1],
      max(`1pRank`),
    games = n()
  )

steam_id_checks[300,]
#write_csv(steam_id_checks, 'steam checks 03_01.csv')
# saveRDS(steam_id_checks, 'steam checks 03_01.rds')

1 / 0.75
1.333 / 2.333
