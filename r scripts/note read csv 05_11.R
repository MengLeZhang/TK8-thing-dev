
library(tidyverse)


may_df <-
  'csv_11_5/replays.csv' %>%
  read.csv()

may_df %>% names()
## omit things that aren't dataframe
## R turns it into a list of things 
## We can convert to a huge data table 
## polarisid = ingame tekken 8 id?
## playername is displayed player name 
## OnlineId is steam id:
## Example: 76561198080469737 is Spooky Jun in tekken and 
#



players <- c(may_df$X1pPlayerName, may_df$`X2pPlayerName`)
players %>% unique
players %>% table %>% sort(decreasing = T) ## whoever is alex played a lot 

may_df_uniques <-
  may_df %>%
  group_by(`X1pPlayerName`) %>%
  summarise(
    rank = 
      #sample(`1pRank`)[1],
      max(`X1pRank`),
    games = n()
  )

games_tab <-
  may_df_uniques %>%
  group_by(rank) %>%
  summarise(
    mean_games = games %>% mean()
  ) 


## Mighty ruler = 18, Fujin = 21
games_tab %>%
  ggplot(aes(x = rank, y = mean_games)) +
  geom_point() +
  ylim(c(0,20))
## lots of games for higher ranks which inflates their true numbers 
## ah nvm we can used their hgihest rank
sample(1:3)
may_df_uniques %>% 
  ggplot(aes(x = `rank`, y = after_stat(prop))) +
  geom_bar(
  )
?geom_bar()

may_df_uniques %>% 
  ggplot(aes(x = `rank`)) +
  stat_ecdf()



may_df_uniques %>% 
  filter(rank > 9) %>% #omit first 9 ranks (beginner - cav)
  ggplot(aes(x = `rank`)) +
  stat_ecdf()

## compare w/ original data 
may_df_uniques
may_df$battleType %>% table() ## overwhleming majority is ranked 

## by ranked 
may_df %>%
  filter(battleType == 2) %>%
  group_by(`X1pPlayerName`) %>%
  summarise(
    rank = sample(`X1pRank`)[1],
    games = n()
  ) %>% 
  filter(rank > 9) %>% #omit first 9 ranks (beginner - cav)
  ggplot(aes(x = `rank`)) +
  stat_ecdf()
# of all ranked it's about the same 

## Check ID for hours 
may_df %>% names()

may_df %>% head
may_df$`X1pPlatform` %>% table

## rematches

rematch_tab <- 
  may_df %>%
  group_by(`X1pPlayerName`, `X2pPlayerName`, battleType) %>%
  summarise(
    matches = n(),
    rank_p1 = `X1pRank`[1],
    wins_p1 = sum(winResult == 1),
    wins_p2 = sum(winResult == 2)
  ) %>%
  mutate(
    ft2_complete = max(wins_p1, wins_p2) > 1 #basically did either of them get 2 wins or more
  )

rematch_tab %>% summary
(rematch_tab$ft2_complete) %>% mean(na.rm = T) ## 20% ah right this is due to the way the data is collected

## Extract steam idea 

may_df$battleType %>% table

steam_id_checks <-
  may_df %>%
  filter(battleType == 2) %>%
  filter(
    `X1pPlatform` == 3
  ) %>%
  group_by(`X1pPlayerName`) %>%
  summarise(
    steamid = `X1pOnlineId`[1],
    rank = 
      #sample(`1pRank`)[1],
      max(`X1pRank`),
    games = n()
  )


steam_id_checks[300,]
write_csv(steam_id_checks, 'steam checks 05_11.csv')
# saveRDS(steam_id_checks, 'steam checks 03_01.rds')

