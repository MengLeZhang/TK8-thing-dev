
library(tidyverse)
library(rjson)

jsons <- list.files('complete_JSONS_02_15', full.names = T)
jsons[1] %>% fromJSON() ## huge number of objects -- each has 999

feb_list <- 
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
feb_list[1]
feb_list[[1]]$replayDetailList %>% class
## R turns it into a list of things 
## We can convert to a huge data table 
## polarisid = ingame tekken 8 id?
## playername is displayed player name 
## OnlineId is steam id:
## Example: 76561198080469737 is Spooky Jun in tekken and 
#


feb_list[[1]]
feb_df <- 
  feb_list %>% bind_rows()
feb_df <- feb_df$replayDetailList ## nested df 


players <- c(feb_df$`1pPlayerName`, feb_df$`2pPlayerName`)
players %>% unique
players %>% table %>% sort(decreasing = T) ## whoever is alex played a lot 

feb_df_uniques <-
  feb_df %>%
  group_by(`1pPlayerName`) %>%
  summarise(
    rank = 
      #sample(`1pRank`)[1],
      max(`1pRank`),
    games = n()
  )

games_tab <-
  feb_df_uniques %>%
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
feb_df_uniques %>% 
  ggplot(aes(x = `rank`, y = after_stat(prop))) +
  geom_bar(
  )
?geom_bar()

feb_df_uniques %>% 
  ggplot(aes(x = `rank`)) +
  stat_ecdf()



feb_df_uniques %>% 
  filter(rank > 9) %>% #omit first 9 ranks (beginner - cav)
  ggplot(aes(x = `rank`)) +
  stat_ecdf()

## compare w/ original data 
feb_df %>%
  filter(`1pRank` > 9) %>% #omit first 9 ranks (beginner - cav)
  ggplot(aes(x = `1pRank`)) +
  stat_ecdf()
## no too different apart from the fact that it makes red ranks look less impressive (bc overcounting higher ranks )
feb_df$`1pRank` %>% table
feb_df%>% names()
feb_df$battleType ## 2 = ranked matches
## ah they didn't randomly sample -- so 
feb_df_uniques
feb_df$battleType %>% table() ## overwhleming majority is ranked 

## by ranked 
feb_df %>%
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
feb_df %>% names()
steam_id_checks <-
  feb_df %>%
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

write_csv(steam_id_checks, 'steam checks 02_15.csv')


### --- check if any linked entries 
steam_march <- read_csv('steam checks 03_01.csv')
steam_feb <- read_csv('steam checks 02_15.csv')

steam_feb$steamid %in% steam_march$steamid %>% table ## ooo we can link 4110 players!

