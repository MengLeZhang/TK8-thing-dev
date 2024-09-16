### note: longitudinal cohort 

library(tidyverse)

linked_path <- list.files('steam data', full.names = T)

linked_path <- 
  linked_path[linked_path %>% grepl(pattern = '03_01')]

analysis_df <-
  linked_path %>% 
  map_dfr(
    .f = function(x) read.csv(x, colClasses = 'character')
  )

### API call

steam_api_key <-
  readline(prompt = "[Instruction] Write steam api in console: ")

### API wiki: https://developer.valvesoftware.com/wiki/Steam_Web_API#GetPlayerSummaries_.28v0002.29

id_test <- analysis_df$steamid %>% unique

requests <-
  paste0(
    'http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=',
    steam_api_key,
    '&steamid=',
    id_test,
    '&format=json'#,
    #    extra_args
  )

names(requests) = id_test


requests
requests[334] %>% jsonlite::fromJSON() ## specifically needs jsonlite


## Terms of service
##
# You are limited to one hundred thousand (100,000) calls to the Steam Web API per day. 
# Valve may approve higher daily call limits if you adhere to these API Terms of Use. 
# So ~ 69 request per minute
# https://steamcommunity.com/dev/apiterms

## depending on stratified sample; depends on the data we get out -- there's 30 rank? 0 = 29
## if we stratify by ranks won't we have to make weights? 

requests <- requests %>% sample() ## randomise order
out_list <- map(
  requests, 
  .f = function(x){
    Sys.sleep(0.4) #0.4 secs between calls 
    
    output <- 
      tryCatch(jsonlite::fromJSON(x), 
               error = function(x) {
                 data.frame(error = 'error')
               }
      )
    return(output)
  }
)


##which(id_test == '76561198196768218') ## okay so it will block at 4339

### will bug out

## try to get stuff, otherwise null
out <- 
  out_list %>% map(.f = function(x) {tryCatch(x$response$games) })
out <- out %>% bind_rows(.id = 'steamid')

## Response rate 
(out$steamid %>% unique() %>% length()) / (requests %>% length)
## 96.7%
## Sept: 94%
## steam iissues?


# create a variable for all games -----------------------------------------
all_games_df <-
  out %>%
  group_by(steamid) %>%
  summarise(
    appid = 'all steam games',
    playtime_forever = sum(playtime_forever, na.rm = T) / 60,
    playtime_2weeks = sum(playtime_2weeks, na.rm = T) / 60
  )

all_games_df %>% summary
all_games_df$playtime_2weeks %>% quantile(1:10/10)
## dsitrivution for all game
## Setp.
# 10%        20%        30%        40%        50%        60%        70%        80% 
#   0.000000   1.133333   6.760000  14.806667  22.383333  30.476667  42.263333  55.353333 
# 90%       100% 
#   76.823333 362.083333
## Overall 60th percentile is 30 hours over 2 weeks 
## sept
### end

## Filter to T8 and T7
out <- out %>%
  filter(appid %in% 
           c(
             1778820, 
             389730)
  ) %>%
  mutate(
    playtime_forever = playtime_forever/60,
    playtime_2weeks = playtime_2weeks/60
  )
## tekken 8 app id = 1778820
## tekken 7 id =  389730
out ## from check steam pages manually 0 means game hours are hidden


## add final hhours
out <-
  out %>% 
  mutate(appid = as.character(appid)) %>%
  bind_rows(all_games_df)


out %>% 
  filter(playtime_forever > 0) %>% ## people hiding hours
  split(.$appid) %>%
  map(
    summary
  )

## Sep:
## Median T7 = 240 - 245 hours 
## median T8 = 240 hours
## median T8 2 weeks = 8.7
## median all steam 2 weeks = 26 hours

out %>% 
  filter(playtime_forever > 0) %>% ## people hiding hours
  split(.$appid) %>%
  map(
    function(x){
      # x$playtime_2weeks %>% quantile(1:10/10, na.rm = T)
      x$playtime_forever %>% quantile(1:10/10, na.rm = T)
    }
  )
## 80th percentile of 2 week platime in T8 is 23 hours
## for EVERY game the median gaming hours is 26 hours (60th percentile is 34 hours)
## For forever: T8 80th percntile is 400 and 90th is 547! -- big jumps
## For T7 lifetime -- top 60th was 431 hours and top 80% was 1340 hours (70th was 725)






## save
dir.create('steam data')
stamp <- Sys.time() %>% gsub(x= ., ':', '-')
save_nm <- paste0('steam data/linked steam data march cohort (', stamp, ').csv')
write_csv(out, save_nm)

save_nm
