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
## steam iissues?
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


out %>% 
  filter(playtime_forever > 0) %>% ## people hiding hours
  split(.$appid) %>%
  map(
    summary
  )

out

## median averagge = 82 hours, T7 = 304 with a huge RH skew (mean = 1133)
## ~ 25% had no game details. 
## 611 / 751 -- 81% played T7

## out of 100, we get 33 / 35 usuable responses 



## save
dir.create('steam data')
stamp <- Sys.time() %>% gsub(x= ., ':', '-')
save_nm <- paste0('steam data/linked steam data march cohort (', stamp, ').csv')
write_csv(analysis, save_nm)

