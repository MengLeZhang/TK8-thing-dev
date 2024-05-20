### note steam api check
## using the list of ranked players in note read json 03_01
library(tidyverse)
library(jsonlite)
library(httr2)



steam_may <- read.csv('steam checks 05_11.csv', colClasses =  'character')
## issue with accuracy closs
#steam_may <- steam_id_checks ## load from other note

#steam_may <- steam_may %>% mutate(steamid = as.character(steamid))
steam_may %>% tail
## i think potential errors in reading names


steam_api_key <-
  readline(prompt = "[Instruction] Write steam api in console: ")

### API wiki: https://developer.valvesoftware.com/wiki/Steam_Web_API#GetPlayerSummaries_.28v0002.29

#extra_args <- 
##   '&input_json={appids_filter: [ 1778820, 389730 ]}' ### bad example -- json needs url encoding
#  '&input_json=%7Bappids_filter%3A%20%5B%201778820%2C%20389730%20%5D%7D'# encode even {}
## https://onlinejsontools.com/url-encode-json
## URL encode issue: https://developer.valvesoftware.com/wiki/Steam_Web_API/Feedback
## extra argument
## nope cannot get it to work

### use fromJSON to direct get from url
## stratifed sample
id_test <- 
  steam_may %>% 
  split(.$rank) %>%
  map(
    .f = function(x){
      if(nrow(x) < 50){return(x$steamid)}
      x$steamid %>% sample(50, replace = T)
    } 
  )

id_test <- id_test %>% unlist


## weights for the stratified stats 
id_test_weights <-
  steam_may %>%
  group_by(rank) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    sample_weights = n / nrow(steam_may)
  )


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



requests[1349] %>% jsonlite::fromJSON() ## specifically needs jsonlite


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
## 28% - 30% response rates -- so not great
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



analysis <-
  out %>% 
  left_join(steam_may) %>%
  left_join(id_test_weights)


## save
dir.create('steam data')
stamp <- Sys.time() %>% gsub(x= ., ':', '-')
save_nm <- paste0('steam data/linked steam data 11_05 (', stamp, ').csv')
write_csv(analysis, save_nm)



analysis <-
  analysis %>%
  filter(playtime_forever > 0) %>% ## people hiding hours
  mutate(
    game = ifelse(appid == '1778820', 'T8', 'T7')
  )

rank_times <- 
  analysis %>% 
  filter(playtime_forever > 0) %>% ## people hiding hours
  group_by(rank, game) %>%
  summarise(
    mean = mean(playtime_forever),
    median = median(playtime_forever),
    n = n()
  )


analysis %>%
  ggplot(aes(x = playtime_forever, y= rank %>% as.numeric, colour = game)) +
  geom_smooth(
    ## Note: geom_smooth does use weights but throws a misleading error mesg:
    ## see: https://github.com/tidyverse/ggplot2/issues/5053
    
    aes(weight = sample_weights )
  ) +
  xlim(c(0, 500)) +
  xlab('total playtime (hours)') +
  ylab('rank (15 = Garyu)') +
  ggtitle('T8 rank by total playtime') +
  theme(legend.position = 'bottom')
