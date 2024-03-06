### note steam api check
## using the list of ranked players in note read json 03_01
library(tidyverse)
library(jsonlite)
library(httr2)



# steam_march <- read_csv('steam checks 03_01.csv', guess_max = Inf)
## issue with accuracy closs
steam_march <- steam_id_checks ## load from other note

#steam_march <- steam_march %>% mutate(steamid = as.character(steamid))
steam_march[1000,] %>% head

steam_march$steamid[1] %>% as.character()
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
  steam_march %>% 
  split(.$rank) %>%
  map(
    .f = function(x){
      if(nrow(x) < 100){return(x$steamid)}
      x$steamid %>% sample(100, replace = T)
    } 
  )

id_test <- id_test %>% unlist

## weights for the stratified stats 
id_test_weights <-
  steam_march %>%
  group_by(rank) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    sample_weights = n / nrow(steam_march)
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
requests[310] %>% jsonlite::fromJSON() ## specifically needs jsonlite

requests[310]

## Terms of service
##
# You are limited to one hundred thousand (100,000) calls to the Steam Web API per day. 
# Valve may approve higher daily call limits if you adhere to these API Terms of Use. 
# So ~ 69 request per minute
# https://steamcommunity.com/dev/apiterms

## depending on stratified sample; depends on the data we get out -- there's 30 rank? 0 = 29
## if we stratify by ranks won't we have to make weights? 
out_list <- map(
  requests, 
  .f = function(x){
    Sys.sleep(0.2) #0.2 secs between calls 
    jsonlite::fromJSON(x)
  }
)


requests[999]
out[990:1000]
### will bug out

## try to get stuff, otherwise null
out <- 
  out_list %>% map(.f = function(x) {tryCatch(x$response$games) })
?exists
out <- out %>% bind_rows(.id = 'steamid')

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
## median averagge = 82 hours, T7 = 406 with a huge RH skew (mean = 939)
## ~ 28% had no game details. 
## 598 / 721 -- 82% played T7

## out of 100, we get 33 / 35 usuable responses 

## save
write_csv(out, 'steam data.csv')


analysis <-
  out %>% 
  left_join(steam_id_checks) %>%
  left_join(id_test_weights)

analysis <-
  analysis %>%
  filter(playtime_forever > 0) %>% ## people hiding hours
  mutate(
    game = ifelse(appid == '1778820', 'T8', 'T7')
  )
analysis
analysis %>%
  #  filter(game == 'T7')%>%
  ggplot(aes(y = playtime_forever, x= game)) +
  geom_boxplot()
?geom_smooth

analysis %>%
  ggplot(aes(x = playtime_forever, y= rank, colour = game)) +
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

stat_Tab <- 
  analysis %>%
  group_by(rank, game) %>%
  summarise(
    mean_hours = playtime_forever %>% weighted.mean(w = sample_weights),
    sd_hours = playtime_forever %>% sd(),
    n = n()
  )


lm(rank ~ playtime_forever + I(playtime_forever^2), analysis, subset =(game == 'T8')) %>% summary
## Rsquared of 0.46 so pretty darn high 
## 1 hour increases rank by 0,13 
lm(rank ~ playtime_forever + I(playtime_forever^2), analysis, subset =(game == 'T8'&playtime_forever<100)) %>% summary
## below 100 hours = 0.19 ranks per hour
lm(rank ~ playtime_forever + I(playtime_forever^2), analysis, subset =(game == 'T8'&playtime_forever>100)) %>% summary
## above 100 hours it's like 1 hour = 0.08 

## is there a soft rank slow down mechanism @garyu

stat_Tab ## after rank 22 (Raijin) it gets low 

stat_Tab %>%
  filter(rank <= 22) %>%
  ggplot(aes(x = rank, y = mean_hours, fill = game)) +
  geom_bar(stat = 'identity', position = 'dodge')

stat_Tab %>%
  filter(game == 'T8') %>%
  ggplot(aes(x = rank, y = mean_hours)) +
  geom_bar(stat = 'identity', position = 'dodge')



## very linear relationship

# unsed -----
get_playtime_fn <- function(request){
  resp <-
    tryCatch(
      jsonlite::fromJSON(request),
      error = function() 'error'
    )
  
  if(resp == 'error')return(NULL)
  
  
  
  resp <- 
    resp$response$games %>%
    filter(appid %in% 
             c(1778820, 389730)
    ) %>%
    mutate(steam_id = steamid) %>%
    select(steam_id, appid, playtime_forever)
  return(resp)
  
}

get_playtime_fn(id_test[1]) #awesome

map(
  id_test,
  .f = get_playtime_fn
)
