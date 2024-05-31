## wavu scrap function

get_wavu_stats <- 
  function(tekken_id){
    ## replace - 
    tekken_id <- tekken_id %>% gsub('-', '', x = .)
    
    wavu_url <-
      #  'https://wank.wavu.wiki/player/5RM58bMi8BMM'
      'https://wank.wavu.wiki/player/' %>%
      paste0(
        tekken_id
      )
    
    thisPage <- 
      read_html(wavu_url)
    
    
    ## We can select elements by xpath
    
    ## Okay we can get the table
    xpath_idTable <- "/html/body/main/table[1]"
    xpath_ratingTable <- "/html/body/main/table[3]" 
    xpath_replayTable <- "/html/body/main/div/table"
    
    
    ## Get replay table
    replayTable <-
      thisPage %>% 
      html_element(xpath = xpath_replayTable) %>%
      html_table()
    ## 
    replayTable_cleaner <-
      replayTable %>%
      transmute(
        When = When %>% anytime::anytime(tz = 'GMT'),
        Rating = Rating %>% substr(1,4) %>% as.numeric()
      )
    

    return(replayTable_cleaner)
  }

get_wavu_stats('2yGr72b2854B')


## Get sampling frame
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
    t7legacy = ifelse(t7hours > 800, 'c. > 800', t7legacy)
  )



## Get original lookup to tekken id
may_df <-
  'csv_11_5/replays.csv' %>%
  read.csv()
may_df %>% head()
tekkenid_lookup <-
  may_df %>%
  transmute(
    steamid = X1pOnlineId,
    tekkenid = X1pPolarisId
  ) %>%
  group_by(steamid, tekkenid) %>%
  summarise(
    flag = 1
  ) 

analysis_df <-
  analysis_df %>% 
  left_join(tekkenid_lookup)


## Now to try to get the wavu stats 

id_query <- 
  (analysis_df$tekkenid %>% unique())

wavu_results <-
  id_query %>%
  map(
    .f = function(x){
      Sys.sleep(1)
      get_wavu_stats(x)
      }
  )


names(wavu_results) <- id_query

wavu_results_tab <-
  wavu_results %>% bind_rows(.id = 'tekkenid')


wavu_results_tab %>% saveRDS('wavu scrape.rds')

## latest ratings
player_rating <-
  wavu_results_tab %>%
  group_by(tekkenid) %>%
  summarise(
    When = When %>% head(1),
    Rating = Rating %>% head(1),
    n_played = n()
  )



player_rating <-
  player_rating %>%
  left_join(
    analysis_df %>% filter(game == 'T8')
    )

player_rating$sample_weights
player_rating %>%
  filter(
    !(sample_weights %>% is.na)
      ) %>%
  ggplot(
    aes(x = Rating, weights = sample_weights)
  ) +
  geom_density() +
  scale_x_continuous(breaks = seq(900, 2300, 100))

## median is exactly 1500
player_rating %>% head

player_rating %>%
  filter(
    !(sample_weights %>% is.na)
  ) %>%
  ggplot(
    aes(x = Rating, weights = sample_weights)
  ) +
  stat_ecdf(geom = "step") +
  scale_x_continuous(breaks = seq(900, 2300, 100)) 


stat_ecdf(geom = "step")

rating_rank_df <- 
  player_rating %>%
  group_by(rank) %>%
  summarise(
    Rating = Rating %>% median(na.rm = T),
    n = n()
  )

rating_rank_df %>% write.csv('wavu rating by rank.csv')

player_rating %>% summary
player_rating$playtime_forever
player_rating %>%
  ggplot(
    aes(y = Rating, x = playtime_forever, colour = t7legacy)
  ) +
  geom_smooth() +
  xlim(c(0, 500)) +
  #scale_x_continuous(breaks = seq(0, 600, 100)) +
  scale_y_continuous(breaks = seq(1000, 2400, 100)) +
  theme(legend.position = 'bottom') 
## roughly 50 hours for like 50 rating
## S
?cor
cor(player_rating$rank, player_rating$Rating, use = 'complete.obs')
cor(player_rating$rank, player_rating$Rating, use = 'complete.obs', method = 'spearman')
## yeah fairly good correlation