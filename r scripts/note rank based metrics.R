## note rank discrete choice values 

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

## get rank stat for wins in ranked match and quick match
## no draws
march_df %>% summary
march_df$battleType %>% table

eligible_df <- 
  march_df %>%
  filter(
    battleType %in% 1:2
  ) %>%
  filter(
    winResult %in% 1:2 ## no draws 
  )


## Base Z scores on percentile of normal dist
eligible_df <-
  eligible_df %>%
  group_by(`1pPolarisId`) %>%
  summarise(
    rank = max(`1pRank`)
  )

eligible_df <-
  eligible_df %>%
  mutate(
    rank_percentiles = rank %>% cume_dist()
  )


z_score_tab <- 
  eligible_df %>%
  group_by(rank) %>%
  summarise(
    rank_percentiles = rank_percentiles[1],
    n = n()
  )

## Get Z score 
z_score_tab <-
  z_score_tab %>%
  mutate(
    z = rank_percentiles %>% qnorm()
  )

## truncate scores to +- 1.96
z_score_tab <-
  z_score_tab %>%
  mutate(
    z = ifelse(z > 1.96, 1.96, z),
    z = ifelse(z < -1.96, -1.96, z)
  )

## save the data 


write.csv(z_score_tab, 'rank z scores.csv')


### how to use ... say we played some matches 
my_record <-
  data.frame(
    opp_rank = 0:29 %>% sample(30, replace = T),
    me_win = 0:1 %>% sample(30, replace = T)
  )

## convert opp rank into logit score
my_record <-
  my_record %>%
  mutate(
    opp_score = z_score_tab$z[opp_rank + 1]
  )

my_record_fit <-
  glm(me_win ~ offset(-opp_score), data = my_record)


my_record_fit %>% summary
## The intercept is the score! 

## example from march -- ~ 5 losses and 15 wins 
my_record <-
  data.frame(
    opp_rank = 15:18 %>% sample(20, replace = T),
    me_win = c(rep(1, 15), rep(0, 5))
  )

## convert opp rank into logit score
my_record <-
  my_record %>%
  mutate(
    opp_score = z_score_tab$z[opp_rank + 1]
  )

my_record_fit <-
  glm(me_win ~ offset(-opp_score), data = my_record)


my_record_fit %>% summary

pnorm(1.58)
pnorm(1.8) ## okay so can detect diff of 2%
## well that's pretty high but keep in mind it's a relative score
## But the SE is 0,11 so like 0.11 SD
## at 75% ; sqrt(0.75 * 0.25) / 5 = 8% or 0.08/ 0.5  = 0.16SD


## simmulation?
## Essentially in 100 games -- if our winrate rises from 50% to 55% can we detect a diff?
my_record <-
  data.frame(
    opp_rank = 0:29 %>% sample(100, replace = T)
  ) %>%
  mutate(
    opp_score = z_score_tab$z[opp_rank + 1]
  )


glm(me_win ~ offset(opp_score), 
    data = my_record %>%
      mutate(me_win = c(rep(1, 50), rep(0, 50)))
      ) %>% summary

## in theory due to the SE -- the SE is 0.15 population SD
## whilst in 100 games, 5% is the winrate (sqrt(0.5*0.5) / 10 ) SE which is equal to 0.1 SD 

glm(0:1 %>% sample(100, replace = T) ~1) %>% summary

## Let's just check out the sample size and SE
## Sample it 
## convert win rate to Z scores 
empirical_df <- 
 eligible_df[101:200, ]
empirical_df <-
  empirical_df %>% 
  left_join(
    z_score_tab
  )

## result list

sim_res <- list()
for (i in 1:1e4){
  empirical_df <-
    empirical_df %>%
    mutate(
      me_win = 0:1 %>% sample(100, replace = T)
    )
  
  glm_z <- 
    glm(me_win ~ offset(-z), 
        family = binomial(link = 'probit'), 
        data = empirical_df)
  
  glm_z <- glm_z$coefficients[1]
  rate_z <- empirical_df$me_win %>% mean() %>% qnorm()
  
  sim_res[[i]] <-
    data.frame(glm_z = glm_z, rate_z = rate_z)

}
sim_res <- sim_res %>% bind_rows()

sim_res %>% summary
sim_res$glm_z %>% sd()
sim_res$rate_z%>% sd()

## okay answer = the rate itself has a lower SD! 

