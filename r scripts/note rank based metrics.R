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


## we can simply summarise the data 
eligible_df <-
  eligible_df %>%
  group_by(
    `1pRank`, `2pRank`
  ) %>%
  summarise(
    n = n(),
    p1_win = sum(winResult == 1)
  ) %>%
  mutate(
    p1_rate = p1_win / n
  )

## save this data 
write_csv(eligible_df, 'edgelist of wins by rank.csv')


## if we're super lazy we can simply run the probit

rank_fit <-
  glm(I(winResult == 1) ~ I(`2pRank` %>% as.factor) - 1, data = march_df, family = binomial(link = 'probit'))

# rank_fit %>% summary
# 
# Call:
#   glm(formula = I(winResult == 1) ~ I(`2pRank` %>% as.factor) - 
#         1, family = binomial(link = "probit"), data = march_df)
# 
# Deviance Residuals: 
#   Min      1Q  Median      3Q     Max  
# -1.301  -1.170  -1.012   1.164   1.657  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# I(`2pRank` %>% as.factor)0   0.179554   0.005312  33.799  < 2e-16 ***
#   I(`2pRank` %>% as.factor)1   0.133827   0.011741  11.398  < 2e-16 ***
#   I(`2pRank` %>% as.factor)2   0.126516   0.010339  12.237  < 2e-16 ***
#   I(`2pRank` %>% as.factor)3   0.079819   0.008393   9.510  < 2e-16 ***
#   I(`2pRank` %>% as.factor)4   0.085906   0.008290  10.363  < 2e-16 ***
#   I(`2pRank` %>% as.factor)5   0.071322   0.008178   8.722  < 2e-16 ***
#   I(`2pRank` %>% as.factor)6   0.047957   0.006620   7.244 4.36e-13 ***
#   I(`2pRank` %>% as.factor)7   0.043739   0.008428   5.190 2.11e-07 ***
#   I(`2pRank` %>% as.factor)8   0.058978   0.006337   9.307  < 2e-16 ***
#   I(`2pRank` %>% as.factor)9   0.155982   0.005310  29.376  < 2e-16 ***
#   I(`2pRank` %>% as.factor)10  0.043049   0.005435   7.920 2.37e-15 ***
#   I(`2pRank` %>% as.factor)11 -0.010959   0.005300  -2.068 0.038664 *  
#   I(`2pRank` %>% as.factor)12  0.064205   0.004376  14.673  < 2e-16 ***
#   I(`2pRank` %>% as.factor)13  0.019150   0.004661   4.109 3.98e-05 ***
#   I(`2pRank` %>% as.factor)14  0.044336   0.004492   9.870  < 2e-16 ***
#   I(`2pRank` %>% as.factor)15 -0.012889   0.003426  -3.762 0.000169 ***
#   I(`2pRank` %>% as.factor)16 -0.092327   0.004475 -20.630  < 2e-16 ***
#   I(`2pRank` %>% as.factor)17 -0.125863   0.004761 -26.436  < 2e-16 ***
#   I(`2pRank` %>% as.factor)18 -0.133069   0.004777 -27.858  < 2e-16 ***
#   I(`2pRank` %>% as.factor)19 -0.125283   0.006076 -20.618  < 2e-16 ***
#   I(`2pRank` %>% as.factor)20 -0.139645   0.006782 -20.591  < 2e-16 ***
#   I(`2pRank` %>% as.factor)21 -0.185108   0.006875 -26.926  < 2e-16 ***
#   I(`2pRank` %>% as.factor)22 -0.179462   0.009806 -18.301  < 2e-16 ***
#   I(`2pRank` %>% as.factor)23 -0.189204   0.011550 -16.381  < 2e-16 ***
#   I(`2pRank` %>% as.factor)24 -0.197202   0.014672 -13.441  < 2e-16 ***
#   I(`2pRank` %>% as.factor)25 -0.223869   0.018367 -12.189  < 2e-16 ***
#   I(`2pRank` %>% as.factor)26 -0.303965   0.023086 -13.167  < 2e-16 ***
#   I(`2pRank` %>% as.factor)27 -0.250758   0.033893  -7.398 1.38e-13 ***
#   I(`2pRank` %>% as.factor)28 -0.357884   0.047031  -7.610 2.75e-14 ***
#   I(`2pRank` %>% as.factor)29 -0.663450   0.060938 -10.887  < 2e-16 ***
(rank_fit %>% summary)$coef
write.csv((rank_fit %>% summary)$coef, 'rank baselines from probit.csv')


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
    opp_score = rank_fit$coefficients[opp_rank + 1]
  )

my_record_fit <-
  glm(me_win ~ offset(opp_score), data = my_record)


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
    opp_score = rank_fit$coefficients[opp_rank + 1]
  )

my_record_fit <-
  glm(me_win ~ offset(opp_score), data = my_record)


my_record_fit %>% summary
## well that's pretty high but keep in mind it's a relative score

##