library(tidyverse)
library(rvest)
library(lubridate)

## Follow this guide: https://statsandr.com/blog/web-scraping-in-r/

wavu_url <-
#  'https://wank.wavu.wiki/player/5RM58bMi8BMM'
  'https://wank.wavu.wiki/player/3Mg56NgbT4yd'

thisPage <- 
  read_html(wavu_url)


## We can select elements by xpath

## Okay we can get the table
thisPage %>% html_element(xpath = "/html/body/main/table[1]") %>%
  html_table()


xpath_idTable <- "/html/body/main/table[1]"
xpath_ratingTable <- "/html/body/main/table[3]" 
xpath_replayTable <- "/html/body/main/div/table"


replayTable <-
  thisPage %>% 
  html_element(xpath = xpath_replayTable) %>%
  html_table()


replayTable$Rating %>% substr(1,4) %>% as.numeric() %>% plot ## read in reverse
replayTable$`Opp. rating` %>% substr(1,4) %>% as.numeric() %>% plot
replayTable$`Opp. rating` %>% substr(1,4) %>% as.numeric() %>% hist()

## At the very least this will be useful for automated stat checks 
replayTable$When[1] %>% parse_date_time('dmY')
replayTable$When[100]
replayTable$When[100] %>% anytime::anytime(tz = 'GMT') ##hour is like +1 though
getTZ()
?anytime::anytime

## 
replayTable_cleaner <-
  replayTable %>%
  transmute(
    When = When %>% anytime::anytime(tz = 'GMT'),
    Rating = Rating %>% substr(1,4) %>% as.numeric()
  )

replayTable_cleaner %>% 
  mutate(
    row_id = n():1
  ) %>%
  filter(row_id > 25) %>%
  ggplot(aes(x = When, y = Rating)) +
  geom_point() +
  geom_smooth()
