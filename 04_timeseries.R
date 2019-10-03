library(tidyverse)
library(lubridate)
library(tibbletime)


working_joined <- readRDS("output/joined_impeachment.rds")

sept_daily_allyes <- working_joined %>% 
  filter(date_month == "9",
         date_year == "2019") %>% 
  count(date_exact) %>% 
  arrange(date_exact)

