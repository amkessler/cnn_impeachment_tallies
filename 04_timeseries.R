library(tidyverse)
library(lubridate)
library(tibbletime)


working_joined <- readRDS("output/joined_impeachment.rds")

sept_daily_allyes <- working_joined %>% 
  filter(date_month == "9",
         date_year == "2019") %>% 
  count(date_exact) %>% 
  arrange(date_exact)


# some playing around with tibbletime 
# https://business-science.github.io/tibbletime/articles/TT-04-use-with-dplyr.html

series <- create_series('2013' ~ '2017', 'daily', class = "Date") %>%
  mutate(var = rnorm(n()))

series

#traditional through group by
series %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(year, month) %>%
  summarise(mean_var = mean(var))


# With tibbletime, rather than creating new columns to group on, you manipulate your original date column 
# into something that corresponds to the period you are summarising at. 
# The tibbletime way to do this is with collapse_by().

series %>%
  collapse_by("monthly") %>%
  group_by(date) %>%
  summarise(mean_var = mean(var))

