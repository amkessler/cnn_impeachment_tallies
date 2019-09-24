library(tidyverse)
library(lubridate)
library(janitor)
library(googlesheets)


# import data from Google Sheet 
mykey <- Sys.getenv("IMPEACH_LIST_KEY")

# gs_ls()

impeachsheet <- gs_key(mykey)

#read in data from the trump false claims tab
impeachdata <- impeachsheet %>% 
  gs_read(ws = "housedems") 

glimpse(impeachdata)

#clean column names
impeachdata <- impeachdata %>%
  clean_names()

#format date columns
impeachdata$date_exact <- mdy(impeachdata$date_exact)
impeachdata$date_approx_proxydate <- mdy(impeachdata$date_approx_proxydate)

#combined date column to gather exact and approximate for month derivations
impeachdata <- impeachdata %>% 
  mutate(
    date_comb = if_else(is.na(date_exact), date_approx_proxydate, date_exact),
    date_month = month(date_comb),
    date_year = year(date_comb)
  )

# save results
saveRDS(impeachdata, "processed_data/impeachdata.rds")

