library(tidyverse)
library(janitor)
library(lubridate)
library(summarytools)

# run step 01 to download new version
# source("01_load_impeach.R")

# OR

# load saved file from previous step 01
impeachdata <- readRDS("processed_data/impeachdata.rds")


  
# we'll remove Pelosi since the speaker doesn't really cosponsor bills
#that will give us 234 total dems
# impeachdata <- impeachdata %>% 
#   filter(id != "P000197")


#### BRING IN THE CONGRESSIONAL DISTRICT PROFILE DATA #### -----------
workingtable <- readRDS("processed_data/workingtable.rds")

impeachdata <- impeachdata %>% 
  rename(GEOID = geoid)

#join
working_joined <- inner_join(impeachdata, workingtable, by = "GEOID")

#add flag columns for margin categorization
working_joined <- working_joined %>% 
  mutate(
    margin_flag = case_when(
      live_margin <= 5 ~ "5_points_or_less", 
      live_margin > 5 ~ "more_than_5_points", 
      TRUE ~ "more_than_5_points" #note there are four disticts where to Dems ran against each other; captured here
    )
  ) 

working_joined %>% 
  filter(margin_flag == "other") 

working_joined %>% nrow()

working_joined %>% names() 

working_joined <- working_joined %>% 
  select(for_impeachment,
           last_name,
           first_name,
           middle_name,
           party,
           state,
           district,
           date_exact,
           date_approx_month,
           date_comb,
           date_month,
           date_year,
           D_pct_2018 = live_D_pct,
           R_pct_2018 = live_R_pct,
           winner_2018 = live_winning,
           margin_2018 = live_margin,
           margin_flag,
           flip_2018 = flips,
           house_dist,
           keyrace_rating,
           population = totalpop,
           median_income = medincome,
           median_income_compared_to_national = medincome.abovebelow.natl,
           median_age = medage,
           median_age_compared_to_national = medage.abovebelow.natl,
           pct_nonwhite = pct.race.nonwhite,
           pct_nonwhite_compared_to_national = pct.race.nonwhite.abovebelow.natl,
           pct_bachelors = pct.ed.college.all,
           pct_bachelors_compared_to_national = pct.ed.college.all.abovebelow.natl,
           rural_pop_above20pct = pct.rural.above20,
           gdp_above_national = gdp_abovebelow_natlavg,
           clinton_percent,
           trump_percent,
           obama_percent,
           romney_percent,
           p16winningparty,
           p12winningparty,
           cnn_blurb,
           GEOID,
           fec_candidate_id,
           id
          )


working_joined %>% 
  count(for_impeachment)


#fill in NAs for impeachment with NO for ease of use
working_joined <- working_joined %>% 
  mutate(
    for_impeachment = case_when(
      for_impeachment == "YES" ~ for_impeachment,
      TRUE ~ "NO"
    )
  )

#add a measure for Trump margin over Hillary
working_joined <- working_joined %>% 
  mutate(
    trump_margin = trump_percent - clinton_percent
    )


#save results
writexl::write_xlsx(working_joined, "output/joined_impeachment.xlsx")
saveRDS(working_joined, "output/joined_impeachment.rds")



# 
# ### ANALYSIS ####

#for crosstabs using summarytools
# print(ctable(tobacco$smoker, tobacco$diseased, prop = "r"), method = "render")

# trump districts vs hillary
summarytools::ctable(working_joined$p16winningparty, working_joined$for_impeachment, prop = "r")

#education
ctable(working_joined$pct_bachelors_compared_to_national, working_joined$for_impeachment, prop = "r")
ctable(working_joined$pct_bachelors_compared_to_national, working_joined$for_impeachment, prop = "c")

#GDP
ctable(working_joined$gdp_above_national, working_joined$for_impeachment, prop = "r")
ctable(working_joined$gdp_above_national, working_joined$for_impeachment, prop = "c")
ctable(working_joined$gdp_above_national, working_joined$for_impeachment, prop = "n")


#income
ctable(working_joined$median_income_compared_to_national, working_joined$for_impeachment, prop = "r")
ctable(working_joined$median_income_compared_to_national, working_joined$for_impeachment, prop = "c")


#diversity
ctable(working_joined$pct_nonwhite_compared_to_national, working_joined$for_impeachment, prop = "r")
ctable(working_joined$pct_nonwhite_compared_to_national, working_joined$for_impeachment, prop = "c")

#rural
ctable(working_joined$rural_pop_above20pct, working_joined$for_impeachment, prop = "r")
ctable(working_joined$rural_pop_above20pct, working_joined$for_impeachment, prop = "c")

#margin flag
ctable(working_joined$margin_flag, working_joined$for_impeachment, prop = "r")
ctable(working_joined$margin_flag, working_joined$for_impeachment, prop = "c")

#top 10 by trump percentage of vote
top_trump_dists <- working_joined %>% 
  arrange(desc(trump_margin)) %>% 
  head(25)



# summarytools::tb()

glimpse(working_joined)


# groupings for export to spreadsheet for gfx ####

sept_daily_allyes <- working_joined %>% 
  filter(date_month == "9",
         date_year == "2019") %>% 
  count(date_exact) %>% 
  arrange(date_exact)

sept_daily_prezresults16 <- working_joined %>% 
  filter(date_month == "9",
         date_year == "2019") %>% 
  count(date_exact, p16winningparty) %>% 
  arrange(date_exact)

sept_daily_college <- working_joined %>% 
  filter(date_month == "9",
         date_year == "2019") %>% 
  count(date_exact, pct_bachelors_compared_to_national) %>% 
  arrange(date_exact)

sept_daily_gdp <- working_joined %>% 
  filter(date_month == "9",
         date_year == "2019") %>% 
  count(date_exact, gdp_above_national) %>% 
  arrange(date_exact)



prezresults2016 <- working_joined %>%
  count(p16winningparty, for_impeachment) %>% 
  arrange(for_impeachment)

gdp <- working_joined %>%
  count(gdp_above_national, for_impeachment) %>% 
  arrange(for_impeachment)

college_degree <- working_joined %>%
  count(pct_bachelors_compared_to_national, for_impeachment) %>% 
  arrange(for_impeachment)

nonwhite_pop <- working_joined %>%
  count(pct_nonwhite_compared_to_national, for_impeachment) %>% 
  arrange(for_impeachment)

rural_area <- working_joined %>%
  count(rural_pop_above20pct, for_impeachment) %>% 
  arrange(for_impeachment)

margin_5_or_less <- working_joined %>%
  count(margin_flag, for_impeachment) %>% 
  arrange(for_impeachment)

full_list_of_nos <- working_joined %>% 
  filter(for_impeachment == "NO")


#now make a list to feed to writexl
list_of_breakdowns <- list(prezresults2016 = prezresults2016,
                           gdp_vs_nationalavg = gdp,
                           college_vs_nationalavg = college_degree,
                           nonwhite_vs_nationalavg = nonwhite_pop,
                           rural_morethanfifth = rural_area,
                           margin_5_or_less = margin_5_or_less,
                           top_trump_dists = top_trump_dists,
                           sept_daily_allyes = sept_daily_allyes,
                           sept_daily_prezresults16 = sept_daily_prezresults16,
                           sept_daily_college = sept_daily_college,
                           sept_daily_gdp = sept_daily_gdp,
                           full_list_of_nos = full_list_of_nos
                          )

writexl::write_xlsx(list_of_breakdowns, "output/groupings_dems_impeachment.xlsx")


# 
# 
# working_joined %>%
#   filter(margin_flag == "5_points_or_less")
# 
# 
# ###
# 
# working_joined %>%
#   count(position)
# 
# working_joined %>%
#   count(p16winningparty)
# 
# working_joined %>%
#   count(keyrace_rating)
# 
# working_joined %>%
#   count(flips)
# 
# working_joined %>%
#   count(pct.ed.college.all.abovebelow.natl)
# 
# working_joined %>%
#   count(medincome.abovebelow.natl)
# 
# working_joined %>%
#   count(pct.race.nonwhite.abovebelow.natl)
# 
# 
# working_joined %>%
#   count(p16winningparty, pct.ed.college.all.abovebelow.natl)
# 
# 
