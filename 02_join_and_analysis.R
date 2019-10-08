library(tidyverse)
library(janitor)
library(lubridate)
library(summarytools)
library(tidyverse)
library(plotly)
library(widgetframe)
library(scales)
library(zoo)
options(scipen = 999)

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




#### CHARTS #### --------------------------------------------------


glimpse(prezresults2016)

nos_prezresults2016 <- prezresults2016 %>% 
  filter(for_impeachment == "NO") %>% 
  mutate(
    p16winningparty = case_when(
      p16winningparty == "D" ~ "Clinton",
      p16winningparty == "R" ~ "Trump"
    ),
    p16winningparty = as_factor(p16winningparty)
  )

nos_prezresults2016

## bar chart
p <- ggplot(data = nos_prezresults2016, aes(x = p16winningparty, y = n)) +
  geom_col()
p


d <- ggplot(data = nos_prezresults2016, aes(x = p16winningparty, y = n, fill = p16winningparty)) +
  geom_col() + coord_flip() + theme_minimal()

d

# The palette with grey:
cbPalette <- c("#1A6AFB", "#f5425a")
# To use for fills, add
# scale_fill_manual(values=cbPalette)

d2 <- d + labs(title="Holdouts by 2016 presidential result",
               # subtitle = "A subtitle",
               caption = "Source: Census, Election Data, CNN analysis",
               x ="", y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=cbPalette) +
  # theme(legend.title=element_blank()) +
  theme(legend.position = "none") +
  scale_y_continuous(breaks= pretty_breaks()) #this makes it just whole numbers on the y axis

d2

dd <- ggplotly(d2) 

dd_nomenu <- dd %>% config(displayModeBar = FALSE)
dd_nomenu

#save as embeddable format
# htmlwidgets::saveWidget(frameableWidget(dd), 'demtopzip_plt.html')
htmlwidgets::saveWidget(frameableWidget(dd_nomenu), 'chart_impeachholdouts_byprez16.html')



# GDP

glimpse(gdp)

nos_gdp <- gdp %>% 
  filter(for_impeachment == "NO") %>% 
  mutate(
    gdp_above_national = as_factor(gdp_above_national)
  )

nos_gdp

## bar chart
d <- ggplot(data = nos_gdp, aes(x = gdp_above_national, y = n, fill = gdp_above_national)) +
  geom_col() + coord_flip() + theme_minimal()

d

# The palette:
cbPalette <- c("#748843", "#da9432")
# To use for fills, add
# scale_fill_manual(values=cbPalette)

d2 <- d + labs(title="Holdouts: GDP vs. national average",
               # subtitle = "A subtitle",
               caption = "CNN analysis",
               x ="", y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=cbPalette) +
  # theme(legend.title=element_blank()) +
  theme(legend.position = "none") +
  scale_y_continuous(breaks= pretty_breaks()) #this makes it just whole numbers on the y axis

d2

dd <- ggplotly(d2) 

dd_nomenu <- dd %>% config(displayModeBar = FALSE)
dd_nomenu

#save as embeddable format
# htmlwidgets::saveWidget(frameableWidget(dd), 'demtopzip_plt.html')

htmlwidgets::saveWidget(frameableWidget(dd_nomenu), 'chart_impeachholdouts_bygdp.html')


# Education

glimpse(college_degree)

nos_college_degree <- college_degree %>% 
  filter(for_impeachment == "NO") %>% 
  mutate(
    pct_bachelors_compared_to_national = as_factor(pct_bachelors_compared_to_national)
  )

nos_college_degree

## bar chart
d <- ggplot(data = nos_college_degree, aes(x = pct_bachelors_compared_to_national, y = n, fill = pct_bachelors_compared_to_national)) +
  geom_col() + coord_flip() + theme_minimal()

d

# The palette:
cbPalette <- c("#748843", "#da9432")
# To use for fills, add
# scale_fill_manual(values=cbPalette)

d2 <- d + labs(title="Holdouts: College education vs. national average",
               # subtitle = "A subtitle",
               caption = "CNN analysis",
               x ="", y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=cbPalette) +
  # theme(legend.title=element_blank()) +
  theme(legend.position = "none") +
  scale_y_continuous(breaks= pretty_breaks()) #this makes it just whole numbers on the y axis

d2

dd <- ggplotly(d2) 

dd_nomenu <- dd %>% config(displayModeBar = FALSE)
dd_nomenu

#save as embeddable format
# htmlwidgets::saveWidget(frameableWidget(dd), 'demtopzip_plt.html')

htmlwidgets::saveWidget(frameableWidget(dd_nomenu), 'chart_impeachholdouts_byeducation.html')



### Sept days - ALL

sept_daily_allyes

sept_daily_allyes <- sept_daily_allyes %>% 
  rename(date = date_exact)

## bar chart
d <- ggplot(data = sept_daily_allyes, aes(x = date, y = n)) +
  geom_col() + 
  theme_minimal()

d


d2 <- d + labs(title="Title",
               # subtitle = "A subtitle",
               caption = "CNN analysis",
               x ="", y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") #+
  # scale_y_continuous(breaks= pretty_breaks()) #this makes it just whole numbers on the y axis

d2

dd <- ggplotly(d2) 

dd_nomenu <- dd %>% config(displayModeBar = FALSE)
dd_nomenu


# htmlwidgets::saveWidget(frameableWidget(dd_nomenu), 'chart_yesonimpeachment_allsept.html')









