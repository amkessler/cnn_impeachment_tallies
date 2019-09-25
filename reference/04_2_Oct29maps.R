# Maps for Ron's column:
# --Districts with above/below education levels.  Also above/below median income.
# --Republican-held CDs that are toss-up or better for Democrats
# --Republican-held CDs that are lean/likely Republican

library(tidyverse)
library(tigris)
library(sf)
library(leaflet)
library(tidycensus)
library(reshape2)
library(janitor)
library(htmltools)
library(widgetframe)
library(sp)
library(raster)
library(mapview)
options(tigris_class = "sf")


#### load and join data ####

#load saved data file from Step 02
alldistricts_FINAL <- readRDS("alldistricts_FINAL.rds")

names(alldistricts_FINAL)
head(alldistricts_FINAL)

#import geo table with CD points
cd_points <- read_csv("geodata/CD116_points.csv")
colnames(cd_points) <- c("state", "GEOID", "lat", "lon")
head(cd_points)

#join tables
districts_wpoints <- inner_join(alldistricts_FINAL, cd_points)



#### build filtered tables for the maps ####
# --Districts with above/below education levels.  Also above/below median income.
#     --Republican-held CDs that are toss-up or better for Democrats
#     --Republican-held CDs that are lean/likely Republican

#rename for easy of use
data <- districts_wpoints

#change/add these for use later
data$display.education <- data$pct.ed.college.all.abovebelow.natl 
data$display.edrate <- round_half_up(data$pct.ed.college.all, 1)
data$pct.ed.college.all.abovebelow.natl <- as.factor(data$pct.ed.college.all.abovebelow.natl)


rheld_allkey <- data %>% 
  filter(current_party == "R",
         keyrace_rating %in% c("likely democratic", "lean democratic", "tossup",
                               "likely republican", "lean republican"))

rheld_demtossorbetter <- data %>% 
  filter(current_party == "R",
         keyrace_rating %in% c("likely democratic", "lean democratic", "tossup"))

rheld_favorgop <- data %>% 
  filter(current_party == "R",
         keyrace_rating %in% c("likely republican", "lean republican"))

#count for education ####
rheld_demtossorbetter %>% 
  count(pct.ed.college.all.abovebelow.natl)

rheld_favorgop %>% 
  count(pct.ed.college.all.abovebelow.natl)

#count for median income ####
rheld_demtossorbetter %>% 
  count(medincome.abovebelow.natl)

rheld_favorgop %>% 
  count(medincome.abovebelow.natl)

#count for non-white
rheld_demtossorbetter %>% 
  count(pct.race.nonwhite.abovebelow.natl)

rheld_favorgop %>% 
  count(pct.race.nonwhite.abovebelow.natl)


### MAPPING - BUBBLE MAPS with LEAFLET ####

### MAP1 ####
#education levels - only dem favored or tossups

#labels
labs <- lapply(seq(nrow(rheld_demtossorbetter)), function(i) {
  paste0( '<p><strong>', rheld_demtossorbetter[i, "house_dist"], '</strong></p>',
          '<p></p>', 
          "Four-year degree percent is ", rheld_demtossorbetter[i, "display.education"], " national average" 
          # '<p></p>', 
          # districts_wpoints[i, "cmag_r_spotcnt"]
  ) 
})

#colors
factpal <- colorFactor(c("darkgreen","orange"), rheld_demtossorbetter$pct.ed.college.all.abovebelow.natl)

#leaflet circle map
demadvantage_ed <- leaflet(rheld_demtossorbetter) %>% 
  addProviderTiles(providers$Stamen.TonerLite) %>%
  # addTiles() %>%
  addCircleMarkers(lng = ~lon,
                   lat = ~lat,
                   radius = 7,
                   color = ~factpal(pct.ed.college.all.abovebelow.natl),
                   label = lapply(labs, HTML)
  ) %>%
  addControl("GOP-held districts favoring Dems or tossups - College pct vs. US average", position = "topright") %>% 
  addLegend("bottomright", pal = factpal, values = ~pct.ed.college.all.abovebelow.natl,
            title = "",
            opacity = 1
  )


demadvantage_ed

#save to frameable file for CMS
htmlwidgets::saveWidget(frameableWidget(demadvantage_ed),'map_demadvantage_education.html')



### MAP2 ####
## rheld_favorgop ####

#labels
labs <- lapply(seq(nrow(rheld_favorgop)), function(i) {
  paste0( '<p><strong>', rheld_favorgop[i, "house_dist"], '</strong></p>',
          '<p></p>', 
          "Four-year degree percent is ", rheld_favorgop[i, "display.education"], " national average" 
          # '<p></p>', 
          # districts_wpoints[i, "cmag_r_spotcnt"]
  ) 
})

#colors
factpal <- colorFactor(c("darkgreen","orange"), rheld_favorgop$pct.ed.college.all.abovebelow.natl)

#leaflet circle map
gopadvantage_ed <- leaflet(rheld_favorgop) %>% 
  addProviderTiles(providers$Stamen.TonerLite) %>%
  # addTiles() %>%
  addCircleMarkers(lng = ~lon,
                   lat = ~lat,
                   radius = 7,
                   color = ~factpal(pct.ed.college.all.abovebelow.natl),
                   label = lapply(labs, HTML)
  ) %>%
  addControl("GOP-held districts rated likely/lean Republican - College pct vs. US average", position = "topright") %>% 
  addLegend("bottomright", pal = factpal, values = ~pct.ed.college.all.abovebelow.natl,
            title = "",
            opacity = 1
  )


gopadvantage_ed

#save to frameable file for CMS
htmlwidgets::saveWidget(frameableWidget(gopadvantage_ed),'map_gopadvantage_education.html')







# m1 <- leaflet(zip_map) %>% 
#   addTiles() %>%
#   addCircles(lng = ~lon, lat = ~lat, weight = .4,
#              stroke = FALSE, fillOpacity = .25,
#              radius = ~sqrt(advantage) * 300, 
#              fillColor = ~factpal(winner),
#              label = lapply(labs1, HTML)
#   ) %>%
#   addControl("RNC/NRCC vs. DNC/DCCC - Sept. individual contributions by zip code", position = "topright") 




##########################################################
# 
# #labels
# labs2 <- lapply(seq(nrow(districts_wpoints)), function(i) {
#   paste0( '<p><strong>', districts_wpoints[i, "house_dist"], '</strong></p>',
#           '<p></p>', 
#           "TV ads aired: ", districts_wpoints[i, "cmag_r_spotcnt"] 
#           # '<p></p>', 
#           # districts_wpoints[i, "cmag_r_spotcnt"]
#   ) 
# })
# 
# rep_ads_map <- leaflet(districts_wpoints) %>% 
#   # addProviderTiles(providers$Stamen.TonerLite) %>% 
#   addTiles() %>%
#   addCircles(lng = ~lon, lat = ~lat, weight = 1,
#              radius = ~sqrt(cmag_r_spotcnt) * 2000, 
#              # fillColor = ~pal(cmag_d_spotcnt),
#              fillColor = "red",
#              label = lapply(labs2, HTML)
#   ) %>%
#   setView(-96, 37.8, zoom=4) 
# 
# rep_ads_map
# 
# 
# 
# ### mapping points using the winning RATIO instead of raw counts ####
# 
# #first we must remove the fair outliers -- so let's only look at districts where each party 
# #had more than 10 ads
# districts_mapratios <- districts_wpoints %>% 
#   filter(cmag_d_spotcnt > 10,
#          cmag_r_spotcnt > 10)
# 
# 
# #labels
# labs3 <- lapply(seq(nrow(districts_mapratios)), function(i) {
#   paste0( '<p><strong>', districts_mapratios[i, "house_dist"], '</strong></p>',
#           '<p></p>', 
#           "Democratic ads aired: ", districts_mapratios[i, "cmag_d_spotcnt"], 
#           '<p></p>',
#           "Republican ads aired: ", districts_mapratios[i, "cmag_r_spotcnt"]
#   ) 
# })
# 
# dem_ads_ratio_map <- leaflet(districts_mapratios) %>% 
#   # addProviderTiles(providers$Stamen.TonerLite) %>% 
#   addTiles() %>%
#   addCircles(lng = ~lon, lat = ~lat, weight = 1,
#              radius = ~cmag_d_ratio * 10000, 
#              # fillColor = ~pal(cmag_d_spotcnt),
#              label = lapply(labs3, HTML)
#   ) %>%
#   setView(-96, 37.8, zoom=4) 
# 
# dem_ads_ratio_map
# 
# 
# 
# 
# 
# 
# #***************************************************************************************
# ### looking at just lean and tossup key races ####
# 
# districts_leantossup <- districts_wpoints %>% 
#   filter(keyrace_rating %in% c("lean republican", "lean democratic", "tossup"))
# 
# districts_leantossup$keyrace_rating <- str_to_title(districts_leantossup$keyrace_rating)
# 
# 
# 
# #labels
# labs_leantossup <- lapply(seq(nrow(districts_leantossup)), function(i) {
#   paste0( '<p><strong>', districts_leantossup[i, "house_dist"], ' (', districts_leantossup[i, "keyrace_rating"], ')', '</strong></p>',
#           '<p></p>', 
#           "Democratic ads aired: ", districts_leantossup[i, "cmag_d_spotcnt"], 
#           '<p></p>',
#           "Republican ads aired: ", districts_leantossup[i, "cmag_r_spotcnt"]
#   ) 
# })
# 
# m1 <- leaflet(districts_leantossup) %>% 
#   addTiles() %>%
#   addCircles(lng = ~lon, lat = ~lat, weight = 1,
#              radius = ~sqrt(cmag_d_spotcnt) * 3000, 
#              # fillColor = ~pal(cmag_d_spotcnt),
#              label = lapply(labs_leantossup, HTML)
#   ) %>%
#   addControl("Democratic Ads - Lean/Tossup Districts - Aired Oct 9-15", position = "topright") 
# # %>% 
# #   setView(-96, 37.8, zoom=4) 
# 
# m1
# 
# 
# m2 <- leaflet(districts_leantossup) %>% 
#   addTiles() %>%
#   # addProviderTiles(providers$Stamen.TonerLite) %>%
#   addCircles(lng = ~lon, lat = ~lat, weight = 1,
#              radius = ~sqrt(cmag_r_spotcnt) * 3000, 
#              # fillColor = ~pal(cmag_d_spotcnt),
#              fillColor = "red",
#              label = lapply(labs_leantossup, HTML)
#   ) %>%
#   addControl("Republican Ads - Lean/Tossup Districts - Aired Oct 9-15", position = "topright") 
# # %>% 
# #   setView(-96, 37.8, zoom=4) 
# 
# m2

#how to get side-by-side maps with mapview package
# https://r-spatial.github.io/mapview/reference/latticeView.html

# latticeView(m1, m2, ncol = 1)
# sync(m1, m2)


#save to frameable file for CMS
# htmlwidgets::saveWidget(frameableWidget(m1),'cmag_weekofoct9_dem.html')
# htmlwidgets::saveWidget(frameableWidget(m2),'cmag_weekofoct9_gop.html')

