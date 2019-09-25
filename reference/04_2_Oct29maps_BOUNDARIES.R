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
library(rmapshaper)
library(tmap)
library(tmaptools)
library(gdalUtils)

options(tigris_class = "sf")


#### load and join data ####

#load saved data file from Step 02
alldistricts_FINAL <- readRDS("alldistricts_FINAL.rds")

# cd <- congressional_districts(cb = TRUE, resolution = "20m")
cd <- congressional_districts(year = "2018")  #new PA lines

#"simplify" file to reduce file size
cd1 <- ms_simplify(cd, keep = 0.1)

#water layer
water <- st_read("geodata/USA_Water_Bodies/USA_Water_Bodies.shp")
water1 <- ms_simplify(water, keep = 0.1)
#just keep MI and WI
water_selected <- water1 %>% 
  filter(STATE %in% c("MI", "WI", "MN"))

#join census data to geography 
mymap <- inner_join(cd1, alldistricts_FINAL)

#change/add these for use later
mymap$display.education <- mymap$pct.ed.college.all.abovebelow.natl 

#remove AK and HI
mymap <- mymap %>% 
  filter(state.name != "Alaska",
         state.name != "Hawaii")



#rename for easy of use
data <- mymap


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

#count for education
rheld_demtossorbetter %>% 
  count(pct.ed.college.all.abovebelow.natl)

rheld_favorgop %>% 
  count(pct.ed.college.all.abovebelow.natl)

#count for median income
rheld_demtossorbetter %>% 
  count(medincome.abovebelow.natl)

rheld_favorgop %>% 
  count(medincome.abovebelow.natl)



### TMAP ####

mymap_test <- tm_shape(data) +
  tm_polygons(gdp_abovebelow_natlavg, id = "house_dist") 





+
  tm_shape(rheld_demtossorbetter) +
  tm_polygons("display.education", id = "house_dist") +
  tm_shape(water1) +
  tm_polygons(col = "blue", id = "NAME")

mymap_test

tmap_save(mymap_test, "map_rheld_education_demfavored.pdf")

#
mymap_test <- tm_shape(data) +
  tm_polygons(id = "house_dist") +
  tm_shape(rheld_favorgop) +
  tm_polygons("display.education", id = "house_dist") 

# mymap_test
# mymap$medincome.abovebelow.natl

tmap_save(mymap_test, "map_rheld_education_gopfavored.pdf")

#
mymap_test <- tm_shape(data) +
  tm_polygons(id = "house_dist") +
  tm_shape(rheld_demtossorbetter) +
  tm_polygons("medincome.abovebelow.natl", id = "house_dist") 

# mymap_test

tmap_save(mymap_test, "map_rheld_medincome_demfavored.pdf")


#
mymap_test <- tm_shape(data) +
  tm_polygons(id = "house_dist") +
  tm_shape(rheld_favorgop) +
  tm_polygons("medincome.abovebelow.natl", id = "house_dist") 

# mymap_test

tmap_save(mymap_test, "map_rheld_medincome_gopfavored.pdf")



#
mymap_test <- tm_shape(data) +
  tm_polygons(id = "house_dist") +
  tm_shape(rheld_demtossorbetter) +
  tm_polygons("pct.race.nonwhite.abovebelow.natl", id = "house_dist") 

# mymap_test

tmap_save(mymap_test, "map_rheld_nonwhite_demfavored.pdf")


#
mymap_test <- tm_shape(data) +
  tm_polygons(id = "house_dist") +
  tm_shape(rheld_favorgop) +
  tm_polygons("pct.race.nonwhite.abovebelow.natl", id = "house_dist") 

# mymap_test

tmap_save(mymap_test, "map_rheld_nonwhite_gopfavored.pdf")


