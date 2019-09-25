library(tigris)
library(sf)
library(ggplot2)
library(leaflet)
library(tmap)
library(tmaptools)
library(dplyr)
library(tidycensus)
library(purrr)
library(reshape2)
library(stringr)
library(janitor)
library(htmltools)
library(rmapshaper)
options(tigris_class = "sf")


#load saved data file from Step 01
alldistricts_FINAL <- readRDS("alldistricts_FINAL.rds")


#pull geo from tigris
#for national map, 20m likely better here. For single states, 5m:

# cd <- congressional_districts(cb = TRUE, resolution = "20m")
cd <- congressional_districts(year = "2018")  #new PA lines

#"simplify" file to reduce file size
cd1 <- ms_simplify(cd, keep = 0.1)


#join census data to geography 
mymap <- inner_join(cd1, alldistricts_FINAL)

#change/add these for use later
mymap$display.education <- mymap$pct.ed.college.all.abovebelow.natl 
mymap$pct.ed.college.all.abovebelow.natl <- as.factor(data$pct.ed.college.all.abovebelow.natl)



#remove AK and HI
mymap <- mymap %>% 
  filter(state.name != "Alaska",
         state.name != "Hawaii")


### TMAP ####

mymap_test <-  tm_shape(mymap) +
  tm_polygons("display.education", id = "house_dist") 

mymap_test




#### LEAFLET ####


#create test map layer
leaflet(mymap) %>% 
  addTiles() %>% 
  addPolygons(color = "#444444") %>%
  setView(-96, 37.8, zoom=4)


### mapping ####

#palette
pal <- colorNumeric(
  palette = "Greens",
  domain = mymap$pct.ed.college.all)

#labels
labs <- lapply(seq(nrow(mymap)), function(i) {
  paste0( '<p><strong>', mymap[i, "house_dist"], '</strong></p>',
          '<p></p>',
          "Pct with 4-year degree: ", mymap[i, "pct.ed.college.all"]
          # # '<p></p>', 
          # districts_wpoints[i, "cmag_r_spotcnt"]
  ) 
})

labs$Value[1]

#make map
leaflet(mymap) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(color = "#444444", weight = .5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.9,
              fillColor = ~pal(pct.ed.college.all),
              label = lapply(labs, HTML),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend("bottomright", pal = pal, values = ~pct.ed.college.all,
            title = "Pct with Bachelor's or Higher",
            opacity = 1) 



#make map - key races
pal <- colorFactor(
  palette = c("#6284d9","#e76d6f","#4152ad","#b64f49","#3f842b"),
  domain = mymap$keyrace_rating)

leaflet(mymap) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(color = "#444444", weight = .5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.9,
              fillColor = ~pal(keyrace_rating),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend("bottomright", pal = pal, values = ~keyrace_rating,
            title = "Key Race Ratings",
            opacity = 1) %>% 
  setView(-96, 37.8, zoom=4)

