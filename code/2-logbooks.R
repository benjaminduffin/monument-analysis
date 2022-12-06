# Header ------------------------------------------------------------------

# Compling data for monument litigation
# Logbooks for select vessels 
# BD 12/5/2022



# Libraries ---------------------------------------------------------------

library(writexl)
library(readxl)
library(dplyr)
library(stringr)
library(here)
library(sf) 
library(ggplot2)
library(leaflet)


# Load Data ---------------------------------------------------------------

# need to load the logbook and the spatial file of the monument 
monument <- st_read(here::here("data", "Northeast_Canyons_and_Seamounts_Marine_National_Monument", 
                               "Northeast_Canyons_and_Seamounts_Marine_National_Monument.shp"), 
                    )
# quick plot 
plot(monument$geometry)

# load the logbook data 
logbook <- read.csv(here::here("data", "logbook_monument_tabs_2022-12-05.csv"), stringsAsFactors = F)


# Preprocessing -----------------------------------------------------------

## need to buffer the monument data ##

# check current crs 
st_crs(monument)
st_crs(monument)$units # null, need to set crs
st_crs(monument) <- 4269

# convert to non-dd projection - currently 4269
#equidistanct conic contiguous USA: 102005
monument_102005 <- st_transform(monument, crs = "ESRI:102005")

# buffer using 50 miles 
buff_t <- st_buffer(monument_102005, 
                    dist = 80467)

# union the buffer region
buff_t_union <- st_union(buff_t)

# plot to check for goodness
ggplot() + 
  geom_sf(data = buff_t_union, fill = "red", alpha = 0.5) +
  geom_sf(data = monument_102005, fill = "green", alpha = 0.2) 
  
# convert back to 4269
monument_buff <- st_transform(buff_t_union, crs = 4326)

leaflet() %>%
  addTiles() %>%
  addMeasure(primaryLengthUnit = "meters") %>%
  addPolygons(data = monument_buff) %>%
  addPolygons(data = monument)

## get set level logbook data to provide to Ian 

head(logbook)

# need to generate spatial data we can use for the logbook data 
table(logbook$LATITUDE_HEMISPHERE) # all N 
table(logbook$LONGITUDE_HEMISPHERE) # all W 

logbook$lat_dd <- logbook$LATITUDE_DEGREES + (logbook$LATITUDE_MINUTES / 60)
logbook$lon_dd <- (logbook$LONGITUDE_DEGREES + (logbook$LONGITUDE_MINUTES / 60)) * -1
  
# how many sets should we have? 967
length(unique(logbook$ORIGINAL_BATCH_SEQUENCE_NUMBER))

# set level data 
logbook_sets <- logbook %>% 
  select(-c(X, NMFS_COMMON, SPECIES_CODE_NMFS, SCIENTIFIC_NAME, SPECIES_ITIS, NUMBER_OF_INDIVIDUALS, 
            TOTAL_WHOLE_POUNDS, DISPOSITION_STATUS, AREA_FISHED)) %>%
  distinct()


# Comparing Sets to Areas -------------------------------------------------
# logbook data 
# need to convert the logbook data to a spatial object - set crs to 4326
lb_sets_sf <- st_as_sf(logbook_sets, 
                       coords = c("lon_dd", "lat_dd"), 
                       crs = 4326, 
                       remove = F)
plot(lb_sets_sf$geometry) # good 

ggplot() + 
  geom_sf(data = monument_buff) + 
  geom_sf(data = lb_sets_sf)

# now, intersect the data to pull out those sets withing "range" of the monument
monument_lb <- st_intersection(lb_sets_sf, monument_buff)
monument_lb_neg <- st_difference(lb_sets_sf, monument_buff)

# and also those that are strictly int he monument area 
monument_4326 <- st_transform(monument, 4326)
monument_lb_nobuff <- st_intersection(lb_sets_sf, monument_4326)

# quick plot 
ggplot() + 
  geom_sf(data = monument_buff) + 
  geom_sf(data = monument_lb, color = "red") + 
  geom_sf(data = monument_lb_neg, color = "blue")

# create accompanying leaflet htmlwidget
map <- leaflet() %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  addPolygons(data = monument_buff) %>%
  addPolygons(data = monument, 
              fillOpacity = 0.3, 
              fillColor = "yellow", 
              stroke = F) %>%
  addCircleMarkers(lng = monument_lb$lon_dd, lat = monument_lb$lat_dd, 
                   radius = 3, 
                   color = "green", 
                   label = paste("Vessel ID:", monument_lb$VESSEL_ID, "; Set Date:", monument_lb$SET_DATE)) %>%
  addCircleMarkers(lng = monument_lb_neg$lon_dd, lat = monument_lb_neg$lat_dd, 
                   radius = 3, 
                   color = "red", 
                   label = paste("Vessel ID:", monument_lb_neg$VESSEL_ID, "; Set Date:", monument_lb_neg$SET_DATE))
map
# save 
htmlwidgets::saveWidget(map, 
                        here::here("output", paste0("logbook_monument_map_", Sys.Date(), ".html")), 
                        selfcontained = T)

## Postiive sets output ##
# output positive sets 
write.csv(monument_lb, 
          here::here("output", paste0("logbook_sets_monument_buffer_", Sys.Date(), ".csv")), 
          row.names = F)
# output as xlsx
write_xlsx(monument_lb, 
           here::here("output", paste0("logbook_sets_monument_buffer_", Sys.Date(), ".xlsx")))
# only those in the monument area 
write_xlsx(monument_lb_nobuff, 
           here::here("output", paste0("logbook_sets_monument_noBuffer_", Sys.Date(), ".xlsx")))


