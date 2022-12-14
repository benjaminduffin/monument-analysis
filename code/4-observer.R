# Header ------------------------------------------------------------------

# Compling data for monument litigation
# Observer data 
# BD 12/14/2022



# Libraries ---------------------------------------------------------------

library(writexl)
library(readxl)
library(dplyr)
library(stringr)
library(here)
library(sf)
library(ggplot2)
library(leaflet)
library(tidyr)


# Read Data  --------------------------------------------------------------

# monument 
monument <- st_read(here::here("data", "Northeast_Canyons_and_Seamounts_Marine_National_Monument", 
                               "Northeast_Canyons_and_Seamounts_Marine_National_Monument.shp"))

# observer trips, hauls, animals 
obs_files <- list.files(here::here("data"), 
                        full.names = T)
# list the files 
obs_files <- obs_files[grepl("obs_", obs_files)]

# read them in and name list elements 
obs_list <- lapply(obs_files, function(x) read.csv(x, stringsAsFactors = F))
names(obs_list) <- c("animal", "haul", "trip")

# bring to env
list2env(obs_list, .GlobalEnv)

# clean up 
rm(obs_list, obs_files)



# Preprocessing -----------------------------------------------------------
hemi_cols <- names(haul)[grepl("HEMISPHERE", names(haul))]
# is there a decimal degrees field? negative
loc_cols <- names(haul)[grepl("LAT|LON", names(haul))]

head(haul[, loc_cols])
# check NA
lapply(haul[, loc_cols], function(x) table(is.na(x)))


# all N, all W? yes
lapply(haul[, hemi_cols],  table)


haul$BEGIN_SET_LATITUDE + haul$BEGIN_SET_LATITUDE_MINUTES / 60

dd_convert <- function(deg, mins) {
  if (any(grepl("LAT", substitute(deg)))) {
    x <- deg + mins/60
    return(x)
    
  } else if (any(grepl("LONG", substitute(deg)))) {
    
    
    x <- (deg + (min/60))*-1
    return(x)
  }
}


dd_convert(haul$BEGIN_HAUL_LATITUDE, haul$BEGIN_HAUL_LATITUDE_MINUTES)  
dd_convert(haul$END_SET_LONGITUDE, haul$END_SET_LONGITUDE_MINUTES)  

(!is.na(haul$END_SET_LONGITUDE + haul$END_SET_LONGITUDE_MINUTES / 60) * -1)

# convert to dd
haul_sf <- haul %>%
  mutate(begin_set_lat_dd = BEGIN_SET_LATITUDE + BEGIN_SET_LATITUDE_MINUTES / 60, 
         begin_set_lon_dd = (BEGIN_SET_LONGITUDE + BEGIN_SET_LONGITUDE_MINUTES / 60) * -1, 
         end_set_lat_dd = END_SET_LATITUDE + END_SET_LATITUDE / 60, 
         end_set_lon_dd = (END_SET_LONGITUDE + END_SET_LONGITUDE_MINUTES / 60) * -1, 
         begin_haul_lat_dd = BEGIN_HAUL_LATITUDE + BEGIN_HAUL_LATITUDE_MINUTES / 60, 
         begin_haul_lon_dd = (BEGIN_HAUL_LONGITUDE + BEGIN_HAUL_LONGITUDE_MINUTES / 60)*-1, 
         end_haul_lat_dd = END_HAUL_LATITUDE + END_HAUL_LATITUDE_MINUTES / 60, 
         end_haul_lon_dd = (END_HAUL_LONGITUDE + END_HAUL_LONGITUDE_MINUTES / 60) * -1, 
         # add in trip.haul
         trip.haul = paste0(TRIP_NUMBER, ".", HAUL_NUMBER)
         ) %>%
  select(trip.haul, begin_set_lat_dd, begin_set_lon_dd, end_set_lat_dd, end_set_lon_dd, 
         begin_haul_lat_dd, begin_haul_lon_dd, end_haul_lat_dd, end_haul_lon_dd)
set_sf <- haul_sf %>%
  select(trip.haul, begin_set_lat_dd, begin_set_lon_dd, end_set_lat_dd, end_set_lon_dd)

# convert to long sf object 
a <- haul_sf[, c(1, 8:9)]
b <- haul_sf[, c(1, 6:7)]
c <- haul_sf[, c(1, 4:5)]
d <- haul_sf[, c(1:3)]

# make a list of these 
loc_list <- list(d,c,b,a)
# variables for column indicating what they will be 
loc_type <- c("begin_set", "end_set", "begin_haul", "end_haul")

# look at data
lapply(loc_list, head)
# add new columns indicating what type it is 
t <- purrr::map2(loc_list, 
                 loc_type, 
                 ~cbind(.x, Location_Type = .y))

lapply(t, head)

l_colnames <- c("trip.haul", "lat_dd", "lon_dd", "location_type")


# change all the names 
t <- purrr::map(t, purrr::set_names, l_colnames)

# and bring them all together
locs <- bind_rows(t)

# convert to sf for intersect
locs_sf <-  locs %>% 
  drop_na() %>%
  st_as_sf(coords = c("lon_dd", "lat_dd"), 
           crs = 4326, 
           remove = F)

# quick plot 
plot(locs_sf$geometry) # good 



## Buffer the monument ##

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

monument_buff <- st_transform(buff_t_union, crs = 4326)



# Analysis ----------------------------------------------------------------

monument_obs_hauls <- st_intersection(locs_sf, monument_buff)
monument_obs_hauls_neg <- st_difference(locs_sf, monument_buff)

str_split_fixed(monument_obs_hauls$trip.haul, pattern = "\\.", n = 2)[,1]
# how many unique trips/sets 
# sets 
length(unique(monument_obs_hauls$trip.haul)) # 14
# trips 
length(unique(str_split_fixed(monument_obs_hauls$trip.haul, pattern = "\\.", n = 2)[,1])) # 2 trips


# how many outside? 
length(unique(monument_obs_hauls_neg$trip.haul)) # 653
# trips 
length(unique(str_split_fixed(monument_obs_hauls_neg$trip.haul, pattern = "\\.", n = 2)[,1])) # 40 trips
