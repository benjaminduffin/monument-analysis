# Header ------------------------------------------------------------------

# Compling data for monument litigation
# BD 12/2/2022


# Allocate space ----------------------------------------------------------

# options(java.parameters = "-Xmx1024m") # more space for ACCSP data - just subset in query

# Libraries ---------------------------------------------------------------

library(RJDBC)
library(dotenv)
library(keyring)
library(writexl)
library(readxl)
#library(plyr)
library(dplyr)
library(stringr)
library(here)

# File Structure ----------------------------------------------------------

# set up the file strucutre
dirs <- c("code", "data", "output")

for (i in 1:length(dirs)){
  if(dir.exists(dirs[i]) == FALSE){
    dir.create(dirs[i])
  }
}

## ADD DIRS TO GITIGNORE
# just new lines 
# data/
# output/
## ADD TO GITIGNORE
# just new lines 
# .env



# {dotenv} and {keyring} setup and secrets --------------------------------

# create a .env file with a value pair for the DB connection string 
# add a line after completed text 

## ADD TO GITIGNORE
# just new lines 
# .env

# load .env
load_dot_env(".env") # then access with Sys.getenv("HMS_EDEALER")


# keyring
# if not done previously, add credentials to Windows Credential Manager 
# with key_set_with_value()
keyring::key_list("SECPR")$username
keyring::key_get("SECPR", keyring::key_list("SECPR")$username)

Sys.getenv("SECPR")



# Establish Connection ----------------------------------------------------

## Make Connection to Database ##
# create driver object 
# point this to your local ojdbc8.jar file! 
jdbcDriver =JDBC(driverClass = "oracle.jdbc.OracleDriver",
                 classPath="C:/Users/benjamin.duffin/Desktop/sqldeveloper-20.4.1.407.0006-x64/sqldeveloper/jdbc/lib/ojdbc8.jar") # find driver dir


# Create connection to the database 
jdbConnection <- dbConnect(jdbcDriver, 
                           Sys.getenv("SECPR"), 
                           user = keyring::key_list("SECPR")$username, 
                           password = keyring::key_get("SECPR", keyring::key_list("SECPR")$username))





# Query Observer Data -----------------------------------------------------

## trips per year
trips_yr_qry <- "select * 
             from obs.v_pop_trip_log
             where trip_number in (
             select distinct(trip_number) from obs.v_pop_trip_summary where vessel_id = '1026595')"

trips_yr <- dbGetQuery(jdbConnection, trips_yr_qry)

haul_qry <- "select 
            HAUL_LOG_KEY, GEAR_LOG_KEY, HAUL_NUMBER, GEAR_CODE, WAS_HAUL_OBSERVED, WAS_CATCH,
            GEAR_CONDITION, GEAR_CONDITION_DESC, MAINLINE_LENGTH, BOTTOM_DEPTH_MINIMUM, 
            BOTTOM_DEPTH_MAXIMUM, HOOK_DEPTH_MINIMUM, HOOK_DEPTH_MAXIMUM, TARGET_SPECIES, 
            SOAK_DURATION, NUMBER_HOOKS_SET, BEGIN_SET_DATE, BEGIN_SET_TIME, END_SET_DATE, 
            END_SET_TIME, BEGIN_HAUL_DATE, BEGIN_HAUL_TIME, END_HAUL_DATE, END_HAUL_TIME, 
            BEGIN_SET_LATITUDE, BEGIN_SET_LATITUDE_MINUTES, BEGIN_SET_LATITUDE_HEMISPHERE, 
            END_SET_LATITUDE, END_SET_LATITUDE_MINUTES, END_SET_LATITUDE_HEMISPHERE, 
            BEGIN_HAUL_LATITUDE, BEGIN_HAUL_LATITUDE_MINUTES, BEGIN_HAUL_LATITUDE_HEMISPHERE, 
            END_HAUL_LATITUDE, END_HAUL_LATITUDE_MINUTES, END_HAUL_LATITUDE_HEMISPHERE,
            BEGIN_SET_LONGITUDE, BEGIN_SET_LONGITUDE_MINUTES, BEGIN_SET_LONGITUDE_HEMISPHERE,
            END_SET_LONGITUDE, END_SET_LONGITUDE_MINUTES, END_SET_LONGITUDE_HEMISPHERE, 
            BEGIN_HAUL_LONGITUDE, BEGIN_HAUL_LONGITUDE_MINUTES, BEGIN_HAUL_LONGITUDE_HEMISPHER,
            END_HAUL_LONGITUDE, END_HAUL_LONGITUDE_MINUTES, END_HAUL_LONGITUDE_HEMISPHERE,
            HAUL_DURATION, FISHING_AREA, IS_EXPERIMENT, TRIP_NUMBER
            from obs.v_pop_haul_log
            where trip_number in (
            select distinct(trip_number) from obs.v_pop_trip_summary where vessel_id = '1026595')"

hauls <- dbGetQuery(jdbConnection, haul_qry)

# now get the haul log keys we need - use this for the animal dataset 
u_hauls <- paste(unique(hauls$HAUL_LOG_KEY), collapse = ",")


animal_qry <- paste0("select 
                     ANIMAL_LOG_KEY, HAUL_LOG_KEY, CARCASS_TAG_NUMBER, ALPHA_SPECIES_CODE, 
                      SPECIES_NAME, LENGTH_MEASUREMENT_ONE, LENGTH_TYPE_ONE, LENGTH_MEASUREMENT_TAKEN_CODE,
                      LENGTH_MEASURE_TAKEN_CODE_D, DRESSED_WEIGHT, TAG_NUMBER, BOARDING_STATUS,
                      BOARDING_STATUS_DESC, KEPT_OR_RELEASED, KEPT_OR_RELEASED_DESC, ESTIMATED_WEIGHT,
                      TRIP_NUMBER, HAUL_NUMBER, BEGIN_HAUL_DATE
                     from obs.v_pop_animal_log
                     where haul_log_key in (", 
                     u_hauls, 
                     ")")

animals <- dbGetQuery(jdbConnection, animal_qry)                     
        

# save trips, hauls, animals
write.csv(trips_yr, here::here("data", paste0("obs_monument_trips", Sys.Date(), ".csv")))
write.csv(hauls, here::here("data", paste0("obs_monument_hauls", Sys.Date(), ".csv")))
write.csv(animals, here::here("data", paste0("obs_monument_animals", Sys.Date(), ".csv")))

# Logbook Data  -----------------------------------------------------------


