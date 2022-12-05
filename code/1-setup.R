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

# udp.v_hms_extraction_lh
  # filter on SET_YEAR, VESSEL_ID_NUMBER
v_hms_qry <- "select * 
              from udp.v_hms_extraction_lh
              where set_year > 2015 
              and vessel_id_number in 
              ('627357', '1275363', '615907', '1026595', '557860', 'NJ0746GN', '622027')"

logbook_v <- dbGetQuery(jdbConnection, v_hms_qry)

# so what vessels are there? 
table(logbook_v$VESSEL_ID_NUMBER)

# or we can pull directly from the UDP tables
udp_logbook_qry <- "select distinct
			      t.logbook_key,
			      t.batch_number,
            a.original_batch_sequence_number,
			      t.schedule_number,
			      t.vessel_id,
			      t.landing_date, 
            a.set_date, 
            a.begin_set_time, 
            a.begin_set_meridian, 
            a.end_set_time, 
            a.end_set_meridian,
            a.haulback_date, 
            a.begin_haulback_time, 
            a.begin_haulback_meridian, 
            a.end_haulback_time, 
            a.end_haulback_meridian,
            a.latitude_degrees, 
            a.latitude_minutes, 
            a.latitude_hemisphere, 
            a.longitude_degrees, 
            a.longitude_minutes, 
            a.LONGITUDE_HEMISPHERE, 
			      c.area_fished,
			      g.gear_code_nmfs, 
		        gc.gear_name, 
			      gc.gear_class, 
            c.species_code_nmfs, 
            sc.nmfs_common, 
            sc.scientific_name, 
            sc.species_itis,
            cd.number_of_individuals,
            c.total_whole_pounds, 
            cd.disposition_status
		from udp.fls_trips t
			left join udp.fls_fishing_activities a on t.logbook_key = a.logbook_key
			left join udp.fls_gears_fished g on a.fishing_activity_key = g.fishing_activity_key
			left join udp.fls_catches c on g.gear_fished_key = c.gear_fished_key 
			left join udp.fls_gear_codes_nmfs gc on g.gear_code_nmfs = gc.gear_code_nmfs
			left join udp.fls_gear_descriptions gd on g.gear_fished_key = gd.gear_fished_key
			left join udp.fls_gear_parameter_codes gp on gd.gear_parameter_code = gp.gear_parameter_code
			left join UDP.QC_VAL_TRIP qc on t.SCHEDULE_NUMBER = qc.SCHEDULE_NUMBER
            /* adidtional parameters looped in to include weights */
      left join udp.fls_species_codes_nmfs sc on (c.species_code_nmfs = sc.species_code_nmfs)
      left join udp.fls_catch_descriptions cd on (c.catch_key = cd.catch_key)
		where
			t.landing_date > to_date('31-DEC-2015','dd-mon-yyyy') 
			AND qc.DELETED_DATE IS NULL 
            and t.vessel_id in ('627357', '1275363', '615907', '1026595', '557860', 'NJ0746GN', '622027')
		order by t.batch_number, t.schedule_number"

logbook_t <- dbGetQuery(jdbConnection, udp_logbook_qry)

## quick look to check for consistency across these two data
# good - same vessels
table(logbook_t$VESSEL_ID)
table(logbook_v$VESSEL_ID_NUMBER)

# trips - good 
logbook_t %>% 
  group_by(VESSEL_ID) %>%
  summarize(trip_count = n_distinct(SCHEDULE_NUMBER))

logbook_v %>%
  group_by(VESSEL_ID_NUMBER) %>%
  summarize(trip_count = n_distinct(TRIP_NUMBER))

# sets - good 
logbook_t %>% 
  group_by(VESSEL_ID) %>%
  summarize(trip_count = n_distinct(ORIGINAL_BATCH_SEQUENCE_NUMBER))

logbook_v %>%
  group_by(VESSEL_ID_NUMBER) %>%
  summarize(trip_count = n_distinct(BATCH_SEQUENCE_NUMBER))


# write these files 
write.csv(logbook_t, here::here("data", paste0("logbook_monument_tabs_", Sys.Date(), ".csv")))
write.csv(logbook_v, here::here("data", paste0("logbook_monument_viewExtract", Sys.Date(), ".csv")))
