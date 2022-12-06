# Header ------------------------------------------------------------------

# Compling data for monument litigation
# BFT and eDealer landings 
# BD 12/5/2022

# this script indentifies which dealer data corresponds to trips with effort 
# inside the monument buffer zone

# Libraries ---------------------------------------------------------------

library(writexl)
library(readxl)
library(dplyr)
library(stringr)
library(here)
library(sf) 
library(ggplot2)
library(leaflet)
library(googlesheets4)
library(googledrive)


# Load Data ---------------------------------------------------------------

# need to load the logbook and the spatial file of the monument 
monument <- st_read(here::here("data", "Northeast_Canyons_and_Seamounts_Marine_National_Monument", 
                               "Northeast_Canyons_and_Seamounts_Marine_National_Monument.shp"), 
)


# load the logbook - inside the buffer
lb_mon <- read_xlsx(here::here("output", "logbook_sets_monument_buffer_2022-12-05.xlsx"))
# all logbooks
logbook <- read.csv(here::here("data", "logbook_monument_tabs_2022-12-05.csv"), stringsAsFactors = F)

# read in the eDealer data 
edlr <- read_sheet("https://docs.google.com/spreadsheets/d/1LNu4Yzezguc5V_t4Irzo6bTddzceznDxHZpN8ir4cU4/edit?usp=sharing")
# read in the BFT data 
# commented out, saved locally
# drive_download(file = as_id("https://docs.google.com/spreadsheets/d/1wgfmPv2kWBDFBl0aVeLK3LuwHbzeMujM/edit?usp=sharing&ouid=100135471101551472212&rtpof=true&sd=true"), 
#                       path = here::here("data", paste0("bft_monument_", Sys.Date(), ".xlsx")), 
#                       type = "xlsx")
bft <- read_xlsx(here::here("data", "bft_monument_2022-12-05.xlsx"), 
                 sheet = 1)



# Identifying Potential Monument Landings ---------------------------------

# Logbook - need trip level unique vessel, landing dates
glimpse(lb_mon)

# pull out just the trip info we need
lb_mon_sub <- lb_mon %>%
  select(VESSEL_ID, LANDING_DATE, SCHEDULE_NUMBER) %>%
  distinct()


### EDEALER ###

# also from the edealer data 
glimpse(edlr)

edlr_sub <- edlr %>%
  select(SUPPLIER_VESSEL_ID, DATE_LANDED, CORPORATE_NAME, unique_report_no, TRIP_ID) %>%
  distinct()

# check for direct matches
table(paste0(lb_mon_sub$VESSEL_ID, ".", lb_mon_sub$LANDING_DATE) %in%
        paste0(edlr_sub$SUPPLIER_VESSEL_ID, ".", edlr_sub$DATE_LANDED)) # all but 1

table(lb_mon_sub$SCHEDULE_NUMBER %in% edlr_sub$TRIP_ID) # also all but 1

# which one? 
lb_mon_sub <- lb_mon_sub %>%
  mutate(sch_in_edlr = ifelse(SCHEDULE_NUMBER %in% unique(edlr_sub$TRIP_ID), "x", NA), 
         ves.date_in_edlr = ifelse(paste0(VESSEL_ID, ".", LANDING_DATE) %in% 
                                     paste0(edlr_sub$SUPPLIER_VESSEL_ID, ".", edlr_sub$DATE_LANDED), 
                                   "x", NA))

lb_mon_sub

# so we have all trips covered, but not all the same
# create dummy for in the monument
edlr <- edlr %>%
  mutate(mon_activity = ifelse(paste0(SUPPLIER_VESSEL_ID, ".", DATE_LANDED) %in% 
                                 paste0(lb_mon_sub$VESSEL_ID, ".", lb_mon_sub$LANDING_DATE) |
                               TRIP_ID %in% lb_mon_sub$SCHEDULE_NUMBER, 
                               1, 0
                               )
         )
table(edlr$mon_activity)
table(is.na(edlr$mon_activity))


### BFT ###
