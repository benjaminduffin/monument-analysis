# Header ------------------------------------------------------------------

# Compling data for monument litigation
# BFT and eDealer landings 
# BD 12/5/2022

# this script indentifies which dealer data corresponds to trips with effort 
# inside the monument buffer zone


# BFT round to dressed weight conversion factor: 0.8


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
library(tidyr)


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
edlr <- read_sheet("https://docs.google.com/spreadsheets/d/1LNu4Yzezguc5V_t4Irzo6bTddzceznDxHZpN8ir4cU4/edit?usp=sharing", 
                   sheet = "raw data")
# read in the BFT data 
# commented out, saved locally
# drive_download(file = as_id("https://docs.google.com/spreadsheets/d/1wgfmPv2kWBDFBl0aVeLK3LuwHbzeMujM/edit?usp=sharing&ouid=100135471101551472212&rtpof=true&sd=true"), 
#                       path = here::here("data", paste0("bft_monument_", Sys.Date(), ".xlsx")), 
#                       type = "xlsx")
bft <- read_xlsx(here::here("data", "bft_monument_2022-12-05.xlsx"), 
                 sheet = 1)
names(bft) <- make.names(toupper(names(bft)))

# and quickly subset for just >2015
bft <- subset(bft, LANDING.YEAR > 2015)


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

# check - good all accounted for 
unique(edlr$TRIP_ID[edlr$mon_activity == 1])


### BFT ###

# quick look 
glimpse(bft)

# convert to DW those that are round 
bft$dw_lb <- ifelse(bft$REPORTED.QUANTITY == "ROUND", 
                    bft$REPORTED.QUANTITY * 0.8, 
                    bft$REPORTED.QUANTITY)

# make subset of the data for identifying those in monument buffer 
bft_sub <- bft %>% 
  select(COAST.GUARD.NBR, LANDING.DATE, DEALER.NAME, DEALER.RPT.ID) %>%
  distinct()

# check for direct matches 
table(paste0(lb_mon_sub$VESSEL_ID, ".", lb_mon_sub$LANDING_DATE) %in% 
        paste0(bft_sub$COAST.GUARD.NBR, ".", bft_sub$LANDING.DATE)) # 3 false 

lb_mon_sub$ves.date_in_bft <- ifelse(paste0(lb_mon_sub$VESSEL_ID, ".", lb_mon_sub$LANDING_DATE) %in% 
                                       paste0(bft_sub$COAST.GUARD.NBR, ".", bft_sub$LANDING.DATE), 
                                     "x", NA)
lb_mon_sub

# are these just without bluefin tuna? 
# one had a BFT - 221015587; dates differed ()
nobft <- logbook %>% 
  filter(VESSEL_ID == '1026595', 
         LANDING_DATE %in% c("2018-10-09", "2021-08-28", "2021-09-23")) %>%
  group_by(SCHEDULE_NUMBER, NMFS_COMMON, DISPOSITION_STATUS) %>%
  summarize(n_animals = sum(NUMBER_OF_INDIVIDUALS))

## sidebar: where does this line up with the dealer data? ##
# 
edlr %>% # also 8/30/22
  filter(TRIP_ID == '221015587')

## End sidebar ##

# lets create a new date variable for the logbook data that matches the dealer report
lb_mon_sub
lb_mon_sub <- lb_mon_sub %>%
  mutate(dealer_date = ifelse(SCHEDULE_NUMBER == '221015587', "2021-08-30", LANDING_DATE),
         ves.date = paste(VESSEL_ID, dealer_date, sep = ".") # add in vessel.date
         )

### Indicating which landings were Monument landings int he soruce data 

# BFT
bft_mon <- bft %>% 
  mutate(ves.date = paste(COAST.GUARD.NBR, LANDING.DATE, sep = "."),
         monument = ifelse(ves.date %in% lb_mon_sub$ves.date, 1, 0)
         ) 

# eDealer 
edlr_mon <- edlr %>%
  mutate(ves.date = paste(SUPPLIER_VESSEL_ID, DATE_LANDED, sep = "."), 
         monument = ifelse(ves.date %in% lb_mon_sub$ves.date, 1, 0)
         ) 

# summarize landings 
# BFT
bft_summary <- bft_mon %>% 
  mutate(COMMON_NAME = "Bluefin", 
         ) %>%
  group_by(COAST.GUARD.NBR, LANDING.YEAR, COMMON_NAME, monument) %>%
  summarize(dw_lb = sum(dw_lb), 
            revenue = sum(DOLLARS)
            ) %>%
  pivot_wider(names_from = monument, 
              values_from = c(dw_lb, revenue), 
              values_fill = 0)

# rename for consistency 
names(bft_summary) <- c("VESSEL_ID", "YEAR", "COMMON_NAME", "DW_LBS_OUT", "DW_LBS_IN", "REVENUE_OUT", "REVENUE_IN")
# edealer
edlr_summary <- edlr_mon %>%
  filter(is.na(not_included)) %>% # remove those that Jackie indicated were dupes 
  group_by(SUPPLIER_VESSEL_ID, YEAR_OF_LANDING, COMMON_NAME_AS_REPORTED, monument) %>%
  summarize(dw_lb = sum(converted_dw), 
            revenue = sum(TOTAL_DOLLARS, na.rm = T)
            ) %>%
  ungroup() %>%
  pivot_wider(names_from = monument, 
              values_from = c(dw_lb, revenue), 
              values_fill = 0)

# rename for consistency
names(edlr_summary) <- c("VESSEL_ID", "YEAR", "COMMON_NAME", "DW_LBS_OUT", "DW_LBS_IN", "REVENUE_OUT", "REVENUE_IN")

# bring it all together 

f_summary <- bind_rows(edlr_summary, bft_summary) %>%
  arrange(VESSEL_ID, YEAR, COMMON_NAME)

# calculate the percent in/out for each row
f_summary_sp <- f_summary %>%
  mutate(TOT_DW_LBS = DW_LBS_OUT + DW_LBS_IN,
         DW_LBS_IN_PERC = round(DW_LBS_IN / (DW_LBS_OUT + DW_LBS_IN), 2), 
         TOT_REV = REVENUE_IN + REVENUE_OUT,
         REVENUE_IN_PERC = round(REVENUE_IN / (REVENUE_IN + REVENUE_OUT), 2)
         )

# collapse to vessel/year combo
f_summary_vesyear <- f_summary %>%
  group_by(VESSEL_ID, YEAR) %>%
  summarize(DW_LBS_OUT = sum(DW_LBS_OUT), 
            DW_LBS_IN = sum(DW_LBS_IN), 
            REVENUE_IN = sum(REVENUE_IN), 
            REVENUE_OUT = sum(REVENUE_OUT), 
            TOT_DW_LBS = sum(TOT_DW_LBS), 
            TOT_REV = sum(TOT_REV)
            ) %>%
  mutate(DW_LBS_IN_PERC = round(DW_LBS_IN / (DW_LBS_OUT + DW_LBS_IN), 2), 
         REVENUE_IN_PERC = round(REVENUE_IN / (REVENUE_IN + REVENUE_OUT), 2))


# just by vessel
f_summary_vessel <- f_summary %>%
  group_by(VESSEL_ID) %>%
  summarize(DW_LBS_OUT = sum(DW_LBS_OUT), 
            DW_LBS_IN = sum(DW_LBS_IN), 
            REVENUE_IN = sum(REVENUE_IN), 
            REVENUE_OUT = sum(REVENUE_OUT), 
            TOT_DW_LBS = sum(TOT_DW_LBS), 
            TOT_REV = sum(TOT_REV)
  ) %>%
  mutate(DW_LBS_IN_PERC = round(DW_LBS_IN / (DW_LBS_OUT + DW_LBS_IN), 2), 
         REVENUE_IN_PERC = round(REVENUE_IN / (REVENUE_IN + REVENUE_OUT), 2))

# Output ------------------------------------------------------------------

f_summary_list <- list(f_summary_sp, f_summary_vesyear, f_summary_vessel)
names(f_summary_list) <- c("VesselYearSpecies", "VesselYear", "Vessel")
# write the final summary 
write_xlsx(f_summary_list, 
           here::here("output", paste0("BFT_eDealer_with_monument_", Sys.Date(), ".xlsx")))

# Total landings by vessel and year 

# edlr -- caveat - all reported landings - can dive deeper in the grades etc. 
edlr_landings <- edlr %>%
  mutate(YEAR = str_sub(DATE_LANDED, 1, 4), 
         vessel_rev = REPORTED_QUANTITY * PURCHASE_PRICE) %>%
  filter(is.na(not_included)) %>%
  group_by(SUPPLIER_VESSEL_ID,  COMMON_NAME_AS_REPORTED) %>%
  summarize(Dealer_Landings = sum(REPORTED_QUANTITY), 
            Total_Dollars = sum(vessel_rev))
  

# bft 
bft_landings <- bft %>% 
  group_by(COAST.GUARD.NBR, LANDING.YEAR) %>%
  summarize(BFT_Landings = sum(REPORTED.QUANTITY), 
            Total_Dollars = sum(DOLLARS))
  
landings <- list(edlr_landings, bft_landings)
names(landings) <- c("eDealer Landings", "BFT Landings")

write_xlsx(landings, 
           here::here("output", paste0("BFT_eDealer_landings_", Sys.Date(), ".xlsx")))


# output plots 
# edealer landings 
ggplot(data = edlr_landings) + 
  geom_bar(aes(x = as.character(SUPPLIER_VESSEL_ID), y = Dealer_Landings, fill = COMMON_NAME_AS_REPORTED), 
           stat = "identity", position = "dodge") +
  labs(x = "Vessel", y = "Reported Quantity (lbs)")

# edealer dollars
ggplot(data = edlr_landings) + 
  geom_bar(aes(x = as.character(SUPPLIER_VESSEL_ID), y = Total_Dollars, fill = COMMON_NAME_AS_REPORTED), 
           stat = "identity", position = "dodge") +
  labs(x = "Vessel", y = "Total Dollars")

ggplot(data = bft_landings) + 
  geom_line(aes(x = LANDING.YEAR, y = BFT_Landings), size = 1.25, color = "red") + 
  geom_point(aes(x = LANDING.YEAR, y = BFT_Landings), color = "red") +
  geom_line(aes(x = LANDING.YEAR, y = Total_Dollars), size = 1.25, color = "forestgreen") + 
  geom_point(aes(x = LANDING.YEAR, y = Total_Dollars), color = "forestgreen") +
  labs(x = "Year", y = "Value", title = "BFT Landings (lbs, red); BFT Total Dollars ($, green)")

## Total lbs/dollars from TRIPS that had activity in the monument area 

## trips that had activity in the monument area 

## panels for vessel - landings and $$
