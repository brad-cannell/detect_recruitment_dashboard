# =============================================================================
# Read-in and wrangle participant scheduler data
# Created: 2019-09-11
# =============================================================================

library(dplyr)
source("make_posixct.R") # Used to convert dates to POSIXct

# Read-in data
# Download to desktop to refresh
participant_scheduler <- readr::read_csv("/Users/bradcannell/Desktop/Participant Scheduler.csv")

# Data wrangling

## Convert variable names to snake case
names(participant_scheduler) <- names(participant_scheduler) %>% stringr::str_replace_all('(\\B)([A-Z])', '_\\2') 
names(participant_scheduler) <- names(participant_scheduler) %>% tolower()
participant_scheduler <- rename(participant_scheduler, medstar_id = medstar_i_d)

## Keep scheduled rows only
scheduled_ids <- participant_scheduler %>% 
  filter(!is.na(appointment_date))

## Keep only the information needed for merging with call log
scheduled_ids <- scheduled_ids %>% 
  select(x_created_timestamp, medstar_id) %>% 
  mutate(
    scheduled = 1L,
    # Change classes
    x_created_timestamp  = make_posixct(x_created_timestamp),
    # Separate date and time
    scheduled_date = as.Date(x_created_timestamp),
    scheduled_time = hms::as_hms(x_created_timestamp),
    # Create a call time hour variable
    scheduled_hour = lubridate::hour(scheduled_time),
    scheduled_hour_f = factor(scheduled_hour, 
                         levels = c(10, 11, 12, 13, 14, 15),
                         labels = c("10-10:59", "11-11:59", "12-12:59", "13-13:59", "14-14:59", "15-15:59")
    )
  ) %>% 
  select(-x_created_timestamp)

## Clean up
rm(participant_scheduler, make_posixct)