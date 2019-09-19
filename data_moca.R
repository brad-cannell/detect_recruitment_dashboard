# =============================================================================
# Read-in and wrangle MoCA data
# Created: 2019-09-16
# =============================================================================

library(dplyr)

# Read-in data
# Download to desktop to refresh
moca <- readr::read_csv("/Users/bradcannell/Desktop/Phone Recruitment.csv")

# Data wrangling

## Convert variable names to snake case
names(moca) <- names(moca) %>% stringr::str_replace_all('(\\B)([A-Z])', '_\\2') 
names(moca) <- names(moca) %>% tolower()
moca <- rename(moca, medstar_id = medstar_i_d)

## Deidentify data for local storage
moca_deid <- moca %>% 
  select(phone_eligible_consent:appointment_date) %>% 
  # Create dummy variable for whether or not an appointment was scheduled
  # Can't store the actual date locally.
  mutate(appt_scheduled = !is.na(appointment_date)) %>% 
  select(-appointment_date)

## Save locally
readr::write_csv(moca_deid, "data/moca_deid.csv")

