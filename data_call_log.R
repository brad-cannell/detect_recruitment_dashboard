# =============================================================================
# Read-in and wrangle call log data
# Created: 2019-09-11
# =============================================================================

library(dplyr)
source("make_posixct.R") # Used to convert dates to POSIXct

# Read-in data
# Download to desktop to refresh
call_log <- readr::read_csv("/Users/bradcannell/Desktop/Participant Call Log.csv")


# NOTES on data 
# =============================================================================
# 2019-08-31 (From Sunil): 
 
# Originally Participant Call Log was not configured to be an exportable table, 
# since there was no research data coming out of there. So this table did not 
# include the following variables, NameFull, xRecordMonth, and xRecordYear (which 
# pulls in participant's full name and related record month and year respectively 
# from the Participant table).

# On or around 8/22/19 you had asked about including the phone call log in the 
# Analytics section. I ran a script that updated all records in the Calls Log 
# with the xRecordMonth and xRecordYear, which is the modification timestamp 
# showing 8/22/2019 at 11:17 AM.

# During the above change, I didn't pull in NameFull because of the way that 
# variable is configured, instead changed the code so that all future call 
# logs would pull in the name going forward. However, if you need the name I 
# can update this, not complicated to do. The modification timestamp would 
# update though with when I do this.
                                                                                                                                                                                                                                      
# 8/8/19 seems more likely to be a data entry error than a test case. The 
# record was created on 8/15/2019 by Jennifer (jtoro) and there are valid 
# records before and after with CallDate set to 8/15/2019. Also 8/8/19 is 
# right above 8/15/19 when using the drop-down calendar. It might make more 
# sense to change CallDate for that record from 8/8/19 to 8/15/19.
# =============================================================================


# Data wrangling

## Convert variable names to snake case
names(call_log) <- names(call_log) %>% stringr::str_replace_all('(\\B)([A-Z])', '_\\2') 
names(call_log) <- names(call_log) %>% tolower()
call_log <- rename(call_log, medstar_id = medstar_i_d)

## Convert dates
call_log <- call_log %>% 
  mutate(
    # Change classes
    x_created_timestamp  = make_posixct(x_created_timestamp),
    x_modified_timestamp = make_posixct(x_modified_timestamp),
    call_date            = as.Date(call_date, "%m/%d/%Y"),
    # Separate date and time
    created_date = as.Date(x_created_timestamp),
    created_time = hms::as_hms(x_created_timestamp),
    # Create a call time hour variable
    call_hour = lubridate::hour(created_time),
    call_hour_f = factor(call_hour, 
      labels = c("10-10:59", "11-11:59", "12-12:59", "13-13:59", "14-14:59", "15-15:59")
    ),
    # Fix call_date typos
    # If call_date is earlier than created_date, then set call_date equal to 
    # the date in x_created_timestamp.
    call_date = if_else(call_date < created_date, created_date, call_date),
    # If call_date or call_time are missing, use the record created timestamp
    call_date = if_else(is.na(call_date), created_date, call_date),
    call_time = if_else(is.na(call_time), created_time, call_time),
    # Add call day variable
    day = weekdays(call_date),
    day = forcats::fct_relevel(day, "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
    # Fill-in missing record month and year
    x_record_month = if_else(is.na(x_record_month), months(created_date), x_record_month),
    x_record_year  = if_else(is.na(x_record_year), lubridate::year(created_date), x_record_year)
  )

## Clean up
rm(make_posixct)









