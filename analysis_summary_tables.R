# =============================================================================
# Wrangle data and calculate summary statistics for to be used in the dashboard
# Do it here instead of in the dashboard Rmd file. It keeps the dashboard Rmd 
# file cleaner, and easier to get an overview of the layout.
# Created: 2019-09-13
# =============================================================================

library(dplyr)

# Download call log data and scheduled interview data to the desktop. The R 
# script below reads it in and cleans it.
source("data_call_log.R")
source("data_participant_scheduler.R")


# Calculate summary statistics about calls made and interviews scheduled.
#   - Number of calls made by day.
#   - Cummulative number of calls made by day. 
#   - Number of f/u interviews scheduled by day.
#   - Cummulative number of f/u interviews scheduled by day.
# -----------------------------------------------------------------------------
n_calls_per_day <- call_log %>%
  count(call_date) %>% 
  # Rename for merge with scheduled interviews data.
  # Capitalize so that it looks better on the plotly popovers
  rename(
    Date = call_date,
    n_called = n
  )

n_scheduled_per_day <- scheduled_ids %>% 
  count(scheduled_date) %>% 
  # Rename for merge with scheduled interviews data.
  # Capitalize so that it looks better on the plotly popovers
  rename(
    Date = scheduled_date,
    n_scheduled = n
  )


# Join calls made and interviews scheduled data into a single summary table.
#   
calls_and_scheduled_interviews_by_day <- n_calls_per_day %>% 
  full_join(n_scheduled_per_day, by = "Date") %>% 
  # Fill-in missing days with zero
  tidyr::complete(
    Date = seq(Date[1], Sys.Date(), by = "1 day"), 
    fill = list(
      n_called    = 0, 
      n_scheduled = 0
    )
  ) %>% 
  # Add cumulative calls and interviews scheduled
  mutate(
    n_called_cum    = cumsum(n_called),
    n_scheduled_cum = cumsum(n_scheduled)
  ) %>%
  # Add call day of week variable
  mutate(
    Day = weekdays(Date),
    Day = forcats::fct_relevel(
      Day, 
      "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"
    )
  ) %>% 
  # Make a factor version of the number of interviews scheduled variable for 
  # for coloring the points on the "Recruiting calls made and follow-up 
  # interviews scheduled by date" plot.
  mutate(
    n_scheduled_f = case_when(
      n_scheduled == 0 ~ 1,
      n_scheduled == 1 ~ 2,
      n_scheduled >  1 ~ 3
    ),
    n_scheduled_f = factor(n_scheduled_f, levels = c(1, 2, 3), labels = c(
      "None", "1", "2 or More"
    ))
  ) %>% 
  # Remove Saturday and Sunday from the data
  filter(!Day %in% c("Saturday", "Sunday"))


# Summary values
# -----------------------------------------------------------------------------
total_calls <- sum(calls_and_scheduled_interviews_by_day["n_called"])
n_days_recruiting <- nrow(calls_and_scheduled_interviews_by_day)
mean_calls_per_day <- round(total_calls/n_days_recruiting, 0)
total_scheduled <- sum(calls_and_scheduled_interviews_by_day["n_scheduled"])
scheduled_rate <- round(total_calls/total_scheduled, 0)

rm(call_log, n_calls_per_day, n_scheduled_per_day, scheduled_ids)
# Data checks
# No scheduled on days with no calls.
# No calls or scheduled on weekends.