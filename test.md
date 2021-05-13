---
title: "DETECT Recruiting Dashboard"
# date: "Updated: 2021-05-12"
output: 
  flexdashboard::flex_dashboard:
    orientation: rows
    vertical_layout: scroll
---

<!-- 
When refreshing, download the following to desktop:
  1. Participant scheduler
  2. Participant call log
  3. Phone recruitment
  4. Gift card
-->

<!-- 
Packages:
  1. rmarkdown
  2. dplyr
  3. DBI
  4. odbc
  5. Keyring
  6. purrr
  7. stringr
  8. hms
  9. lubridate
  10. forcats
  11. readr
  12. flexdashboard
  13. tidyr
  14. ggplot2
  15. plotly
  16. knitr
-->




```r
# Download call log data to desktop. The R script below reads it in and cleans it.
# source("data_call_log.R")
# source("data_participant_scheduler.R")
# source("data_gift_card.R")
# source("data_moca.R")
# source("data_import.R")
```

<!-- Data Wrangling -->


```r
# Calculate summary statistics about calls made that will be used in the charts below
calls_per_day <- call_log %>%
  count(call_date) %>% 
  # Fill-in missing days with zero
  complete(call_date = seq(call_date[1], Sys.Date(), by = "1 day"), fill = list(n = 0)) %>% 
  # Add cumulative calls 
  mutate(cumulative_n = cumsum(n)) %>% 
  # Add call day variable
  mutate(
    day = weekdays(call_date),
    day = forcats::fct_relevel(day, "Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  ) %>% 
  # Improve plot readability
  rename(
    Date = call_date,
    Day = day
  ) %>% 
  filter(Day!='Saturday' & Day!='Sunday') %>%
  mutate(
    day_num = as.numeric(Day)
  )
```


```r
# Adds a new column to calls-per_day that iteratively counts the week number.
# Set the very first value in week_num equal to 0. Week 1 will start on the 
# Start week 1 on the first Monday in the data
calls_per_day[1, "week_num"] <- 0
calls_per_day[3, "week_num"] <- 1

# Starting at the 8th row, and incrementing by 5, count week number up by 1 on each Monday
for (i in seq(8, nrow(calls_per_day), 5)) {
  calls_per_day[i,"week_num"] = calls_per_day[i-5,"week_num"] + 1
} 

# Carry week number forward across rows
calls_per_day <- calls_per_day %>% 
  fill(week_num)
```


```r
# Variables to help calculate calls made this week, calls made two weeks ago, 
# and the percent change.
# Identify last week (i.e., the Monday of the most recent complete Monday-Sunday)
# Identify two weeks ago (i.e., the Monday before that)
calls_per_day <- calls_per_day %>% 
  mutate(
    last_week = week_num == max(week_num, na.rm = T) - 1,
    two_weeks_ago = week_num == max(week_num, na.rm = T) - 2
  )
```


```r
# 2019-09-19: Sunil has a test record in the data. I emailed him about it. This part of the code can be deleted once he drops that record from the data. 
scheduled_ids <- filter(scheduled_ids, !is.na(scheduled_hour_f))

# Calculate summary statistics about interviews scheduled that will be used in the charts below
scheduled_per_day <- scheduled_ids %>% 
  count(scheduled_date) %>% 
  # Improve plot readability
  rename(
    Date = scheduled_date,
    n_scheduled = n
  )
```


```r
# Merge the calls data with the scheduled data
calls_per_day_w_scheduled <- calls_per_day %>%
  left_join(scheduled_per_day, by = "Date") %>% 
  mutate(
    n_scheduled = if_else(is.na(n_scheduled), 0L, n_scheduled),
    cumulative_scheduled = cumsum(n_scheduled)
  ) %>% 
  
  # For coloring the points on the recruiting calls plot below
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
  
  # Renaming to make the plotly popovers look better
  rename(
    `N called` = n,
    `N Scheduled` = n_scheduled_f
  )
```


```r
followups <- gift_card %>%
  mutate(followup_date = as.Date(x_created_timestamp, format = "%m/%d/%Y")) %>%
  count(followup_date)%>% 
  # Fill-in missing days with zero
  complete(followup_date = seq(followup_date[1], Sys.Date(), by = "1 day"), fill = list(n = 0)) %>% 
  mutate(
    day = weekdays(followup_date),
    day = forcats::fct_relevel(day, "Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  ) %>% 
    rename(
    Date = followup_date,
    Day = day 
  ) %>% 
  filter(Day!='Saturday' & Day!='Sunday') %>%
  mutate(
    date = as.numeric(Day)
    )

followups[3,"week_num"] = 1
for (i in seq(8, nrow(followups), 5)) {
  followups[i,"week_num"] = followups[i-5,"week_num"] + 1
} 
```



```r
# Data check
# At this point, both should be zero. We aren't making calls on the weekend. 
# A number > 0 indicates a data entry error.
# calls_per_day_w_scheduled %>% 
#   filter(Day %in% c("Saturday", "Sunday")) %>% 
#   summarise(
#     called = sum(`N called`),
#     scheduled = sum(n_scheduled)
#   )
```


```r
# Remove Saturday and Sunday from the data
# Primarily to get a more accurate calls per day calculation
calls_per_day_w_scheduled <- calls_per_day_w_scheduled %>% 
  filter(!Day %in% c("Saturday", "Sunday"))
```


```r
# Stack the data to long format to easily color the cumulative people and 
# follow-up interviews scheduled line chart
cum_calls_and_scheduled <- calls_per_day_w_scheduled %>%
  select(Date, cumulative_n, cumulative_scheduled) %>%
  tidyr::pivot_longer(-Date, names_to = "group", values_to = "n") %>% 
  
  # To improve readability
  mutate(
    group = if_else(group == "cumulative_n", "Calls", "Scheduled F/U Visits")
  )
```


```r
# Get means and total calls by day
# For the calls made by day of the week plot
means_by_day <- calls_per_day %>% 
  group_by(Day) %>% 
  filter(!Day %in% c("Saturday", "Sunday")) %>%
  summarise(
    mean = mean(n, na.rm = TRUE),
    total = sum(n, na.rm = TRUE),
    .groups = "drop"
  )
```


<!-- Dashboard starts here -->

Overview
=======================================================================

Updated: 2021-05-12

Value boxes
-----------------------------------------------------------------------

### Total follow-up interviews completed


```r
gauge(n_completed, min = 0, max = 2520)
```

```
## Warning in normalizePath(f2): path[1]="webshot11c441bf5831f.png": No such file or directory
```

```
## Warning in file(con, "rb"): cannot open file 'webshot11c441bf5831f.png': No such file or directory
```

```
## Error in file(con, "rb"): cannot open the connection
```

### Total calls made


```r
total_calls <- nrow(call_log)
valueBox(format(total_calls, big.mark = ","), icon = "fa-phone")
```

<!--html_preserve--><span class="value-output" data-icon="fa-phone">17,260</span><!--/html_preserve-->

### Average number of calls per day


```r
n_days_recruiting <- calls_per_day_w_scheduled %>% 
  filter()
mean_calls_per_day <- round(total_calls/nrow(calls_per_day_w_scheduled), 0)
valueBox(mean_calls_per_day, icon = "fa-phone")
```

<!--html_preserve--><span class="value-output" data-icon="fa-phone">38</span><!--/html_preserve-->

### Total follow-up interviews scheduled


```r
total_scheduled <- max(calls_per_day_w_scheduled$cumulative_scheduled)
valueBox(total_scheduled, icon = "fa-calendar")
```

<!--html_preserve--><span class="value-output" data-icon="fa-calendar">458</span><!--/html_preserve-->

### Follow-up interview scheduling rate


```r
rate_scheduled <- round(total_calls/total_scheduled, 0)
scheduled_message <- paste("1 per", rate_scheduled, "calls")
valueBox(scheduled_message, icon = "fa-phone")
```

<!--html_preserve--><span class="value-output" data-icon="fa-phone">1 per 38 calls</span><!--/html_preserve-->

Value boxes
-----------------------------------------------------------------------

### Average number of calls made to each patient


```r
mean_calls <- call_log %>% count(medstar_id) %>% summarise(mean = mean(n)) %>% round(1)
valueBox(mean_calls, icon = "fa-phone")
```

<!--html_preserve--><span class="value-output" data-icon="fa-phone">1.7</span><!--/html_preserve-->

<!-- Calls made 2 weeks ago, last week, and the percentage change -->

### Total calls made 2 weeks ago


```r
total_calls_2_weeks <- calls_per_day %>%
  filter(two_weeks_ago) %>%
  summarise(sum(n))
valueBox(total_calls_2_weeks, icon = "fa-phone")
```

<!--html_preserve--><span class="value-output" data-icon="fa-phone">108</span><!--/html_preserve-->

### Total calls made last week


```r
total_calls_last_week <- calls_per_day %>%
  filter(last_week) %>%
  summarise(sum(n))
valueBox(total_calls_last_week, icon = "fa-phone")
```

<!--html_preserve--><span class="value-output" data-icon="fa-phone">226</span><!--/html_preserve-->

### Percent change between two weeks ago and last week


```r
pct_change <- (total_calls_last_week - total_calls_2_weeks) / total_calls_2_weeks * 100
pct_change_chr <- round(pct_change) %>% paste0("%")
valueBox(pct_change_chr, icon = "fa-phone")
```

<!--html_preserve--><span class="value-output" data-icon="fa-phone">109%</span><!--/html_preserve-->

### Highest number of calls made in any week


```r
highest_calls <- calls_per_day %>%
  fill(week_num) %>%
  group_by(week_num) %>%
  summarise(week_calls = sum(n)) %>%
  ungroup() %>%
  summarise(max(week_calls))
           
valueBox(highest_calls, icon = "fa-phone")
```

<!--html_preserve--><span class="value-output" data-icon="fa-phone">525</span><!--/html_preserve-->

### Total followup interviews scheduled 2 weeks ago


```r
total_followups_2_weeks <- followups %>%
  fill(week_num) %>%
  filter(week_num == max(week_num, na.rm = T)-2) %>%
  summarise(
    `Total Follow-up Interviews Made 2 Weeks Ago` = sum(n)
  )

valueBox(total_followups_2_weeks, icon = "fa-phone")
```

<!--html_preserve--><span class="value-output" data-icon="fa-phone">5</span><!--/html_preserve-->

### Total followup interviews scheduled last week


```r
total_followups_last_week <- followups %>%
  fill(week_num) %>%
  filter(week_num == max(week_num, na.rm = T)-1) %>%
  summarise(
    total_calls = sum(n)
  )
valueBox(total_followups_last_week, icon = "fa-phone")
```

<!--html_preserve--><span class="value-output" data-icon="fa-phone">3</span><!--/html_preserve-->


### Highest number of followup interviews scheduled in any week

```r
highest_followups <- followups %>%
  fill(week_num) %>%
  group_by(week_num) %>%
  summarise(week_followups = sum(n)) %>%
  ungroup() %>%
  summarise(max(week_followups))
           
valueBox(highest_followups, icon = "fa-phone")
```

<!--html_preserve--><span class="value-output" data-icon="fa-phone">14</span><!--/html_preserve-->

Plots
-----------------------------------------------------------------------

### Recruiting calls made and follow-up interviews scheduled by date


```r
calls_per_day_plot <- calls_per_day_w_scheduled %>% 
  ggplot(aes(Date, `N called`)) +
  geom_line(color = "#8a8a8a") +
  geom_point(aes(color = `N Scheduled`)) +
  scale_x_date("Date", date_label = "%Y-%b" 
    # Make sure the x-axis includes the earliest date and today with other breakes
    # coming at 1 week intervals.
    # 2020-11-09: Commented out the breaks. It looked cluttered.
    # breaks = seq(min(calls_per_day_w_scheduled$Date), Sys.Date(), "weeks")
  ) +
  scale_y_continuous("Number of Calls") +
  scale_color_manual(
    "F/U Scheduled", 
    values = c("#a60303", "#6AA2E7", "#03a606"),
    drop = FALSE
  ) +
  theme_bw() +
  theme(legend.title = element_text(size = 8))

plotly::ggplotly(calls_per_day_plot)
```

```
## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
```

```
## Warning in normalizePath(f2): path[1]="webshot11c4413decde5.png": No such file or directory
```

```
## Warning in file(con, "rb"): cannot open file 'webshot11c4413decde5.png': No such file or directory
```

```
## Error in file(con, "rb"): cannot open the connection
```

> The points on the plot above are colored according to the number of follow-up interviews scheduled on a given day.     
> Red = None, Blue = 1, Green = 2 or more.
