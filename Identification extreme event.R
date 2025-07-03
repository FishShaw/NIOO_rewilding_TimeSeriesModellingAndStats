library(tidyverse)
library(lubridate)
library(SPEI)
library(readxl)    
library(zoo)

# Drought events analysis ----
# Load the data ----
data_folder_path <- "E:/WUR_Intern/RewildingProject_RawData/气象数据/weather.csv"

station_info <- tibble(
  stn = c(275, 283, 356),
  name = c("Deelen", "Hupsel", "Herwijnen"),
  lat = c(52.056, 52.069, 51.859)
)

all_stations_raw <- read.csv(data_folder_path,header=TRUE, stringsAsFactors = FALSE)

# Unit transition and special value ----
daily_data_processed <- all_stations_raw %>%
  rename_with(tolower) %>%
  mutate(date = ymd(yyyymmdd)) %>%
  mutate(
    t_mean = tg / 10,
    t_min = tn / 10,
    t_max = tx / 10
  ) %>%
  mutate(
    precip_mm = if_else(rh == -1, 0, rh / 10)
  ) %>%
  left_join(station_info, by = "stn") %>%
  select(stn, name, date, lat, t_mean, t_min, t_max, precip_mm)


# Aggregate to monthly data ----
monthly_data_by_station <- daily_data_processed %>%
  mutate(
    year = year(date),
    month = month(date)
  ) %>%
  group_by(stn, name, lat, year, month) %>%
  summarise(
    t_mean = mean(t_mean, na.rm = TRUE),
    t_min = mean(t_min, na.rm = TRUE),
    t_max = mean(t_max, na.rm = TRUE),
    precip_mm = sum(precip_mm, na.rm = TRUE),
    .groups = 'drop' 
  )


# Calculate Regional Average (stands for the regional climate condition) ----
regional_monthly_data <- monthly_data_by_station %>%
  group_by(year, month) %>%
  summarise(
    t_mean = mean(t_mean, na.rm = TRUE),
    t_min = mean(t_min, na.rm = TRUE),
    t_max = mean(t_max, na.rm = TRUE),
    precip_mm = mean(precip_mm, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(year, month)


# Calculate PET ----
regional_monthly_data$pet_hargreaves <- hargreaves(
  Tmin = regional_monthly_data$t_min,
  Tmax = regional_monthly_data$t_max,
  lat = regional_monthly_data$lat[1] 
)

# Calculate SPEI ----
regional_monthly_data$balance <- regional_monthly_data$precip_mm - regional_monthly_data$pet_hargreaves

balance_ts <- ts(
  regional_monthly_data$balance,
  start = c(min(regional_monthly_data$year), min(regional_monthly_data$month)),
  frequency = 12
)

spei_3 <- spei(balance_ts, scale = 3)
spei_6 <- spei(balance_ts, scale = 6)
spei_12 <- spei(balance_ts, scale = 12)

regional_monthly_data$spei_3 <- as.numeric(spei_3$fitted)
regional_monthly_data$spei_6 <- as.numeric(spei_6$fitted)
regional_monthly_data$spei_12 <- as.numeric(spei_12$fitted)

# Generate Drought Event Table ----
drought_threshold <- -1.0
min_duration_months <- 3

drought_periods <- regional_monthly_data %>%
  filter(!is.na(spei_12)) %>% 
  mutate(
    is_drought = spei_12 <= drought_threshold,
    event_start = is_drought & !lag(is_drought, default = FALSE),
    event_id = cumsum(event_start)
  ) %>%
  filter(is_drought)

drought_events_summary <- drought_periods %>%
  group_by(event_id) %>%
  summarise(
    start_date = min(ymd(paste(year, month, 1))),
    end_date = max(ymd(paste(year, month, 1))),
    duration_months = n(),
    peak_severity = min(spei_12) 
  ) %>%
  filter(duration_months >= min_duration_months) %>%
  mutate(EventType = "Drought") %>%
  select(
     EventType,
     StartDate = start_date,
     EndDate = end_date,
     DurationMonths = duration_months,
     PeakSeverity_SPEI = peak_severity
  ) %>%
  arrange(PeakSeverity_SPEI) 

print(drought_events_summary, n = 50)



# Flood event analysis
# Load the data ----
flood_data_path <- "E:/WUR_Intern/RewildingProject_RawData/水文数据/20250416_033_analysis.csv"
discharge_raw <- read_csv(flood_data_path)

# Data preprocessing ----
discharge_data <- discharge_raw %>%
  rename(
    discharge_m3s = `mean daily discharge`,
    date_str = time
  ) %>%
  mutate(date = dmy(date_str)) %>%
  select(date, discharge_m3s) %>%
  filter(!is.na(discharge_m3s)) %>%
  arrange(date)

# Calculate flood threshold ----
flood_threshold <- quantile(discharge_data$discharge_m3s, 0.95, na.rm = TRUE)
print(round(flood_threshold, 2))

# Identify flood events ----
min_duration_days <- 5
flood_periods <- discharge_data %>%
  mutate(
    is_flood = discharge_m3s >= flood_threshold,
    event_start = is_flood & !lag(is_flood, default = FALSE),
    event_id = cumsum(event_start)
  ) %>%
  filter(is_flood)

flood_events_summary <- flood_periods %>%
  group_by(event_id) %>%
  summarise(
    start_date = min(date),
    end_date = max(date),
    duration_days = n(),
    peak_discharge = max(discharge_m3s)
  ) %>%
  filter(duration_days >= min_duration_days) %>%
  mutate(EventType = "Flood") %>%
  select(
    EventType,
    StartDate = start_date,
    EndDate = end_date,
    DurationDays = duration_days,
    PeakDischarge_m3s = peak_discharge
  ) %>%
  arrange(desc(PeakDischarge_m3s))

print(flood_events_summary, n = 50)

# Combine drought and flood events and identify overlap of the events ----
drought_events_formatted <- drought_events_summary %>%
  mutate(SeverityRank = rank(PeakSeverity_SPEI, ties.method = "first")) %>%
  select(EventType, StartDate, EndDate, SeverityRank)

flood_events_formatted <- flood_events_summary %>%
  mutate(SeverityRank = rank(-PeakDischarge_m3s, ties.method = "first")) %>%
  select(EventType, StartDate, EndDate, SeverityRank)

all_events <- bind_rows(drought_events_formatted, flood_events_formatted) %>%
  arrange(SeverityRank)

pre_event_buffer_months <- 12
post_event_buffer_months <- 18

all_events_with_window <- all_events %>%
  mutate(
    WindowStart = StartDate %m-% months(pre_event_buffer_months),
    WindowEnd = EndDate %m+% months(post_event_buffer_months)
  )


final_independent_events <- list()
banned_intervals <- list()

for (i in 1:nrow(all_events_with_window)) {
  
  current_event <- all_events_with_window[i, ]
  is_overlapping <- FALSE
  
  for (banned_interval in banned_intervals) {
    if (current_event$WindowStart <= banned_interval$end && current_event$WindowEnd >= banned_interval$start) {
      is_overlapping <- TRUE
      break
    }
  }
  
  if (!is_overlapping) {
    final_independent_events[[length(final_independent_events) + 1]] <- current_event
    banned_intervals[[length(banned_intervals) + 1]] <- list(start = current_event$WindowStart, end = current_event$WindowEnd)
  }
}



final_events_df <- bind_rows(final_independent_events) %>%
  arrange(StartDate)

print(final_events_df)
# write.csv(final_events_df, 
#           "E:/WUR_Intern/RewildingProject_RawData/ExtremeEvents_selected/FinalExtremeEvents.csv", 
#           row.names = FALSE, 
#           na = "")

# Calculate Rewilding Age ----
rewilding_start_dates <- tibble(
  Unit = c("MW", "BB", "OW", "EW"),
  RewildingStartYear = c(1993, 2002, 2006, 2008)
)

  
site_event_combinations <- crossing(
    Unit = rewilding_start_dates$Unit,
    EventDate = final_events_df$StartDate
  )
  
  
rewilding_age_at_event <- site_event_combinations %>%
  left_join(final_events_df, by = c("EventDate" = "StartDate")) %>%
  left_join(rewilding_start_dates, by = "Unit") %>%
  mutate(
    EventYear = year(EventDate),
    RewildingAge = EventYear - RewildingStartYear
  ) %>%
  filter(RewildingAge >= 0) %>%
  select(
    Site = Unit,
    EventType,
    StartDate = EventDate,
    EndDate,
    RewildingAge,
    WindowStart,
    WindowEnd,
    SeverityRank
  ) %>%
  arrange(Site, StartDate)
  
print(rewilding_age_at_event, n = 100)
# write.csv(rewilding_age_at_event, 
#           "E:/WUR_Intern/RewildingProject_RawData/ExtremeEvents_selected/RewildingAgeAtEvents.csv", 
#           row.names = FALSE, 
#           na = "")







