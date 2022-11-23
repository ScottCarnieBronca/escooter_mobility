# LAST UPDATED: 2022/02/08
# Scott Carnie-Bronca

#Libraries
library(tidyverse)
library(lubridate)
library(sp)
library(leaflet)
library(foreach)
library(sf)
library(parallel)

#R options
options("lubridate.week.start"=1) #set first day of week to Monday instead of Sunday

# CORE NUMBER CHECKING FOR PHOENIX
slurm_ncores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK"))
n_cores <- as.integer(Sys.getenv("SLURM_JOB_CPUS_PER_NODE"))

#n_cores = detectCores()


doParallel::registerDoParallel(cores = n_cores)


# Hire threshold: how many hires are required to get an estimate of demand. Time threshold: minimum time in seconds to get an estimate of demand

pickup_threshold = 5
time_threshold = 3600


# Beam
## Load data

n = 100 #number of bins on each axis

status = read.csv("./status_binned_r_beam.csv", row.names = 1) %>% as_tibble() %>% mutate(event_time = ymd_hms(event_time, tz = "Australia/Adelaide"))

# Define splitting times
day_range = range(status$event_time)
day_range[1] = ceiling_date(day_range[1], unit = "months")
day_range[2] = floor_date(day_range[2], unit = "months")
month_range = seq(from = day_range[1], to = day_range[2], by = "month")
t_range = as.numeric(month_range)*1000 #in unix time
n_months = length(t_range)-1
first_day = min(floor_date(status$event_time, "day"))
tz(first_day) <- "Australia/Adelaide"
last_day = max(floor_date(status$event_time, "day"))
tz(last_day) <- "Australia/Adelaide"
timeBreaks = seq(first_day, last_day, by = "DSTday")
timeBreaks = rep(timeBreaks, each = 5) + rep(c(0, 6*3600, 12*3600, 15*3600, 19*3600), times = length(timeBreaks))

status = status %>%
  add_row(tibble(
    location_timestamp = 1000*as.numeric(timeBreaks),
    cell_num = as.integer(-1),
    event_type_reason = "time_break",
    event_time = timeBreaks
  )) %>%
  arrange(location_timestamp)


## Add columns to status data frame

status = status %>%
  mutate(change = case_when(
    event_type_reason %in% c("user_drop_off", "maintenance_drop_off", "redistribution_in", "extra_drop_off") ~ 1,
    event_type_reason %in% c("user_pick_up",  "maintenance_pick_up", "redistribution_out", "extra_pick_up") ~ -1,
    TRUE                                                                                   ~ 0
  ))


## Complete demand estimation code
demands = foreach(j = 1:(length(t_range)-1), .combine = c, .packages = c("lubridate", "tidyverse")) %dopar% { # For each month
  cell_populations = tibble(cell = 0, n_in_cell = 0, t = 0, month = 0) #check scooter counts
  # Get INITIAL scooter locations
  t_start = t_range[j]
  t_end = t_range[j+1]
  
  status_month = status %>%
    filter(event_time >= month_range[j]-months(1), event_time <= month_range[j+1], (cell_num >= 0 )| (event_type_reason == "time_break"))
  # Find data points that are useful for finding the location of each scooter
  
  initial_locations = status %>%
    mutate(cell_num_extra = case_when(
      (event_type_reason %in% c("user_drop_off", "maintenance_drop_off", "redistribution_in", "extra_drop_off")) & (location_timestamp <= t_start) ~ cell_num,
      (event_type_reason %in% c("user_pick_up",  "maintenance_pick_up", "redistribution_out", "extra_pick_up")) & (location_timestamp >= t_start) ~ cell_num,
      (event_type_reason %in% c("user_drop_off", "maintenance_drop_off", "redistribution_in", "extra_drop_off")) & (location_timestamp > t_start) ~ as.integer(-1), #if we can't know where the scooter is
      (event_type_reason %in% c("user_pick_up",  "maintenance_pick_up", "redistribution_out", "extra_pick_up")) & (location_timestamp < t_start) ~ as.integer(-1),
      TRUE                                                                                 ~ NA_integer_
    )) %>%
    add_row(event_type = "t_start", vehicle_id = unique(status$vehicle_id), location_timestamp = t_range[j], event_time = month_range[j]) %>%
    group_by(vehicle_id) %>%
    arrange(location_timestamp) %>%
    fill(cell_num_extra, .direction = "downup") %>%
    filter(event_type == "t_start") %>%
    select(vehicle_id, location_timestamp, event_time, cell = cell_num_extra) %>%
    ungroup() %>%
    filter(cell >= 0) %>%
    pull(cell) %>%
    table
  
  # Loop through
  cells_with_scooters = 0:(n^2-1) %in% names(initial_locations) #which cells have scooters
  status_month = status_month %>% filter(location_timestamp >= t_range[j], location_timestamp <= t_range[j+1])
  next_population = rep(0, n^2)
  next_population[names(initial_locations) %>% as.numeric + 1] = initial_locations
  cell_status = tibble(cell = NA_real_, n_pickups = 0, nzi_start = 0, nzi_end = 0, tcat = 0)
  nzi_start = rep(t_range[j], n^2)*cells_with_scooters #start time of non-zero-population interval
  n_pickups = rep(0, n^2) #number of pickups since the previous time scooters were available
  
  tcat_current = as.numeric(month_range[j])*1000
  for(k in 1:nrow(status_month)){
    t = status_month$location_timestamp[k] #what time did the event occur at
    cell = status_month$cell_num[k] #what cell did the event occur in
    change = status_month$change[k] #how did the scooter count change at this time
    type = status_month$event_type_reason[k]
    cell_population = next_population[cell+1] #cell population before the event
    next_population[cell+1] = next_population[cell+1] + change #cell population after the change
    
    if(type == "time_break"){ #if there is a time split
      temp_cell = c()
      temp_n_pickups = c()
      temp_nzi_start = c()
      temp_nzi_end = c()
      for(cell in which(cells_with_scooters)){ # for each cell with scooters
        temp_cell = c(temp_cell, cell-1)
        temp_n_pickups = c(temp_n_pickups, n_pickups[cell])
        temp_nzi_start = c(temp_nzi_start, nzi_start[cell])
        temp_nzi_end = c(temp_nzi_end, t)
        
        nzi_start[cell] = t
        n_pickups[cell] = 0
        
      }
      cell_status = cell_status %>% 
        add_row(cell = temp_cell, n_pickups = temp_n_pickups, nzi_start = temp_nzi_start, nzi_end = temp_nzi_end, tcat = tcat_current)
      tcat_current = t
      next
    }
    # Debugging
    cell_populations = cell_populations %>% add_row(cell = cell, n_in_cell = cell_population, t = t, month = j)
    
    
    if(cell_population == 0){
      #print(1)
      #if the number of scooters leaves 0 after the event
      
      #if(type == "user_pick_up"){n_pickups[cell+1] = n_pickups[cell+1] + 1}
      cells_with_scooters[cell+1] = TRUE
      nzi_start[cell+1] = t
      #print(paste("update nzi_start: ", cell, t, nzi_start[cell+1]))
      
      
    } else {
      #print(2)
      if(next_population[cell+1] == 0){
        #print(3)
        #if the cell population reaches 0 after the event
        
        if(type == "user_pick_up"){n_pickups[cell+1] = n_pickups[cell+1] + 1}
        
        # Update tibble
        #print(paste("update cell status: ", cell, n_pickups[cell+1], t, nzi_start[cell+1]))
        cells_with_scooters[cell+1] = FALSE
        cell_status = cell_status %>%
          add_row(cell, n_pickups = n_pickups[cell+1], nzi_start = nzi_start[cell+1], nzi_end = t)
        n_pickups[cell+1] = 0
        nzi_start[cell+1] = 0 #this should be reset after the scooter count leaves 0
      } else {
        #print(4)
        #if the cell population is > 0 either side of the event
        if(type == "user_pick_up"){n_pickups[cell+1] = n_pickups[cell+1] + 1}
      }
    }
  }
  # End intervals
  for(cell_ind in which(cells_with_scooters)){
    #print(cell_ind)
    cell_status = cell_status %>%
      add_row(cell = cell_ind-1, n_pickups = n_pickups[cell_ind], nzi_start = nzi_start[cell_ind], nzi_end = t_end)
  }
  list(demands = cell_status %>% mutate(month = j), cell_populations = cell_populations) #return
}

cell_populations = demands[[2]]
demands_table = demands[[1]]

for(k in 3:length(demands)){
  if(k %% 2 == 0){
    cell_populations = rbind(cell_populations, demands[[k]])
  } else {
    demands_table = rbind(demands_table, demands[[k]])
  }
}

demands_table = demands_table %>% drop_na(cell) %>% ungroup() %>% fill(tcat, .direction = "updown")


# Time of day splitting

demands_table = demands_table %>%
  mutate(tcat = as.POSIXct(tcat/1000, tz = "Australia/Adelaide", origin = origin)) %>%
  mutate(desc = paste(
    case_when(
      hour(tcat) %in% 0:5 ~ "EarlyMorning",
      hour(tcat) %in% 6:11 ~ "Morning",
      hour(tcat) %in% 12:14 ~ "EarlyAfternoon",
      hour(tcat) %in% 15:18 ~ "Afternoon",
      hour(tcat) %in% 19:23 ~ "Evening",
      TRUE ~ "NA"
    ), case_when(
      wday(tcat) %in% 6 ~ "Weekend", #saturday
      (wday(tcat) == 5) & hour(tcat) %in% 15:23 ~ "Weekend", #friday afternoon + evening
      (wday(tcat) == 7) & hour(tcat) %in% 0:14 ~ "Weekend", #sunday morning
      TRUE ~ "Weekday",
    ),
    sep = ""
  ),
  time_desc =  case_when(
    hour(tcat) %in% 0:5 ~ "EarlyMorning",
    hour(tcat) %in% 6:11 ~ "Morning",
    hour(tcat) %in% 12:14 ~ "EarlyAfternoon",
    hour(tcat) %in% 15:18 ~ "Afternoon",
    hour(tcat) %in% 19:23 ~ "Evening",
    TRUE ~ "NA"
  ),
  day_desc = case_when(
    wday(tcat) %in% 6 ~ "Weekend", #saturday
    (wday(tcat) == 5) & hour(tcat) %in% 15:23 ~ "Weekend", #friday afternoon + evening
    (wday(tcat) == 7) & hour(tcat) %in% 0:14 ~ "Weekend", #sunday morning
    TRUE ~ "Weekday",
  )) %>%
  mutate(desc = factor(desc, levels = c("EarlyMorningWeekday", "MorningWeekday", "EarlyAfternoonWeekday", "AfternoonWeekday", "EveningWeekday", "EarlyMorningWeekend", "MorningWeekend", "EarlyAfternoonWeekend", "AfternoonWeekend", "EveningWeekend")),
         day_desc = factor(day_desc, levels = c("Weekday", "Weekend")),
         time_desc = factor(time_desc, levels = c("EarlyMorning", "Morning", "EarlyAfternoon", "Afternoon", "Evening")))


### Time periods by time description

time_periods = tibble(tcat = timeBreaks) %>%
  mutate(desc = paste(
    case_when(
      hour(tcat) %in% 0:5 ~ "EarlyMorning",
      hour(tcat) %in% 6:11 ~ "Morning",
      hour(tcat) %in% 12:14 ~ "EarlyAfternoon",
      hour(tcat) %in% 15:18 ~ "Afternoon",
      hour(tcat) %in% 19:23 ~ "Evening",
      TRUE ~ "NA"
    ), case_when(
      wday(tcat) %in% 6 ~ "Weekend", #saturday
      (wday(tcat) == 5) & hour(tcat) %in% 15:23 ~ "Weekend", #friday afternoon + evening
      (wday(tcat) == 7) & hour(tcat) %in% 0:14 ~ "Weekend", #sunday morning
      TRUE ~ "Weekday",
    ),
    sep = ""
  ),
  time_desc =  case_when(
    hour(tcat) %in% 0:5 ~ "EarlyMorning",
    hour(tcat) %in% 6:11 ~ "Morning",
    hour(tcat) %in% 12:14 ~ "EarlyAfternoon",
    hour(tcat) %in% 15:18 ~ "Afternoon",
    hour(tcat) %in% 19:23 ~ "Evening",
    TRUE ~ "NA"
  ),
  day_desc = case_when(
    wday(tcat) %in% 6 ~ "Weekend", #saturday
    (wday(tcat) == 5) & hour(tcat) %in% 15:23 ~ "Weekend", #friday afternoon + evening
    (wday(tcat) == 7) & hour(tcat) %in% 0:14 ~ "Weekend", #sunday morning
    TRUE ~ "Weekday",
  ))

month_ind = rep(0, length(timeBreaks))
for(j in 1:length(timeBreaks)){
  for(k in 1:(length(month_range)-1)){
    if((month_range[k] <= timeBreaks[j]) & (month_range[k+1] >= timeBreaks[j])){
      month_ind[j] = k
    }
  }
}
time_periods$month = month_ind

time_periods_all = time_periods %>%
  mutate(t_length = lead(as.numeric(tcat))-as.numeric(tcat)) %>%
  filter(month > 0) %>%
  mutate(desc = factor(desc, levels = c("EarlyMorningWeekday", "MorningWeekday", "EarlyAfternoonWeekday", "AfternoonWeekday", "EveningWeekday", "EarlyMorningWeekend", "MorningWeekend", "EarlyAfternoonWeekend", "AfternoonWeekend", "EveningWeekend")),
         day_desc = factor(day_desc, levels = c("Weekday", "Weekend")),
         time_desc = factor(time_desc, levels = c("EarlyMorning", "Morning", "EarlyAfternoon", "Afternoon", "Evening")))



## Estimate demand

demands_table_sum = demands_table %>%
  group_by(cell, month) %>%
  summarise(n_hires = sum(n_pickups), 
            Tj = sum(nzi_end-nzi_start)/1000, 
            demand = n_hires/Tj, 
            bayes_demand = n_hires^2/(as.numeric(diff(day_range), unit = "secs") + n_hires*Tj)
  ) %>%
  ungroup()




# Bayesian vs. MLE Estimates

demands_table_sum %>%
  group_by(cell) %>%
  summarise(scooter_time = sum(Tj), n_hires = sum(n_hires), naive_demand = n_hires/(as.numeric(diff(day_range), unit = "secs")), demand = n_hires/scooter_time, bayesian_demand = sum(n_hires)^2/(as.numeric(diff(day_range), unit = "secs") + sum(n_hires)*sum(Tj))) %>%
  ggplot(aes(x = scooter_time, y = bayesian_demand/demand)) + geom_point() + scale_x_log10() + theme_bw() + labs(x = "Time with scooters in cell (s)", y = "(Bayesian demand)/(Availability-corrected demand)")
ggsave("./demand_ratio_time_grid_100.pdf", height = 4, width = 6)


## Estimate demand - by time of day

time_periods = time_periods_all %>% 
  group_by(month, day_desc, time_desc, desc) %>%
  summarise(t_length = sum(t_length)) %>% # get total time for each time category for each month
  ungroup()


demands_table_tod_output = demands_table %>%
  mutate(interval_length = (nzi_end-nzi_start)/1000) %>%
  group_by(cell, month, desc) %>%
  summarise(time_desc = first(time_desc),
            day_desc = first(day_desc),
            n_hires = sum(n_pickups), 
            Tj = sum(interval_length)) %>%
  ungroup() %>%
  left_join(time_periods, by = c("month", "desc", "time_desc", "day_desc")) %>%
  mutate(
    naive_demand = 3600*n_hires/t_length,
    demand = 3600*n_hires/Tj, 
    bayes_demand = 3600*n_hires^2/(t_length + n_hires*Tj)
  )

demands_table_sum_tod = demands_table %>%
  mutate(interval_length = (nzi_end-nzi_start)/1000) %>%
  group_by(cell, month, desc) %>%
  summarise(time_desc = first(time_desc),
            day_desc = first(day_desc),
            n_hires = sum(n_pickups), 
            Tj = sum(interval_length)) %>%
  ungroup() %>%
  left_join(time_periods, by = c("month", "desc", "time_desc", "day_desc")) %>%
  mutate(
    naive_demand = 3600*n_hires/t_length,
    demand = 3600*n_hires/Tj, 
    bayes_demand = 3600*n_hires^2/(t_length + n_hires*Tj)
  ) %>%
  group_by(time_desc, day_desc, desc, month) %>%
  summarise(naive_demand = sum(naive_demand),
            demand = sum(demand),
            bayes_demand = sum(bayes_demand))

# By month
time_periods_month_only = time_periods_all %>%
  group_by(month) %>%
  summarise(t_length = sum(t_length)) %>% # get total time for each time category for each month
  ungroup()


demands_table_sum_month_only = demands_table %>%
  mutate(interval_length = (nzi_end-nzi_start)/1000) %>%
  group_by(cell, month) %>%
  summarise(n_hires = sum(n_pickups), 
            Tj = sum(interval_length)) %>%
  ungroup() %>%
  left_join(time_periods_month_only, by = c("month")) %>%
  mutate(
    naive_demand = 3600*n_hires/t_length,
    demand = 3600*n_hires/Tj, 
    bayes_demand = 3600*n_hires^2/(t_length + n_hires*Tj)
  ) %>%
  group_by(month) %>%
  summarise(naive_demand = sum(naive_demand),
            demand = sum(demand),
            bayes_demand = sum(bayes_demand))

## by time of day

time_periods_tod_only = time_periods_all %>%
  group_by(time_desc) %>%
  summarise(t_length = sum(t_length)) %>% # get total time for each time category for each month
  ungroup()


demands_table_sum_tod_only = demands_table %>%
  mutate(interval_length = (nzi_end-nzi_start)/1000) %>%
  group_by(cell, time_desc) %>%
  summarise(n_hires = sum(n_pickups), 
            Tj = sum(interval_length)) %>%
  ungroup() %>%
  left_join(time_periods_tod_only, by = c("time_desc")) %>%
  mutate(
    naive_demand = 3600*n_hires/t_length,
    demand = 3600*n_hires/Tj, 
    bayes_demand = 3600*n_hires^2/(t_length + n_hires*Tj)
  ) %>%
  group_by(time_desc) %>%
  summarise(naive_demand = sum(naive_demand),
            demand = sum(demand),
            bayes_demand = sum(bayes_demand))

## by day of week

time_periods_day_only = time_periods_all %>%
  group_by(day_desc) %>%
  summarise(t_length = sum(t_length)) %>% # get total time for each time category for each month
  ungroup()


demands_table_sum_day_only = demands_table %>%
  mutate(interval_length = (nzi_end-nzi_start)/1000) %>%
  group_by(day_desc) %>%
  summarise(n_hires = sum(n_pickups), 
            Tj = sum(interval_length)) %>%
  ungroup() %>%
  left_join(time_periods_day_only, by = c("day_desc")) %>%
  mutate(
    naive_demand = 3600*n_hires/t_length,
    demand = 3600*n_hires/Tj, 
    bayes_demand = 3600*n_hires^2/(t_length + n_hires*Tj)
  ) %>%
  group_by(day_desc) %>%
  summarise(naive_demand = sum(naive_demand),
            demand = sum(demand),
            bayes_demand = sum(bayes_demand))

## by day of week + time of day

time_periods_tod_day = time_periods_all %>%
  group_by(time_desc, day_desc, desc) %>%
  summarise(t_length = sum(t_length)) %>% # get total time for each time category for each month
  ungroup()

demands_table_sum_tod_day = demands_table %>%
  mutate(interval_length = (nzi_end-nzi_start)/1000) %>%
  group_by(cell, time_desc, day_desc, desc) %>%
  summarise(n_hires = sum(n_pickups), 
            Tj = sum(interval_length)) %>%
  ungroup() %>%
  left_join(time_periods_tod_day, by = c("time_desc", "day_desc", "desc")) %>%
  mutate(
    naive_demand = 3600*n_hires/t_length,
    demand = 3600*n_hires/Tj, 
    bayes_demand = 3600*n_hires^2/(t_length + n_hires*Tj)
  ) %>%
  group_by(time_desc, day_desc, desc) %>%
  summarise(naive_demand = sum(naive_demand),
            demand = sum(demand),
            bayes_demand = sum(bayes_demand))

#Plots of overall demands

demands_table_sum_month_only %>%
  mutate(month = month_range[month]) %>%
  rename(`Bayesian Demand` = bayes_demand, Month = month) %>%
  ggplot(aes(x = Month, y = `Bayesian Demand`)) + geom_col(position = "dodge") + theme_bw()
ggsave("./bayes_demand_beam_month.pdf", width = 6, height = 4)

demands_table_sum_tod_only %>%
  rename(`Bayesian Demand` = bayes_demand, Time = time_desc) %>%
  ggplot(aes(x = Time, y = `Bayesian Demand`)) + geom_col(position = "dodge") + theme_bw()
ggsave("./bayes_demand_beam_time.pdf", width = 6, height = 4)

demands_table_sum_day_only %>%
  rename(`Bayesian Demand` = bayes_demand, Day = day_desc) %>%
  ggplot(aes(x = Day, y = `Bayesian Demand`)) + geom_col(position = "dodge") + theme_bw()
ggsave("./bayes_demand_beam_day.pdf", width = 6, height = 4)

demands_table_sum_tod_day %>%
  rename(`Bayesian Demand` = bayes_demand, Day = day_desc, Time = time_desc) %>%
  ggplot(aes(x = Time, y = `Bayesian Demand`, fill = Day)) + geom_col(position = "dodge") + theme_bw()
ggsave("./bayes_demand_beam_timeday.pdf", width = 6, height = 4)

demands_table_sum_tod %>%
  mutate(month = month_range[month]) %>%
  ggplot(aes(x = month, y = bayes_demand, fill = desc)) + geom_col(position = "dodge") + theme_bw()
ggsave("./bayes_demand_beam_time_oneplot.pdf", width = 7, height = 4)

demands_table_sum_tod %>%
  mutate(month = month_range[month]) %>%
  rename(Time = desc, `Bayesian Demand` = bayes_demand, Month = month) %>%
  ggplot(aes(x = Month, y = `Bayesian Demand`)) + geom_col(position = "dodge") + theme_bw() + facet_wrap(~Time, nrow = 2)# + coord_flip()
ggsave("./bayes_demand_beam_time_multiplot.pdf", width = 14, height = 7)


demands_table_tod_output %>% write.csv(file = "./demands_by_time.csv")
