
set.seed(2021)


library(tidyverse)
library(lubridate)
library(sp)

options("lubridate.week.start"=1) #set first day of week to Monday instead of Sunday 

get_manhattan_distance_grid <- function(lat, lon, lat2, lon2){
# Vector inputs
  lat_dist = spDists(cbind(lat, lon), cbind(lat2, lon), longlat = FALSE, diagonal = TRUE)
  lon_dist = spDists(cbind(lat2, lon), cbind(lat2, lon2), longlat = FALSE, diagonal = TRUE)
  return(lat_dist + lon_dist) 
}


## Load data
n = 100 #number of cells per axis

tz = "Australia/Adelaide"
trips_data = read.csv("../Likelihood Estimation/trips_binned_beam.csv", row.names = 1)
status_data = read.csv("../Likelihood Estimation/status_binned_beam.csv", row.names = 1)

#Time descriptors:
timeDesc = expand_grid(time = 0:23, day = 1:7) %>%
  mutate(desc = paste(
    case_when(
      time %in% 0:5 ~ "EarlyMorning",
      time %in% 6:11 ~ "Morning",
      time %in% 12:14 ~ "EarlyAfternoon",
      time %in% 15:18 ~ "Afternoon",
      time %in% 19:23 ~ "Evening",
      TRUE ~ "NA"
    ), case_when(
      day %in% c(6,7) ~ "Weekend",
      day %in% 1:5 ~ "Weekday",
      TRUE ~ "NA"
    ),
    sep = ""
  ))

#Redistribution estimates:

filenames = list.files(path = "../Likelihood Estimation/Demands", pattern = "beam")
filenames = filenames[str_detect(filenames, "redists")]
(nTimePairs = length(filenames))
redists = array(dim = c(n, n, length(unique(timeDesc$desc))))
for(k in 1:nTimePairs){
  redists[ , ,k]  = read.csv(paste("../Likelihood Estimation/Demands/", filenames[k], sep = ""), header = FALSE) %>% as.matrix()
}
dimnames(redists)[[3]] <- gsub(".csv|_|beam|redists", "", filenames)

# Demand estimates:

demand_df = read.csv("../Likelihood Estimation/demands_by_time.csv", row.names = 1) %>% as_tibble

n_months = length(unique(demand_df$month))
n_desc = length(unique(demand_df$desc))

demand_df = demand_df %>% 
  left_join(
    timeDesc %>% 
      distinct(desc) %>% 
      mutate(desc_ind = 1:n_desc)
    )

timeDesc = timeDesc %>%
  left_join(
    unique(select(demand_df, desc, desc_ind)),
    by = "desc"
  )

demands = array(0, dim = c(n^2, n_desc, n_months))

for(k in 1:nrow(demand_df)){
  demands[demand_df$cell[k]+1, demand_df$desc_ind[k], demand_df$month[k]] <- demand_df$bayes_demand[k]
}

demands = demands/3600 # Convert to hires/sec instead of hires/hr


#Trip speed:

speed_cell = scan("./estimated_speed.txt")


#Get trip probabilities:

transProbs = trips_data %>% 
  group_by(pickup_cell_num) %>%
  mutate(n_pickups = n()) %>%
  group_by(dropoff_cell_num, .add = TRUE) %>%
  summarise(p = n()/mean(n_pickups))

redist_dest_probs = transProbs 




#Keep current status and next event in a data frame, per scooter. Every time the next event is reached, update that particular row.

#Functions:
getHireTime = function(cell, demands, t, month){ #can take a vector of cells (1-n^2)
  t2 = as.POSIXct(t/1000, tz = tz, origin = lubridate::origin)

  l = demands[cell+1, timeDesc$desc_ind[(timeDesc$time %in% hour(t2)) & (timeDesc$day %in% wday(t2))], month]
  
  out = l
  
  for(k_hire in 1:length(l)){
    if(l[k_hire] > 0){
    out[k_hire] = rexp(1, l[k_hire])
  } else {
    out[k_hire] = 14*24*3600
  }
  }
  return(t + 1000*out)
}

getRedistTime = function(cell, demands, t){ #can take a vector of cells (1-n^2)
  t2 = as.POSIXct(t/1000, tz = tz, origin = lubridate::origin)
  l = demands[((cell) %% n) + 1, ((cell) %/% n) + 1, timeDesc$desc_ind[(timeDesc$time %in% hour(t2)) & (timeDesc$day %in% wday(t2))]]
  
  out = l
  for(k_hire in 1:length(l)){
    if(l[k_hire] > 0){
    out[k_hire] = rexp(1, l[k_hire])
  } else {
    out[k_hire] = 14*24*3600
  }
  }
  return(t + 1000*out)
}

getTripTime = function(cell, dest, speed_cell){ #cell, dest are cell indices from 1 to n^2
  dist = get_manhattan_distance_grid(((cell) %% n) + 1, ((cell) %/% n) + 1, ((dest) %% n) + 1, ((dest) %/% n) + 1)
  #print(paste0("dist_in_cells:", dist, "time:", dist/speed_cell))
  return(dist/speed_cell)
  

}
  
getNextDest = function(cell, transProbs){
  out = cell
  for(k in 1:length(cell)){
    temp = transProbs[transProbs$pickup_cell_num == cell,]
    if(nrow(temp) == 0){
      out[k] = sample(unique(transProbs$dropoff_cell_num), size = 1)
    } else if(nrow(temp) == 1){
      out[k] = temp$dropoff_cell_num
    } else {
      out[k] = sample(temp$dropoff_cell_num, size = 1, prob = temp$p)
    }
    
  }
  out = replace(out, is.nan(out), sample(c(1:n^2), 1))
  return(out)
}


getNextEvent = function(cell, demands, redists, t, month){ #Choose between a pickup or redistribution
  t2 = as.POSIXct(t/1000, tz = tz, origin = lubridate::origin)
  pickupRate = demands[cell+1, timeDesc$desc_ind[timeDesc$time %in% hour(t2) & timeDesc$day %in% wday(t2)], month]
  redistRate = redists[1+((cell-1)%%n), floor(1+(cell-1)/n), paste("demand", timeDesc$desc[timeDesc$time %in% hour(t2) & timeDesc$day %in% wday(t2)], sep = "")]
  
  if(pickupRate == 0 & redistRate == 0) return("pickup")
  r = runif(1)
  if(r < pickupRate/(pickupRate + redistRate)){
    return("pickup")
  } else {
    return("redistribution")
  }
}

updateScooterPickup = function(scooters, scooter, t, transProbs, speed_cell){ #Function to update specific row of scooters data frame in simulation, if a pickup occurs

  scooters$location[scooters$id == scooter] = scooters$nextEventLocation[scooters$id == scooter]
  scooters$nextEvent[scooters$id == scooter] = "dropoff"
  scooters$nextEventLocation[scooters$id == scooter] = getNextDest(scooters$location[scooters$id == scooter], transProbs)
  scooters$nextEventTime[scooters$id == scooter] = t + getTripTime(scooters$location[scooters$id == scooter], scooters$nextEventLocation[scooters$id == scooter], speed_cell)
  scooters$status[scooters$id == scooter] = "in_use"
  scooters$lastChanged[scooters$id == scooter] = t
  scooters$timesChanged[scooters$id == scooter] = scooters$timesChanged[scooters$id == scooter] + 1
  return(scooters)
}

updateScooterDropoff = function(scooters, scooter, t, demands, redists, month){ #Function to update specific row of scooters data frame in simulation, if a dropoff occurs

  scooters$location[scooters$id == scooter] = scooters$nextEventLocation[scooters$id == scooter]
  scooters$nextEvent[scooters$id == scooter] = getNextEvent(scooters$location[scooters$id == scooter], demands, redists, t, month)
  scooters$nextEventLocation[scooters$id == scooter] = scooters$location[scooters$id == scooter]
  if(scooters$nextEvent[scooters$id == scooter] == "pickup"){
    scooters$nextEventTime[scooters$id == scooter] = getHireTime(scooters$location[scooters$id == scooter], demands, t, month) 
  } else {
    scooters$nextEventTime[scooters$id == scooter] = getRedistTime(scooters$location[scooters$id == scooter], redists, t)
  }
  scooters$status[scooters$id == scooter] = "available"
  
  #if(scooters$nextEventTime[scooters$id == scooter] > next_break(t)){
  #  scooters$nextEvent[scooters$id == scooter] = "idle"
  #  scooters$nextEventTime = next_break(t)
  #}
  
  scooters$lastChanged[scooters$id == scooter] = t
  scooters$timesChanged[scooters$id == scooter] = scooters$timesChanged[scooters$id == scooter] + 1
  return(scooters)
}

updateScooterRedistribution = function(scooters, scooter, t, demands, redists, month){
  scooters$location[scooters$id == scooter] = scooters$nextEventLocation[scooters$id == scooter]
  scooters$nextEvent[scooters$id == scooter] = "redistribution_dropoff"
  scooters$nextEventLocation[scooters$id == scooter] = getNextDest(scooters$location[scooters$id == scooter], redist_dest_probs)
  scooters$nextEventTime[scooters$id == scooter] = t + 30*60*1000 
  scooters$status[scooters$id == scooter] = "redistributing"
  scooters$lastChanged[scooters$id == scooter] = t
  scooters$timesChanged[scooters$id == scooter] = scooters$timesChanged[scooters$id == scooter] + 1
  return(scooters)
}

updateScooterRedistributionDropoff = function(scooters, scooter, t, demands, redists, month){
  scooters$location[scooters$id == scooter] = scooters$nextEventLocation[scooters$id == scooter]
  scooters$nextEvent[scooters$id == scooter] = getNextEvent(scooters$location[scooters$id == scooter], demands, redists, t, month)
  scooters$nextEventLocation[scooters$id == scooter] = scooters$location[scooters$id == scooter]
    if(scooters$nextEvent[scooters$id == scooter] == "pickup"){
    scooters$nextEventTime[scooters$id == scooter] = getHireTime(scooters$location[scooters$id == scooter], demands, t, month) 
  } else {
    scooters$nextEventTime[scooters$id == scooter] = getRedistTime(scooters$location[scooters$id == scooter], redists, t)
  }
  scooters$status[scooters$id == scooter] = "available"
  
  #if(scooters$nextEventTime[scooters$id == scooter] > next_break(t)){
  #  scooters$nextEvent[scooters$id == scooter] = "idle"
  #  scooters$nextEventTime[scooters$id == scooter] = next_break(t)
  #}
  
  scooters$lastChanged[scooters$id == scooter] = t
  scooters$timesChanged[scooters$id == scooter] = scooters$timesChanged[scooters$id == scooter] + 1
  return(scooters)
}

updateScooterIdle = function(scooters, scooter, t, demands, redists, month){ #Function to update specific row of scooters data frame in simulation, if it is idle (sitting unused through a time break)
  scooters$location[scooters$id == scooter] = scooters$nextEventLocation[scooters$id == scooter]
  scooters$nextEvent[scooters$id == scooter] = getNextEvent(scooters$location[scooters$id == scooter], demands, redists, t, month)
  scooters$nextEventLocation[scooters$id == scooter] = scooters$location[scooters$id == scooter]
  if(scooters$nextEvent[scooters$id == scooter] == "pickup"){
    scooters$nextEventTime[scooters$id == scooter] = getHireTime(scooters$location[scooters$id == scooter], demands, t, month) 
  } else {
    scooters$nextEventTime[scooters$id == scooter] = getRedistTime(scooters$location[scooters$id == scooter], redists, t)
  }
  scooters$status[scooters$id == scooter] = "available"
  
  if(scooters$nextEventTime[scooters$id == scooter] > next_break(t)){
    scooters$nextEvent[scooters$id == scooter] = "idle"
    scooters$nextEventTime[scooters$id == scooter] = next_break(t)
  }
  
  scooters$lastChanged[scooters$id == scooter] = t
  scooters$timesChanged[scooters$id == scooter] = scooters$timesChanged[scooters$id == scooter] + 1
  return(scooters)
}


#Set up scooter dataframe:

nScooters = 1400
nSteps = 2e5  #number of simulated events/steps. this refers to the underlying simulation, not t_regular output
o = min(status_data$location_timestamp) #starting time of simulations
month = 9 #month that the simulations take place in

timeBreaks = seq(floor_date(as.POSIXct(o/1000, tz = tz, origin = origin), unit = "day"), by = "DSTday", length.out = 28) #max time period = length.out days
timeBreaks = rep(timeBreaks, each = 5) + rep(c(0, 6*3600, 12*3600, 15*3600, 19*3600), times = length(timeBreaks))

next_break = function(t){
  return(1000*as.numeric(timeBreaks[findInterval(t, 1000*as.numeric(timeBreaks), all.inside = TRUE) + 1]))
}
initial_points = trips_data$pickup_cell_num %>% table

# Create scooter status data frame
id = as.character(c(1:nScooters)) #NOTE: Update functions require sequential integer IDs
location = sample(as.numeric(names(initial_points)), nScooters, replace = TRUE, prob = initial_points) #initial cell number - random, uniform sample with prob based on data probabilities
nextEvent = sample(c("pickup", "dropoff"), size = nScooters, prob = c(0.95, 0.05), replace = TRUE)
nextEventTime = getHireTime(location, demands, o, month)
nextEventLocation = location
status = case_when(nextEvent == "pickup" ~ "available", nextEvent == "dropoff" ~ "in_use", TRUE ~ "available")
lastChanged = rep(0, nScooters)
timesChanged = lastChanged

nextEvent = case_when(
  nextEvent == "dropoff" ~ "dropoff",
  nextEvent == "pickup" & nextEventTime < next_break(o) ~ "pickup",
  TRUE ~ "idle"
)

scooters = tibble(id, location, nextEvent, nextEventTime, nextEventLocation, status, lastChanged, timesChanged)



#Simulation code (one sim, one trip time method)
t = rep(o, nSteps+1) #store the times
t_regular = 0 #regular, hourly "snapshots"
propInUse = 0
propOutsidePickupArea = 0
totalRevenue = 0
revenue_regular = 0
n_trips = 0
n_trips_regular = 0
flat_fee = 1
fee_per_min = 0.38

dt = 30000 #time step in seconds*1000, e.g. 30000 for 30s. for t_regular output

for(k in 1:nSteps){
  t[k+1] = min(scooters$nextEventTime, o + t_regular[length(t_regular)]+dt)
  if(t[k+1] == o + t_regular[length(t_regular)]+dt){
    t_regular = c(t_regular, t_regular[length(t_regular)]+dt)
    propInUse = c(propInUse, sum(scooters$status == "in_use")/nScooters)
    revenue_regular = c(revenue_regular, totalRevenue)
    n_trips_regular = c(n_trips_regular, n_trips)
    propOutsidePickupArea = c(propOutsidePickupArea, sum((scooters$nextEventTime - scooters$lastChanged) > 1e9-1)/nScooters) #if the scooter's hire time is too large
  }
  if(as.POSIXct(t[k+1]/1000, tz = tz, origin = origin) >= timeBreaks[length(timeBreaks)]){break} #end simulations if the final time is reached
  updateScooters = scooters$id[scooters$nextEventTime == t[k+1]]
  #print(paste("t =", t[k+1], "  propInUse =", sum(scooters$status == "in_use")/nScooters))
  for(scooter in updateScooters){
    #print(as.character(scooters[scooters$id == scooter,]))
    if(scooters$nextEvent[scooters$id == scooter] == "pickup"){ #if the scooter is being picked up
      scooters = updateScooterPickup(scooters, scooter, t[k+1], transProbs, speed_cell)
      totalRevenue = totalRevenue + flat_fee + fee_per_min*(scooters$nextEventTime[scooters$id == scooter] - scooters$lastChanged[scooters$id == scooter])/1000
      n_trips = n_trips + 1
      next
    } else if(scooters$nextEvent[scooters$id == scooter] == "dropoff"){ #if the scooter is being dropped off
      scooters = updateScooterDropoff(scooters, scooter, t[k+1], demands, redists, month)
      next
    } else if(scooters$nextEvent[scooters$id == scooter] == "redistribution"){ #if the scooter is being redistributed
      scooters = updateScooterRedistribution(scooters, scooter, t[k+1], demands, redists)
      next
    } else if(scooters$nextEvent[scooters$id == scooter] == "redistribution_dropoff"){ #if the scooter is being redistributed
      scooters = updateScooterRedistributionDropoff(scooters, scooter, t[k+1], demands, redists, month)
      next
    } else if(scooters$nextEvent[scooters$id == scooter] == "idle"){ #if the scooter is idle across a time break
      
      scooters = updateScooterIdle(scooters, scooter, t[k+1], demands, redists, month)
      #print(as.character(scooters[scooters$id == scooter,]))
      #stop("idle")
      next
    }
  }
}


#Plots:

# Dist of locations
scooters %>% ggplot(aes(x = location, col = status)) + geom_bar()

x_reg = tibble(t = t_regular/1000, p_in_use = propInUse, n_trips = n_trips_regular, rev = revenue_regular)

# Proportion in use over time
x_reg %>% ggplot(aes(x = t, y = p_in_use)) + geom_point() + theme_bw()
ggsave("./prop_in_use.pdf", width = 6, height = 4)

x_reg %>% ggplot(aes(x = t, y = n_trips)) + geom_point() + theme_bw()
ggsave("./n_trips.pdf", width = 6, height = 4)

x_reg %>% ggplot(aes(x = t, y = rev)) + geom_point() + theme_bw()
ggsave("./revenue.pdf", width = 6, height = 4)

write.csv(x_reg, "./regular_indicators.csv")
