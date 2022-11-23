
# LAST UPDATED: 2022-03-04

set.seed(2022)



library(tidyverse)
library(lubridate)
library(sp)
library(foreach)
library(parallel)

# # CORE NUMBER CHECKING FOR PHOENIX
slurm_ncores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK"))
n_cores <- as.integer(Sys.getenv("SLURM_JOB_CPUS_PER_NODE"))

#n_cores = detectCores()

#n_cores = 8

doParallel::registerDoParallel(cores = n_cores)

options("lubridate.week.start"=1) #set first day of week to Monday instead of Sunday 


# Setup 

## Parameters
month = 9
t0 = as.numeric(ymd_hms("2021-01-02 18:00:00", tz = "Australia/Adelaide"))#initial time. Used for time of day/day of week, not necessarily in month
n_simulations = 100 #number of simulations to run

n = 100 #grid size
flat_fee = 1 
fee_per_sec = 0.45/60
n_scooters = 1400 #number of scooters in the system. Requires sum(initial_deployment) == n_scooters, otherwise returns NA. Needs to be > 1
n_steps = 1e5 #maximum number of trips in the simulation
t_final = 21600 #final time after o, in seconds
dt = 21600 #time step for output
variation_amount = 1000 #scale for variation; on average, this many scooters multiplied by Tk/T0 move
n_optim_iter = 40000
T_0 = 100 #initial temperature
alph = T_0^(-4/n_optim_iter) #alpha in simulated annealing; T_k = alph*T_{k-1}. T_0^(-2/n_optim_iter) for T_k = 1 halfway


## Load data

tz = "Australia/Adelaide"
trips_data = read.csv("../Likelihood Estimation/trips_binned_beam.csv", row.names = 1)
status_data = read.csv("../Likelihood Estimation/status_binned_beam.csv", row.names = 1)


## Time descriptions
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
      day %in% 6 ~ "Weekend", #saturday
      day == 5 & time %in% 15:23 ~ "Weekend", #friday afternoon + evening
      day == 7 & time %in% 0:14 ~ "Weekend", #sunday morning
      TRUE ~ "Weekday",
    ),
    sep = ""
  ),
  time_desc =  case_when(
    time %in% 0:5 ~ "EarlyMorning",
    time %in% 6:11 ~ "Morning",
    time %in% 12:14 ~ "EarlyAfternoon",
    time %in% 15:18 ~ "Afternoon",
    time %in% 19:23 ~ "Evening",
    TRUE ~ "NA"
  ),
  day_desc = case_when(
    day %in% 6 ~ "Weekend", #saturday
    (day == 5) & time %in% 15:23 ~ "Weekend", #friday afternoon + evening
    (day == 7) & time %in% 0:14 ~ "Weekend", #sunday morning
    TRUE ~ "Weekday"
  ))

## Demand estimates
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

demands = demands[,,month] # Select only the required month

demands = demands/3600 # Convert to hires/sec instead of hires/hr


#Trip speed:

speed_cell = scan("../Simulation/estimated_speed.txt")


transProbs = trips_data %>% 
  group_by(pickup_cell_num) %>%
  mutate(n_pickups = n()) %>%
  group_by(dropoff_cell_num, .add = TRUE) %>%
  summarise(p = n()/mean(n_pickups))

 



## Functions:

get_manhattan_distance_grid <- function(lat, lon, lat2, lon2){
# Vector inputs
  lat_dist = spDists(cbind(lat, lon), cbind(lat2, lon), longlat = FALSE, diagonal = TRUE)
  lon_dist = spDists(cbind(lat2, lon), cbind(lat2, lon2), longlat = FALSE, diagonal = TRUE)
  return(lat_dist + lon_dist) 
}

getHireTime = function(cell, demands, t, time_breaks, month){ #can take a vector of cells (1-n^2)
  if(t >= max(time_breaks)){return(Inf)} #if the current time if after the maximum time (primarily for recursive hire time calcs)
  t2 = as.POSIXct(t, tz = tz, origin = lubridate::origin)
  l = demands[cell+1, timeDesc$desc_ind[(timeDesc$time %in% hour(t2)) & (timeDesc$day %in% wday(t2))]]
  if(l > 0){
    out = rexp(1, l)
    if(out > time_breaks[min(which(time_breaks > t))]-t){ # if it goes over the next break point
      out = getHireTime(cell, demands, time_breaks[min(which(time_breaks > t))], time_breaks, month) #recurse
      return(out)
    }
  } else {
    out = Inf #the scooter is not hired
  }
  return(t + out)
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

## Function to run a simulation and return the revenue over a time window

single_sim_trips = function(o, t_final, n_steps, n, flat_fee, fee_per_sec, month){
  
  # Time breaks (for non-homogeneous demand)
  time_breaks = seq(floor_date(as.POSIXct(o, tz = tz, origin = origin), unit = "day"), by = "DSTday", length.out = ceiling(t_final/86400))
  time_breaks = rep(time_breaks, each = 5) + rep(c(0, 6*3600, 12*3600, 15*3600, 19*3600), times = length(time_breaks))
  time_break_desc = foreach(k = 1:length(time_breaks), .combine = c) %do% {timeDesc$desc_ind[timeDesc$time == hour(time_breaks[k]) & timeDesc$day == wday(time_breaks[k])]}
  time_breaks = as.numeric(time_breaks)
  names(time_breaks) <- time_break_desc
  
  # Initialise output
  out = tibble(t = 0, start_cell = 0, end_cell = 0, trip_duration = 0, trip_cost = 0)
  
  # Get first hire times
  next_hire = foreach(k = 0:(n^2-1), .combine = c, .packages = c("lubridate")) %do% {getHireTime(k, demands, o, time_breaks, month)}
  
  
  for(k in 1:n_steps){
    t = min(next_hire)
    if(t > o + t_final){break} #if the final time is reached
    
    cell = which(next_hire == t)
    cell = cell[1] #only select the first trip by cell number
    
    # Update output
    dest = getNextDest(cell, transProbs)
    dur = getTripTime(cell, dest, speed_cell)
    
    out = out %>%
      add_row(t = t, 
              start_cell = cell, 
              end_cell = dest,
              trip_duration = dur,
              trip_cost = flat_fee + fee_per_sec*dur)
    
    # Update next hire time
    next_hire[cell] = getHireTime(cell, demands, t, time_breaks, month)
  }
  
  out = out[-1,] #remove initialisation row
  return(out)
}

## Multiple simulations
multi_sim_trips = function(n_sim, month, t0){
  out = foreach(k = 1:n_simulations, .combine = rbind, .export = c("tz", "single_sim_trips", "getHireTime", "getNextDest", "getTripTime", "demands", "timeDesc", "transProbs", "speed_cell", "n", "n_simulations", "n_steps", "flat_fee", "fee_per_sec", "t_final", "get_manhattan_distance_grid"), .packages = c("lubridate", "tidyverse", "foreach", "sp")) %dopar% {
    df = single_sim_trips(t0, t_final, n_steps, n, flat_fee, fee_per_sec, month)
    df$k = k
    df
  }
  return(out)
}


### Test simulation

trips_test = multi_sim_trips(n_simulations, month, t0)

stopifnot(nrow(trips_test) > 0)

print("Simulating trips. Average revenue (maximum value for optimisation):")
trips_test %>% group_by(k) %>% summarise(r = sum(trip_cost, na.rm = TRUE)) %>% ungroup() %>% pull(r) %>% mean(na.rm = TRUE)


## Revenue function
get_revenue_trips = function(d_0, trips){
  max_iter = max(trips$k)
  out = foreach(j = 1:max_iter, .combine = c) %dopar% {
    revenue = 0
    d_j = d_0
    inbound_scooters = tibble(cell = -1, t = Inf)
    trip_subset = trips %>% filter(k == j)
    for(i in 1:nrow(trip_subset)){
      # Check for inbound scooters that have arrived
      
      for(h in 1:nrow(inbound_scooters)){
        if(inbound_scooters$t[h] < trip_subset$t[i]){
          d_j[inbound_scooters$cell[h]+1] = d_j[inbound_scooters$cell[h]+1] + 1
          inbound_scooters$t[h] = Inf
        }
      }
      
      # Hire if able
      if(d_j[trip_subset$start_cell[i]+1] > 0){#if a scooter is present in the cell
        revenue = revenue + trip_subset$trip_cost[i]
        d_j[trip_subset$start_cell[i]+1] = d_j[trip_subset$start_cell[i]+1]-1
        # Update inbound scooters
        inbound_scooters = inbound_scooters %>%
          add_row(cell = trip_subset$end_cell[i], 
                  t = trip_subset$t[i] + trip_subset$trip_duration[i])
      } 
    }
    
    revenue
    
  }
  return(out)
}

## Test revenue
d_0 = rep(0, n^2)
scooter_locations = sample(names(table(trips_data$pickup_cell_num)), size = n_scooters, replace = TRUE, prob = table(trips_data$pickup_cell_num))
for(k in 1:n^2){d_0[k] = sum(scooter_locations == as.character(k))}

revenue_test = get_revenue_trips(d_0, trips_test)

print("Testing revenue calculations. For each simulation:")
revenue_test
print("Average:")
mean(revenue_test)

## Optimisation

variation_function = function(para_0, fun_length, rf, temp = 1, initial_temp = 1){
  para_1 = foreach(k = 1:n^2, .combine = c) %do% {rep(k, para_0[k])}
  latlon = cbind((1+((para_1-1)%%n)), floor(1+(para_1-1)/n))
  
  scooters_to_move = sample(1:nrow(latlon), max(1, floor(min(n_scooters, rexp(1, 1/mean(rf*temp/initial_temp)))))) #random number of scooters to shift
  for(k in scooters_to_move){
    # Select distances in each direction
    dists = rbinom(2, 1, 0.5)*(rbinom(2, 1, 0.5)-0.5)*2 #+- 0 or 1 cell in each direction
    latlon[k,] = pmin(n, pmax(1, latlon[k,] + dists))
  }
  para_1 = n*(latlon[,2]-1) + latlon[,1]
  para_2 = rep(0, n^2)
  for(k in 1:n^2){para_2[k] = sum(para_1 == k)}
  return(para_2)
}

simulated_annealing = function(fun, vary_function, x_0, T_0, a, max_iter){
  #x = matrix(nrow = max_iter+1, ncol = 3 + n^2) #store the output
  
  f_out = rep(0, max_iter+1)
  f_max = rep(0, max_iter+1)
  
  fun_out = fun(x_0)
  f_km1 = fun_out #previous function value
  f_k = fun_out
  f_max[1] = f_k
  f_out = f_k
  
  x_km1 = x_0 #previous deployment
  x_k = x_0
  T_k = T_0
  
  for(k in 1:max_iter){
    #print(k) #Print the current iteration for testing.
    T_k = a*T_k
    x_k = vary_function(x_k, n^2, variation_amount, initial_temp = T_0)
    
    f_k = fun(x_k)
    
    f_out[k+1] = f_k
    f_max[k+1] = max(f_max, f_k) #running max function value
    if(f_max[k+1] == f_out[k+1]){ #if current deployment is the best so far
      x_max = x_k
    } 
      
    
    #print(f_k)
    if(f_k > f_km1){ #if the new function value is higher, accept
      f_km1 = f_k
      x_km1 = x_k
    } else if(runif(1) < exp((f_k-f_km1)/T_k)){ #if we accept a lower value
      f_km1 = f_k
      x_km1 = x_k
    } else { #do not accept, repeat the previous row
      x_k = x_km1
      f_k = f_km1
    }
    if(k %% 2000 == 0) {
      print(paste("Saving output: k = ", k, sep = ""))
      write.csv(x_k, paste("./output/PARTIAL_final_depl", month, t0, k,".csv", sep = "_"))
      write.csv(f_out, paste("./output/PARTIAL_fun_vals", month, t0, k, ".csv", sep = "_"))
      write.csv(f_max, paste("./output/PARTIAL_best_fun_vals", month, t0, k, ".csv", sep = "_"))
      write.csv(x_max, paste("./output/PARTIAL_best_deployment", month, t0, k, ".csv", sep = "_"))
    }
  }
  
  
  
  return(list(x_k, f_out, f_max, x_max))
}

get_revenue = function(x){mean(get_revenue_trips(x, trips_test))}

## Test function
optim_out = simulated_annealing(get_revenue, variation_function, d_0, T_0, alph, n_optim_iter)

fun_vals = optim_out[[2]]
d_final = optim_out[[1]]
fun_running_max = optim_out[[3]]
d_max = optim_out[[4]]

x = tibble(k = 0:n_optim_iter, fun_vals, T_k = T_0*alph^(0:n_optim_iter), f_max = fun_running_max) # table of outputs


x %>%
  ggplot(aes(x = k)) + theme_bw() +
  geom_line(aes(y = T_k)) +
  geom_line(aes(y = fun_vals), col = "blue") +
  geom_line(aes(y = fun_running_max), col = "red")
ggsave(paste("./figs/optim_outputs", month, t0, ".pdf", sep = "_"), width = 6, height = 4)


# ## Map locations
# d_final_df = tibble(d_final, cell = 0:(n^2-1))
# 
# d_final_df %>%
#   transmute(lat_ind = 1 + ((cell) %% n), 
#             lon_ind = 1 + ((cell) %/% n),
#             n_scooters = d_final) %>%
#   ggplot(aes(x = lon_ind, y = lat_ind, fill = n_scooters)) + geom_raster()
# 
# 
# tibble(value = d_0) %>%
#   mutate(cell = 0:(n^2-1)) %>%
#   transmute(lat_ind = 1 + ((cell) %% n), 
#             lon_ind = 1 + ((cell) %/% n),
#             n_scooters = value) %>%
#   ggplot(aes(x = lon_ind, y = lat_ind, fill = n_scooters)) + geom_raster()
# 
# 
# p_naive = apply(demands, c(1), mean) %>% #take mean over all time/days
#   as.vector()
# #p_naive[((0:(n^2-1))-((0:(n^2-1)) %% n))/n < 10] = 0 # Get rid of outlier values
# 
# d_naive = rep(0, n^2)
# for(k in 1:n_scooters){
#   cell = which.max(p_naive/sum(p_naive)-d_naive/k) #find the maximum difference between the demand (p) and current scooter proportion (d/k)
#   d_naive[cell] = d_naive[cell]+1
# }
# 
# 
# tibble(value = d_naive) %>%
#   mutate(cell = 0:(n^2-1)) %>%
#   transmute(lat_ind = 1 + ((cell) %% n), 
#             lon_ind = 1 + ((cell) %/% n),
#             n_scooters = value) %>%
#   ggplot(aes(x = lon_ind, y = lat_ind, fill = n_scooters)) + geom_raster()
# print("Naive revenue:")
# get_revenue(d_naive)

print("Saving output")
write.csv(d_final, paste("./output/final_depl", month, t0, ".csv", sep = "_"))
write.csv(fun_vals, paste("./output/fun_vals", month, t0, ".csv", sep = "_"))
write.csv(fun_running_max, paste("./output/best_fun_vals", month, t0, ".csv", sep = "_"))
write.csv(d_max, paste("./output/best_deployment", month, t0, ".csv", sep = "_"))
