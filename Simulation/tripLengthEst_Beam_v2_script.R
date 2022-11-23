library(tidyverse)
library(lubridate)
library(sp)

n = 100

df = as_tibble(read.csv("../Likelihood Estimation/trips_binned_beam.csv", row.names = 1))


get_manhattan_distance_grid <- function(lat, lon, lat2, lon2){
# Vector inputs
  lat_dist = spDists(cbind(lat, lon), cbind(lat2, lon), longlat = FALSE, diagonal = TRUE)
  lon_dist = spDists(cbind(lat2, lon), cbind(lat2, lon2), longlat = FALSE, diagonal = TRUE)
  return(lat_dist + lon_dist) 
}

df = df %>%
  mutate(distance_manhattan_grid = get_manhattan_distance_grid(start_lat_ind, start_lon_ind, end_lat_ind, end_lon_ind))

# LMs
speed.lm5 = lm(trip_duration ~ distance_manhattan_grid - 1, data = df)

speed_estimate = 1/unname(speed.lm5$coefficients)

write(speed_estimate, "./estimated_speed.txt")

