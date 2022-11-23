bin_coords = function(latitude, longitude, n = 10, minlat = min(latitude), maxlat = max(latitude), minlon = min(longitude), maxlon = max(longitude)){
  # Function to bin geospatial data into n square bins on each axis
  # INPUTS:
    # latitude = vector of latitude values
    # longitude = vector of longitude values
    # n = number of bins on each axis
    # minlat, maxlat = minimum/maximum latitude value; by default, the minimum/maximum value of the latitude vector
    # minlon, maxlon = minimum/maximum longitude value; by default, the minimum/maximum value of the longitude vector
  # OUTPUTS:
    # List containing:
      # Dataframe of latitude, longitude, latitude index and longitude index
      # Dataframe of the break coordinates
  # LAST UPDATED: 201/09/15
  # Scott Carnie-Bronca
  
  # Packages
  library(tidyverse)
  
  
  # Construct bin breaks
  latBreaks = seq(minlat, maxlat, length.out = n + 1)
  lonBreaks = seq(minlon, maxlon, length.out = n + 1)
  
  # Bin the coordinate data
  coords = tibble(latitude = latitude, longitude = longitude) %>%
    mutate(latitudeInd = findInterval(latitude, latBreaks),
           longitudeInd = findInterval(longitude, lonBreaks))
  
  # Construct dataframe of the breaks
  coordBreaks = tibble(latBreaks = latBreaks, lonBreaks = lonBreaks)
  
  # Return dataframes
  return(list(coords,coordBreaks))
}