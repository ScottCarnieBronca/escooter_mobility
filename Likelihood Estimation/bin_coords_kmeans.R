bin_coords_kmeans = function(coords, k = 100){
  # Function to bin geospatial data into n square bins on each axis
  # INPUTS:
  # coords = data frame with a column of latitude ($lat) and a column of longitude ($lon)
  # k = number of bins
  # minlat, maxlat = minimum/maximum latitude value; by default, the minimum/maximum value of the latitude vector
  # minlon, maxlon = minimum/maximum longitude value; by default, the minimum/maximum value of the longitude vector
  # OUTPUTS:
  # List containing:
  # Dataframe of latitude, longitude, and cluster index
  # Dataframe of the centres of the k clusters
  # LAST UPDATED: 2020/07/23
  # Scott Carnie-Bronca
  
  # Packages
  library(tidyverse)
  
  L = kmeans(select(coords, lat, lon), k)
  
  coords = coords %>%
    mutate(cluster = L$cluster)
}