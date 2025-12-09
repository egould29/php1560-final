# Trip-Level Delay Analysis
library(dplyr)

source("src/utils.R")

get_trip_delay <- function(trip_data) {
  if (!is.data.frame(trip_data)) {
    stop("Argument must be of type 'data frame'.")
  }
  
  trip_data <- arrange(trip_data, Stop.Sequence)
  delay_changes <- c(0)
  
  for (i in 1:(nrow(trip_data)-1)) {
    change <- trip_data$Delay.Sec[i+1] - trip_data$Delay.Sec[i]
    delay_changes <- c(delay_changes, change)
  }
  
  return(delay_changes)
}

calculate_delays <- function(data) {
  data <- data %>%
    mutate(Delta.Delay = rep(0, nrow(data)))
  
  i <- 1
  while (i <= nrow(data)) {
    curr_trip <- get_trip(data, data$Trip[i], data$Date[i])
    trip_length <- nrow(curr_trip)
    
    data$Delta.Delay[i:(i + trip_length - 1)] <- get_trip_delay(curr_trip)
    
    i <- i + trip_length
  }
  
  return(data)
}