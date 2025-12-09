# Helper functions utilized throughout the pipeline
library(dplyr)

#' @description Helper function that returns all of the rows pertaining to a
#' specific trip.
#' 
#' @param data A data frame containing the bus data.
#' @param trip_num The trip ID number.
#' @param trip_date The date of the trip.
#' 
#' @return A data frame containing every row with the given trip number and
#' date.
get_trip <- function(data, trip_num, trip_date) {
  # Check the input
  if (!is.data.frame(data) | !is.numeric(trip_num)) {
    stop("Argument(s) of incorrect type.")
  } else if (!("Trip" %in% colnames(data))) {
    stop("Data has no 'Trip' column.")
  } else if (!(trip_num %in% data$Trip)) {
    stop("Could not find trip.")
  }
  
  trip <- data %>%
    filter(Trip == trip_num, Date == trip_date)
  
  return(trip)
}

#' @description Helper function that returns all of the rows pertaining to a
#' specific route.
#' 
#' @param data A data frame containing the bus data.
#' @param route_num The route ID number.
#' 
#' @return A data frame containing every row with the given route number.
get_route <- function(data, route_num) {
  # Check the input
  if (!is.data.frame(data) | !is.numeric(route_num)) {
    stop("Argument(s) of incorrect type.")
  } else if (!("Route" %in% colnames(data))) {
    stop("Data has no 'Route' column.")
  } else if (!(route_num %in% data$Route)) {
    stop("Could not find route.")
  }
  
  return(filter(data, Route == route_num))
}