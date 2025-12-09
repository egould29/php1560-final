#' Compute Stop-to-Stop Changes in Delay for a Single Trip
#'
#' @description This function takes the data for one bus trip—consisting of multiple stops—
#' and calculates the change in delay between consecutive stops. The first stop
#' in each trip receives an \code{NA} value because there is no previous stop
#' from which to compute a change. The function assumes that delay is measured
#' in seconds and that stops are arranged in increasing order of 
#' \code{Stop.Sequence}. If they are not, the function will internally sort the 
#' trip before computing differences.
#'
#' @param trip_data A data frame containing all observations for a single trip.
#'   Must include the columns \code{Stop.Sequence} and \code{Delay.Sec}.
#'
#' @return A numeric vector containing the change in delay between stops for
#'   that trip. The returned vector has the same length as \code{trip_data}.
#'   The first element is \code{NA}.
get_trip_delay <- function(trip_data) {
  if (!is.data.frame(trip_data)) {
    stop("Argument must be of type 'data frame'.")
  }
  
  required_cols <- c("Stop.Sequence", "Delay.Sec")
  if (!all(required_cols %in% names(trip_data))) {
    stop("trip_data must contain columns: Stop.Sequence and Delay.Sec")
  }
  
  trip_data <- trip_data %>% 
    arrange(Stop.Sequence)
  
  delta_delay <- trip_data$Delay.Sec - lag(trip_data$Delay.Sec)
  
  return(delta_delay)
}


#' Add Stop-to-Stop Delay Change Column to a Full RIPTA Dataset
#'
#' @description
#' This function computes the change in delay between consecutive stops for
#' every trip in the dataset. It processes the data sequentially, identifying
#' each trip using the combination of \code{Trip} and \code{Date}, extracting
#' that trip's rows with \code{get_trip()}, and then applying
#' \code{get_trip_delay()} to compute the stop-to-stop change in delay.
#' The resulting values are inserted into a new column, \code{Delta.Delay},
#' aligned with the original row order.
#'
#' @param data A data frame containing RIPTA bus observations. Must include
#'   \code{Trip}, \code{Date}, \code{Stop.Sequence}, and \code{Delay.Sec}.
#'
#' @return
#' A data frame identical to the input but with one new column,
#' \code{Delta.Delay}, containing the change in delay between consecutive
#' stops for each trip.
calculate_delays <- function(data) {
  data %>%
    arrange(Date, Trip, Stop.Sequence) %>%
    group_by(Date, Trip) %>%
    mutate(
      Delta.Delay = Delay.Sec - lag(Delay.Sec),
      Delay.Type = case_when(
        Delta.Delay > 0  ~ "worsening",
        Delta.Delay < 0  ~ "recovering",
        Delta.Delay == 0 ~ "stable",
        TRUE             ~ NA_character_
      )
    ) %>%
    ungroup()
}


#' Plot the Delay Trajectory for a Single Trip
#'
#' @description
#' Creates a line-and-point visualization showing how a bus trip’s delay
#' evolves across its sequence of stops. This plot highlights whether the trip
#' tends to recover, maintain, or accumulate delay as it progresses.
#'
#' The function filters the dataset for a specific route and trip, orders the
#' stops by their sequence, and plots \code{Delay.Sec} against
#' \code{Stop.Sequence}. A dashed horizontal line at zero delay provides a
#' visual reference for on-time performance.
#'
#' @param delay_data A data frame containing stop-level RIPTA on-time
#' performance data. Must include the columns:
#' \code{Route}, \code{Trip}, \code{Stop.Sequence}, and \code{Delay.Sec}.
#'
#' @param route_id A numeric or character route identifier used to filter the
#' data to a single route.
#'
#' @param trip_id A numeric or character trip identifier specifying which trip
#' to visualize.
plot_trip_trajectory <- function(delay_data, route_id, trip_id) {
  trip_data <- delay_data %>%
    filter(Route == route_id, Trip == trip_id)
  
  ggplot(trip_data, aes(x = Stop.Sequence, y = Delay.Sec)) +
    geom_line(size = 1.2, color = "#0072B2") +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_continuous(breaks = trip_data$Stop.Sequence) +  # show all stops
    labs(
      title = paste("Delay Trajectory for Route", route_id, "Trip", trip_id),
      x = "Stop Sequence",
      y = "Delay (sec)"
    ) +
    theme_minimal(base_size = 14)
}
