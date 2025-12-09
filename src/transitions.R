#' @title Identify driver transitions between routes
#'
#' @description For each driver, finds pairs of consecutive trips and computes:
#'   - the time gap between trips
#'   - whether the trips are on different routes
#'   - the final delay of the first trip
#'   - the initial delay of the second trip
#'
#' This supports analyzing how structural scheduling (tight transitions)
#' contributes to system-wide delay propagation.
#'
#' @param data A preprocessed RIPTA data frame with columns:
#'   Driver.ID, Route, Trip, Actual.Arrival.Time, Scheduled.Time,
#'   Delay.Sec, Stop.Sequence
#'
#' @return A data frame with one row per transition.
identify_driver_transitions <- function(data) {
  
  trip_ends <- data %>%
    group_by(Driver.ID, Route, Trip) %>%
    filter(Stop.Sequence == max(Stop.Sequence)) %>%
    summarise(
      trip_end_time = max(Actual.Arrival.Time),
      final_delay = Delay.Sec,
      .groups = "drop"
    )
  
  trip_starts <- data %>%
    group_by(Driver.ID, Route, Trip) %>%
    filter(Stop.Sequence == min(Stop.Sequence)) %>%
    summarise(
      trip_start_sched = min(Scheduled.Time),
      initial_delay = Delay.Sec,
      .groups = "drop"
    )
  
  trips <- trip_ends %>%
    left_join(trip_starts, by = c("Driver.ID", "Route", "Trip")) %>%
    arrange(Driver.ID, trip_start_sched)
  
  transitions <- trips %>%
    group_by(Driver.ID) %>%
    arrange(trip_start_sched, .by_group = TRUE) %>%
    mutate(
      next_trip = lead(Trip),
      next_route = lead(Route),
      next_start_sched = lead(trip_start_sched),
      next_initial_delay = lead(initial_delay),
      transition_gap_min = as.numeric(difftime(
        next_start_sched, trip_end_time, units = "mins"
      ))
    ) %>%
    ungroup() %>%
    filter(!is.na(next_trip))
  
  return(transitions)
}


#' @title Visualize delay propagation through driver transitions
#'
#' @description Produces a scatter plot showing how final delays of one trip
#' relate to initial delays of the following trip, highlighting whether
#' propagation is stronger in cross-route transitions.
#'
#' @param transitions Output of identify_driver_transitions().
#'
#' @return A ggplot object.
plot_transition_propagation <- function(transitions) {
  ggplot(transitions,
         aes(x = final_delay,
             y = next_initial_delay,
             color = cross_route)) +
    geom_point(alpha = 0.4, size = 2) +
    geom_smooth(method = "lm", se = FALSE, size = 1.2) +
    scale_color_manual(values = c("#0072B2", "#D55E00"),
                       labels = c("Same Route", "Different Route")) +
    labs(
      title = "Delay Propagation Across Driver Transitions",
      x = "Final Delay of Trip A (sec)",
      y = "Initial Delay of Trip B (sec)",
      color = "Transition Type"
    ) +
    theme_minimal(base_size = 14)
}
