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


#' @title Summarize transition effects on delays
#'
#' @description Aggregates transition-level data to study whether
#' late arrivals tend to lead to worse initial delays in the next trip.
#' Can be used for system-level reporting.
#'
#' @param transitions Data frame from identify_driver_transitions().
#'
#' @return A summary data frame.
summarize_transition_effects <- function(transitions) {
  transitions %>%
    mutate(
      cross_route = Route != next_route,
      propagated_delay = next_initial_delay - 0 
    ) %>%
    group_by(cross_route) %>%
    summarise(
      avg_gap_min = mean(transition_gap_min, na.rm = TRUE),
      avg_prev_final_delay = mean(final_delay, na.rm = TRUE),
      avg_next_initial_delay = mean(next_initial_delay, na.rm = TRUE),
      correlation = cor(final_delay, next_initial_delay, use = "complete.obs"),
      n = n(),
      .groups = "drop"
    )
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
             color = Route != next_route)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      title = "Propagation of Delays Through Driver Transitions",
      x = "Final Delay of Trip A (sec)",
      y = "Initial Delay of Trip B (sec)",
      color = "Cross-Route Transition"
    ) +
    theme_minimal()
}

