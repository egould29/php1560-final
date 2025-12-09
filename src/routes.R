#' @title Compute Route-Level Delay Summary
#'
#' @description Aggregates trip-level delay propagation metrics to produce
#' summaries for each route. This includes average delays, average change in
#' delay between stops, variance measures, and the proportion of “worsening”
#' vs. “recovering” segments on a typical route.
#'
#' @param delay_data A data frame produced by the trip-level processing in
#' `delays.R`. Must contain at least:
#'   - Route
#'   - Trip
#'   - Stop.Sequence
#'   - Delay.Sec
#'   - Delta.Delay
#'   
#' @return A data frame with one row per route and computed summary statistics.
summarize_route_delay <- function(delay_data) {
  delay_data %>%
    group_by(Route) %>%
    summarise(
      avg_initial_delay = mean(Delay.Sec[Stop.Sequence == min(Stop.Sequence)], 
                               na.rm = TRUE),
      avg_final_delay   = mean(Delay.Sec[Stop.Sequence == max(Stop.Sequence)], 
                               na.rm = TRUE),
      avg_delta_delay   = mean(Delta.Delay, na.rm = TRUE),
      sd_delta_delay    = sd(Delta.Delay, na.rm = TRUE),
      worsening_prop    = mean(Delta.Delay > 0, na.rm = TRUE),
      recovering_prop   = mean(Delta.Delay < 0, na.rm = TRUE),
      n_trips           = n_distinct(Trip)
    ) %>%
    ungroup()
}


#' @title Identify Routes With Fastest Delay Accumulation
#'
#' @description Ranks all routes by how quickly they accumulate delays
#' (using average positive Delta.Delay as the criterion). Helps highlight
#' potential structural problems with the schedule rather than individual drivers.
#'
#' @param delay_data A data frame produced from `delays.R`.
#' @param top_n The number of routes to return.
#'
#' @return A tibble showing the top routes experiencing worsening delay patterns.
rank_worsening_routes <- function(delay_data, top_n = 10) {
  delay_data %>%
    group_by(Route) %>%
    summarise(
      mean_positive_delta = mean(Delta.Delay[Delta.Delay > 0], na.rm = TRUE),
      n_trips = n_distinct(Trip)
    ) %>%
    arrange(desc(mean_positive_delta)) %>%
    head(top_n)
}


#' @title Route Delay Distribution Plot
#'
#' @description Produces a visualization showing the distribution of
#' delay changes (`Delta.Delay`) across all trips in a route. Helps compare
#' “stable,” “recovering,” and “worsening” tendencies at the system level.
#'
#' @param delay_data Output from `delays.R`.
#' @param route_id Route identifier for which the plot will be created.
#'
#' @return A ggplot object.
plot_route_delay_distribution <- function(delay_data, route_id) {
  route_subset <- delay_data %>% filter(Route == route_id)
  
  if (nrow(route_subset) == 0) {
    stop(paste("No data found for route:", route_id))
  }
  
  ggplot(route_subset, aes(x = Delta.Delay)) +
    geom_histogram(aes(y = after_stat(count/sum(count))), bins = 40) +
    labs(
      title = paste("Delay Change Distribution for Route", route_id),
      x = "Change in Delay Between Stops (sec)",
      y = "Proportion"
    ) +
    theme_minimal()
}


#' @title Compare Delay Profiles Between Routes
#'
#' @description Performs a statistical comparison (e.g., t-test) between
#' two routes to test whether their average delay propagation differs
#' significantly.
#'
#' @param delay_data Output from `delays.R`.
#' @param route_a First route ID.
#' @param route_b Second route ID.
#'
#' @return A list containing:
#'   - summary table of group means
#'   - results of the t-test
compare_routes <- function(delay_data, route_a, route_b) {
  subset_a <- delay_data %>% filter(Route == route_a)
  subset_b <- delay_data %>% filter(Route == route_b)
  if (nrow(subset_a) == 0 | nrow(subset_b) == 0) {
    stop("One or both specified routes have no data.")
  }
  t_result <- t.test(subset_a$Delta.Delay, subset_b$Delta.Delay)
  list(
    summary = data.frame(
      Route = c(route_a, route_b),
      Mean_Delta = c(mean(subset_a$Delta.Delay, na.rm = TRUE),
                     mean(subset_b$Delta.Delay, na.rm = TRUE)),
      SD_Delta = c(sd(subset_a$Delta.Delay, na.rm = TRUE),
                   sd(subset_b$Delta.Delay, na.rm = TRUE))
    ),
    t_test = t_result
  )
}