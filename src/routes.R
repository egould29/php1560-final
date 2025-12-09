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
    geom_histogram(aes(y = after_stat(count/sum(count))),
                   bins = 40, fill = "#009E73", color = "white") +
    labs(
      title = paste("Distribution of Stop-to-Stop Delay Changes:", route_id),
      x = "Δ Delay (sec)",
      y = "Proportion"
    ) +
    theme_minimal(base_size = 14)
}


#' Plot Average Change in Delay by Route
#'
#' @description
#' This function produces a horizontal bar plot showing the average stop-to-stop
#' change in delay (\code{avg_delta_delay}) for each route. Bars are colored
#' based on whether the average delay is positive (worsening) or non-positive
#' (recovering/stable). Routes are ordered from lowest to highest average
#' change in delay for easy comparison.
#'
#' @param route_summary A data frame containing route-level summaries.
#'   Must include at least the following columns:
#'   \itemize{
#'     \item \code{Route} — route identifier.
#'     \item \code{avg_delta_delay} — average change in delay between stops.
#'   }
#'
#' @return
#' A \code{ggplot} object representing the route-level delay summary.
plot_route_reliability <- function(route_summary) {
  route_summary <- route_summary %>%
    group_by(Route) %>%
    summarise(
      avg_delta_delay = mean(avg_delta_delay, na.rm = TRUE)
    ) %>%
    ungroup()
  
  route_summary <- route_summary %>%
    arrange(avg_delta_delay) %>%
    mutate(Route = factor(Route, levels = Route))
  
  ggplot(route_summary, aes(x = Route, y = avg_delta_delay,
                            fill = avg_delta_delay > 0)) +
    geom_col() +
    scale_fill_manual(values = c("#009E73", "#D55E00"),
                      labels = c("Recovering / Stable", "Worsening")) +
    labs(title = "Average ΔDelay by Route", 
         x = "Route", 
         y = "Average Change in Delay Between Stops (sec)", 
         fill = "Pattern" ) +
    coord_flip()+
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