# Main Pipeline
# set you working directory to the php1560-final 
library(tidyverse)
library(readr)
library(ggplot2)
library(patchwork)

source("src/preprocessing.R")
source("src/delays.R")
source("src/routes.R")
source("src/transitions.R")

# Read in and preprocess the data
bus_data_raw <- read_csv("data/otp_simulated.csv")
bus_data <- preprocess(bus_data_raw)

# Add the change in delays within each trip
bus_delays <- calculate_delays(bus_data)

# Generate route delay summaries
route_delays <- summarize_route_delay(bus_delays)
worsening_rank <- rank_worsening_routes(bus_delays, top_n = 10)

# Raw transitions for plotting
transitions <- identify_driver_transitions(bus_data)
transitions <- raw_transitions %>%
  mutate(cross_route = Route != next_route)

# Join everything
bus_data_full <- bus_delays %>%
  left_join(route_delays, by = "Route") %>%
  left_join(transitions %>% 
              select(Driver.ID, Trip, next_trip, next_route, 
                     final_delay, next_initial_delay, cross_route),
            by = c("Driver.ID", "Trip"))

# Final figure report (example focused on Route 95)
plot_trip_trajectory(bus_data_full, 95, 597) +
  plot_route_delay_distribution(bus_data_full, 95) +
  plot_route_reliability(bus_data_full) +
  plot_transition_propagation(bus_data_full) +
  plot_layout(ncol = 2) +
  plot_annotation(title = "RIPTA Delay Propagation: System-Level Summary")
