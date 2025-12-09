# Main Pipeline
# set you working directory to the php1560-final 
library(tidyverse)

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

# Generate transition summary
transitions <- identify_driver_transitions(bus_data) |>
  summarize_transition_effects()

# Join delay data into full bus data
bus_data <- left_join(bus_delays, route_delays, by = "Route")
