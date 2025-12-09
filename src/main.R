# Main Pipeline
# set you working directory to the php1560-final 
library(tidyverse)

source("src/preprocessing.R")
source("src/delays.R")

# Read in the data
bus_data_raw <- read_csv("data/otp_simulated.csv")

# Preprocess the data
bus_data <- preprocess(bus_data_raw)

# Add the change in delays
bus_delays <- calculate_delays(bus_data)
