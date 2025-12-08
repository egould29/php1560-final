# Main Pipeline
library(tidyverse)

source("preprocessing.R")

# Read in the data
bus_data_raw <- read_csv("../data/otp_simulated.csv")

# Preprocess the data
bus_data <- preprocess(bus_data_raw)