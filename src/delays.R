# Trip-Level Delay Analysis
library(dplyr)

source("utils.R")

test <- bus_data %>%
  group_by(Date, Trip, Stop.Sequence) %>%
  summarize(total = n())