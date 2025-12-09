# php1560-final

## Overview

This project is an exploratory data analysis of how delays cascade in RIPTA bus
routes. We look at how delays at one stop propagate over the course of the
remainder of a trip and aggregate this data on the route level, as well as
analyzing how drivers transitioning between routes contributes to delays.

## How to Run

The `src` folder contains all the R scripts we wrote.

`main.R` runs the entirety of our pipeline, reading in the data from the
`otp_simulated.csv` file on Canvas.

`preprocessing.R` cleans the data before we begin our analysis.

`delays.R` contains functions related to calculating delays on the trip-level.

`routes.R` contains functions that aggregate and summarize these delays on the
route-level.

`transitions.R` contains functions that analyze the effects of drivers
transitioning between routes on on-time rates.