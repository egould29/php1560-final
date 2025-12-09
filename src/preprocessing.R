# Data Import and Cleaning
library(dplyr)

#' @description Pre-processes the RIPTA on-time data by modifying column
#' types and dropping irrelevant and/or incomplete data.
#' 
#' @param data A data frame containing the raw RIPTA on-time data.
#' 
#' @return A correctly-formatted data frame containing the relevant data to our
#' analysis.
preprocess <- function(data) {
  # Check the input
  all_cols <- c("Date", "Driver.ID", "Route", "Trip", "Stop", "Stop.Sequence",
                "Scheduled.Time", "Actual.Arrival.Time", "Delay.Sec",
                "StopLat", "StopLng")
  if (!is.data.frame(data)) {
    stop("Preprocessing Error: Argument must be of class 'data frame'.")
  } else if ((sum(colnames(data) %in% all_cols) != 11) |
      (!(sum(colnames(data) %in% all_cols))) > 0) {
    stop("Preprocessing Error: Input data does not have correct column names.")
  }
  
  # Drop unused columns and filter incomplete rows
  cleaned <- data %>%
    select(-c(StopLat, StopLng))
  cleaned <- cleaned[complete.cases(cleaned), ]
  
  return(cleaned)
}