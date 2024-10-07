# Helper functions --------------------------------------------------------

# Function to calculate AUC using the trapezoidal rule
trapezoidal_auc <- function(time, value) {
  # Remove rows with NA in 'value'
  clean_data <- na.omit(data.frame(time, value))

  # Extract clean time and value vectors
  time <- clean_data$time
  value <- clean_data$value

  # Calculate AUC using trapezoidal rule
  n <- length(time)
  if (n > 1) {  # Ensure there are at least two points to calculate AUC
    auc <- sum(diff(time) * (value[-1] + value[-n]) / 2)
    return(abs(auc))
  } else {
    return(0)  # Return 0 if there are not enough data points to calculate AUC
  }
}


# Function to convert "hr" strings to minutes
convert_to_minutes <- function(time_str) {
  hours <- as.numeric(sub("hr", "", time_str))
  return(hours * 60)
}

# Function to calculate the Index
calculate_index <- function(data_cpep, data_gluc) {
  data_cpep$time <- sapply(data_cpep$time, convert_to_minutes)
  data_gluc$time <- sapply(data_gluc$time, convert_to_minutes)

  auc_cpep <- data_cpep %>%
    group_by(subject, visit) %>%
    summarise(auc_cpep = trapezoidal_auc(as.numeric(time), result)) %>%
    ungroup()

  auc_gluc <- data_gluc %>%
    group_by(subject, visit) %>%
    summarise(auc_gluc = trapezoidal_auc(as.numeric(time), result)) %>%
    ungroup()

  index_data <- auc_cpep %>%
    inner_join(auc_gluc, by = c("subject", "visit")) %>%
    mutate(index = (auc_cpep / auc_gluc) * 100)

  return(index_data)
}
