#' Descriptive Analysis of Age
#'
#' This function performs a descriptive analysis on the 'AGE' column of the 'adsl' data frame within the provided list object. It calculates basic descriptive statistics, including mean, median, standard deviation, minimum, and maximum for the age values.
#'
#' @param dl A list containing at least one data frame named 'adsl'.
#' The 'adsl' data frame must include an 'AGE' column with numeric values.
#' @return A named vector containing descriptive statistics for the age column, including mean, median, standard deviation, minimum, and maximum.
#' @importFrom stats median sd
#' @export
#' @examples
#' dl <- list(adsl = data.frame(AGE = c(30, 35, 40, 45, 50)))
#' describe_age(dl)
describe_age <- function(dl) {
  data <- dl$adsl 
  if (!"AGE" %in% names(data)) {
    stop("Data frame does not contain an 'AGE' column.")
  }
  age_stats <- c(
    Mean = mean(data$AGE, na.rm = TRUE),
    Median = median(data$AGE, na.rm = TRUE),
    SD = sd(data$AGE, na.rm = TRUE),
    Min = min(data$AGE, na.rm = TRUE),
    Max = max(data$AGE, na.rm = TRUE)
  )
  return(age_stats)
}
