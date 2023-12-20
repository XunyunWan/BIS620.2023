#' Descriptive Analysis of BMI
#'
#' This function performs a descriptive analysis on the 'BMI' column of the 'adsl' data frame within the provided list object. It calculates and returns basic descriptive statistics for the BMI values.
#'
#' @param dl A list containing at least one data frame named 'adsl'.
#' The 'adsl' data frame must include columns 'B_WEIGHT' and 'B_HEIGHT' for BMI calculation.
#' @return A named vector containing descriptive statistics for the BMI column, including mean, median, standard deviation, minimum, and maximum.
#' @importFrom dplyr select mutate
#' @importFrom stats median sd
#' @export
#' @examples
#' dl <- list(adsl = data.frame(SUBJID = 1:5, ATRT = rep("Treatment", 5), 
#'                             DTHDY = c(100, 150, 200, 250, 300), 
#'                             DTH = c(0, 1, 0, 1, 0), AGE = c(30, 35, 40, 45, 50), 
#'                             SEX = c("Male", "Female", "Male", "Female", "Male"), 
#'                             B_WEIGHT = c(70, 60, 75, 65, 80), 
#'                             B_HEIGHT = c(175, 160, 180, 165, 170), 
#'                             RACE = c("Race1", "Race2", "Race3", "Race1", "Race2")))
#' describe_bmi(dl)
describe_bmi <- function(dl) {
  data <- dl$adsl 
  data <- data |>
    select(SUBJID, ATRT, DTHDY, DTH, AGE, SEX, B_WEIGHT, B_HEIGHT, RACE) |>
    mutate(BMI = round(B_WEIGHT / (B_HEIGHT / 100)^2, 2))
  if (!"BMI" %in% names(data)) {
    stop("Data frame does not contain a 'BMI' column.")
  }
  bmi_stats <- c(
    Mean = mean(data$BMI, na.rm = TRUE),
    Median = median(data$BMI, na.rm = TRUE),
    SD = sd(data$BMI, na.rm = TRUE),
    Min = min(data$BMI, na.rm = TRUE),
    Max = max(data$BMI, na.rm = TRUE)
  )
  return(bmi_stats)
}
