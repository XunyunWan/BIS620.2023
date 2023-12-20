#' Descriptive Analysis of Sex Categories
#'
#' This function performs a descriptive analysis on the 'SEX' column of the 'adsl' data frame within the provided list object. It calculates the count and proportion of each sex category present in the data.
#'
#' @param dl A list containing at least one data frame named 'adsl' with a 'SEX' column.
#' @return A data frame with columns 'Sex', 'Count', and 'Proportion', where each row represents a unique sex category from the input data frame. The 'Count' column contains the number of occurrences of each sex category, and the 'Proportion' column contains the proportion of each sex category relative to the total count.
#' @export
#' @examples
#' dl <- list(adsl = data.frame(SEX = c("Male", "Female", "Female", "Male", "Male")))
#' describe_sex(dl)
describe_sex <- function(dl) {
  data <- dl$adsl 
  if ("SEX" %in% names(data)) {
    sex_counts <- table(data$SEX)
    sex_df <- data.frame(Sex = names(sex_counts), Count = as.integer(sex_counts))
    sex_df$Proportion <- sex_df$Count / sum(sex_df$Count)
    return(sex_df)
  } else {
    stop("Data frame does not contain a 'SEX' column.")
  }
}
