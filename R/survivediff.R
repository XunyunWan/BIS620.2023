#' Survival Difference Analysis
#'
#' This function performs a survival difference analysis for different groups in the dataset,
#' typically different treatment arms, based on a specified stratifying variable.
#'
#' @param data A data frame containing survival data, including death day (DTHDY) and death indicator (DTH).
#' @param whicharm A character string specifying the column name in `data` for stratifying the survival analysis.
#'
#' @return The function prints the results of a survival difference analysis using `survdiff` from the 'survival' package.
#'
#'
#' @importFrom survival survdiff
#' @importFrom stats as.formula
#' @export
survivediff <- function(data, whicharm) {
  surv_formula <- as.formula(paste("Surv(DTHDY, DTH) ~", whicharm))
  survivediff <- do.call("survdiff", list(formula = surv_formula, data = quote(data)))
  print(survivediff)
}
