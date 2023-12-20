#' Create Survival Plot
#'
#' This function generates a survival plot for a specified treatment arm from the provided dataset.
#' It utilizes survival analysis to visualize the survival probability over time.
#'
#' @param data A data frame containing survival data, including death day (DTHDY) and death indicator (DTH).
#' @param whicharm A character string specifying the column name in `data` to be used for stratifying the survival analysis.
#' @param title A character string for the title of the plot.
#'
#' @return The function prints a survival plot using `ggsurvplot` from the 'survminer' package.
#'
#' @importFrom survminer ggsurvplot
#' @importFrom stats as.formula
#' @export
surviveplot <- function(data, whicharm, title) {
  surv_formula <- as.formula(paste("Surv(DTHDY, DTH) ~", whicharm))
  survive <- do.call("survfit", list(formula = surv_formula, data = quote(data)))
  plot <- ggsurvplot(survive, data = data, title = title)
  print(plot)
}
