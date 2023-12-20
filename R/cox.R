#' Fit a Cox Proportional Hazards Model
#'
#' This function fits a Cox proportional hazards model to the biodata. It models 
#' the survival time based on several covariates including sex, race, BMI, age, treatment, 
#' and KRAS status. This is a common model used in survival analysis to explore the 
#' relationship between the survival time and one or more predictor variables.
#'
#' @param dl A list containing data frames necessary for creating biodata.
#'
#' @return The function returns a Cox proportional hazards model object fitted to the data.
#'
#' @examples
#' data(NCT00364013, package = "bis620.2023")
#' dl <- NCT00364013
#' cox_model <- cox(dl)
#'
#' @importFrom survival coxph Surv
#' @export
cox <- function(dl) {
  bio <- get_biodata(dl)
  biodata <- engineer_biodata(bio)
  cox_model <- coxph(Surv(DTHDY, DTH) ~ SEX + RACE + BMI + AGE + ATRT + KRAS, data = biodata)
  cox_model
}
