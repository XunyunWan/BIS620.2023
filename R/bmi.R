#' BMI-Based Survival Analysis
#'
#' This function conducts survival analysis based on BMI groups within the biodata. 
#' It creates survival plots and performs survival difference analysis for groups defined by BMI.
#'
#' @param dl A list containing data frames necessary for creating biodata.
#'
#' @return The function does not return a value but generates survival plots and 
#' prints the results of survival difference analyses based on BMI groups.
#'
#' @examples
#' data(NCT00364013, package = "bis620.2023")
#' dl <- NCT00364013
#' bmi(dl)
#'
#' @export
bmi <- function(dl) {
  bio <- get_biodata(dl)
  biodata <- engineer_biodata(bio)
  biomarkers <- get_biomarker(biodata)
  biomarker1 <- biomarkers[[1]]
  biomarker2 <- biomarkers[[2]]
  surviveplot(data = biodata, whicharm = "arm_bmi", title = "BMI")
  survivediff(data = biodata, whicharm = "arm_bmi")
}
