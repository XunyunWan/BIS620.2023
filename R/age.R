#' Age-Based Survival Analysis
#'
#' This function performs survival analysis based on age groups within different biomarker groups.
#' It generates survival plots and performs survival difference analysis for mid-age and old-age groups,
#' as well as for mutant and wild-type biomarker groups.
#'
#' @param dl A list containing data frames necessary for creating biodata.
#' @return The function does not return a value but generates multiple survival plots 
#' and prints the results of survival difference analyses for each group.
#' @examples
#' data(NCT00364013, package = "bis620.2023")
#' dl <- NCT00364013
#' age(dl)
#' 
#' @importFrom survival survfit
#' @export
age <- function(dl) {
  bio <- get_biodata(dl)
  biodata <- engineer_biodata(bio)
  biomarkers <- get_biomarker(biodata)
  biomarker1 <- biomarkers[[1]]
  biomarker2 <- biomarkers[[2]]
  age_group1 <- biodata |> filter(age_group == "mid_age") |> filter(KRAS != "Unknown")
  age_group2 <- biodata |> filter(age_group == "old_age") |> filter(KRAS != "Unknown")
  surviveplot(data = biomarker1, whicharm = "arm_age", title = "Mutant Biomarker Group")
  surviveplot(data = biomarker2, whicharm = "arm_age", title = "Wild-type Biomarker Group")
  surviveplot(data = age_group1, whicharm = "arm", title = "Middle Age Group (27<age<56)")
  surviveplot(data = age_group2, whicharm = "arm", title = "Old Age Group (56<age<82)")
  survivediff(data = biomarker1, whicharm = "arm_age")
  survivediff(data = biomarker2, whicharm = "arm_age")
  survivediff(data = age_group1, whicharm = "arm")
  survivediff(data = age_group2, whicharm = "arm")
}