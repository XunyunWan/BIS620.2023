#' Sex-Based Survival Analysis
#'
#' This function conducts survival analysis based on sex within different biomarker groups.
#' It generates survival plots and performs survival difference analysis for male and female groups,
#' as well as for mutant and wild-type biomarker groups.
#'
#' @param dl A list containing data frames necessary for creating biodata.
#'
#' @return The function does not return a value but generates multiple survival plots 
#' and prints the results of survival difference analyses for each group.
#'
#' @examples
#' # Assuming dl is a list with necessary data frames
#' data(NCT00364013, package = "bis620.2023")
#' dl <- NCT00364013
#' sex(dl)
#'
#' @importFrom survival survfit
#' @export
sex <- function(dl) {
  bio <- get_biodata(dl)
  biodata <- engineer_biodata(bio)
  biomarkers <- get_biomarker(biodata)
  biomarker1 <- biomarkers[[1]]
  biomarker2 <- biomarkers[[2]]
  sex_group1 <- biodata |> filter(SEX == "Male") |> filter(KRAS != "Unknown")
  sex_group2 <- biodata |> filter(SEX == "Female") |> filter(KRAS != "Unknown")
  surviveplot(data = biomarker1, whicharm = "arm_sex", title = "Mutant Biomarker Group")
  surviveplot(data = biomarker2, whicharm = "arm_sex", title = "Wild-type Biomarker Group")
  surviveplot(data = sex_group1, whicharm = "arm", title = "Male Group")
  surviveplot(data = sex_group2, whicharm = "arm", title = "Female Group")
  survivediff(data = biomarker1, whicharm = "arm_sex")
  survivediff(data = biomarker2, whicharm = "arm_sex")
  survivediff(data = sex_group1, whicharm = "arm")
  survivediff(data = sex_group2, whicharm = "arm")
}
