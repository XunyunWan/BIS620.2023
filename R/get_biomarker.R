#' Extract Biomarker Subsets
#'
#' This function separates the biodata into two subsets based on the KRAS biomarker status: 'Mutant' and 'Wild-type'.
#'
#' @param biodata A data frame containing patient biodata with a column 'KRAS' indicating the biomarker status.
#'
#' @return A list containing two data frames: one with rows where KRAS is 'Mutant' and the other where KRAS is 'Wild-type'.
#'
#' @examples
#' # Assuming biodata is a data frame with a 'KRAS' column
#' data(NCT00364013, package = "bis620.2023")
#' dl <- NCT00364013
#' 
#' biomarker_sets <- get_biomarker(get_biodata(dl))
#'
#' @importFrom dplyr filter
#' @export
get_biomarker <- function(biodata) {
  biomarker1 <- biodata |> filter(KRAS == "Mutant")
  biomarker2 <- biodata |> filter(KRAS == "Wild-type")
  return(list(biomarker1, biomarker2))
}
