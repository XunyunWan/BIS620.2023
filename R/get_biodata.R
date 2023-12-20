#' Extract and Combine Patient Biodata and Biomarkers
#'
#' This function extracts patient biodata from a dataset and combines it with 
#' biomarker data, including the KRAS mutation status determined by the `find_kras` function. 
#' It selects specific columns from the biodata and biomarker datasets and then performs 
#' an inner join on the subject identifier (SUBJID).
#'
#' @param dl A list containing at least two data frames: `adsl` and `biomark`. 
#' `adsl` should have columns for subject ID (SUBJID), treatment (ATRT), death day (DTHDY),
#' death indicator (DTH), age (AGE), sex (SEX), baseline weight (B_WEIGHT), baseline height (B_HEIGHT), 
#' and race (RACE). `biomark` should have columns for subject ID (SUBJID) and biomarker measurements 
#' (BMMTR1, BMMTR2, BMMTR3, BMMTR15).
#'
#' @return A data frame combining the patient's biodata with their biomarker information, 
#' including calculated BMI and KRAS mutation status.
#'
#' @examples
#' # Assume dl is a list with data frames adsl and biomark
#' data(NCT00364013, package = "bis620.2023")
#' dl <- NCT00364013
#' biodata <- get_biodata(dl)
#'
#' @importFrom dplyr select mutate inner_join
#' @export
get_biodata <- function(dl) {
  person <- dl$adsl |>
    select(SUBJID, ATRT, DTHDY, DTH, AGE, SEX, B_WEIGHT, B_HEIGHT, RACE) |>
    mutate(BMI = round(B_WEIGHT / (B_HEIGHT / 100)^2, 2))
  
  bio <- dl$biomark |> select(SUBJID, BMMTR1, BMMTR2, BMMTR3, BMMTR15)
  bio$KRAS <- apply(bio[, c("BMMTR1", "BMMTR2", "BMMTR3", "BMMTR15")], 1, find_kras)
  bio <- bio |> select(-BMMTR1, -BMMTR2, -BMMTR3, -BMMTR15)
  
  biodata <- inner_join(person, bio, by = "SUBJID")
  return(biodata)
}
