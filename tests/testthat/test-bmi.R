library(survival)

test_that("bmi function executes without error", {
  adsl_mock <- data.frame(
    SUBJID = c(101, 102, 103, 104),
    ATRT = c("Treatment1", "Treatment2", "Treatment1", "Treatment2"),
    DTHDY = c(365, 200, 500, 100),
    DTH = c(0, 1, 0, 1),
    AGE = c(45, 60, 55, 35),
    SEX = c("Male", "Female", "Male", "Female"),
    B_WEIGHT = c(70, 60, 80, 55),
    B_HEIGHT = c(175, 160, 180, 165),
    RACE = c("Race1", "Race2", "Race1", "Race2")
  )
  
  biomark_mock <- data.frame(
    SUBJID = c(101, 102, 103, 104),
    BMMTR1 = c("Mutant", "Wild-type", "Mutant", "Unknown"),
    BMMTR2 = c("Mutant", "Wild-type", "Mutant", "Unknown"),
    BMMTR3 = c("Mutant", "Wild-type", "Mutant", "Unknown"),
    BMMTR15 = c("Mutant", "Wild-type", "Mutant", "Unknown")
  )
  
  dl_mock <- list(adsl = adsl_mock, biomark = biomark_mock)
  
  expect_no_error(bmi(dl_mock))
})
