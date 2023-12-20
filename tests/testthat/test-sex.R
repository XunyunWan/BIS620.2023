test_that("sex function executes without error", {
  set.seed(123)  # For reproducibility
  adsl_mock <- data.frame(
    SUBJID = 101:150,
    ATRT = sample(c("Treatment1", "Treatment2"), 50, replace = TRUE),
    DTHDY = sample(100:1000, 50, replace = TRUE),
    DTH = sample(0:1, 50, replace = TRUE),
    AGE = sample(30:80, 50, replace = TRUE),
    SEX = sample(c("Male", "Female"), 50, replace = TRUE),
    B_WEIGHT = sample(55:85, 50, replace = TRUE),
    B_HEIGHT = sample(155:185, 50, replace = TRUE),
    RACE = sample(c("Race1", "Race2", "Race3"), 50, replace = TRUE)
  )
  biomark_mock <- data.frame(
    SUBJID = 101:150,
    BMMTR1 = sample(c("Mutant", "Wild-type", "Unknown"), 50, replace = TRUE),
    BMMTR2 = sample(c("Mutant", "Wild-type", "Unknown"), 50, replace = TRUE),
    BMMTR3 = sample(c("Mutant", "Wild-type", "Unknown"), 50, replace = TRUE),
    BMMTR15 = sample(c("Mutant", "Wild-type", "Unknown"), 50, replace = TRUE)
  )
  
  dl_mock <- list(adsl = adsl_mock, biomark = biomark_mock)
  
  expect_no_error(sex(dl_mock))
})