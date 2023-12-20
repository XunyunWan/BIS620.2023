test_that("cox function returns a coxph model", {
  set.seed(123)  # For reproducibility
  adsl_mock <- data.frame(
    SUBJID = 101:120,
    ATRT = sample(c("Treatment1", "Treatment2"), 20, replace = TRUE),
    DTHDY = sample(100:1000, 20, replace = TRUE),
    DTH = sample(0:1, 20, replace = TRUE),
    AGE = sample(30:80, 20, replace = TRUE),
    SEX = sample(c("Male", "Female"), 20, replace = TRUE),
    B_WEIGHT = sample(55:85, 20, replace = TRUE),
    B_HEIGHT = sample(155:185, 20, replace = TRUE),
    RACE = sample(c("Race1", "Race2", "Race3"), 20, replace = TRUE)
  )
  
  biomark_mock <- data.frame(
    SUBJID = 101:120,
    BMMTR1 = rep(c("Mutant", "Wild-type", "Mutant", "Wild-type"), length.out = 20),
    BMMTR2 = rep(c("Mutant", "Wild-type", "Mutant", "Wild-type"), length.out = 20),
    BMMTR3 = rep(c("Mutant", "Wild-type", "Mutant", "Wild-type"), length.out = 20),
    BMMTR15 = rep(c("Mutant", "Wild-type", "Mutant", "Wild-type"), length.out = 20)
  )
  
  
  dl_mock <- list(adsl = adsl_mock, biomark = biomark_mock)
  
  result <- cox(dl_mock) 
  expect_s3_class(result, "coxph")
})
