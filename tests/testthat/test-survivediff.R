test_that("survivediff runs without error", {
  biodata_mock <- data.frame(
    SUBJID = c(101, 102, 103, 104),
    ATRT = c("Treatment1", "Treatment2", "Treatment1", "Treatment2"),
    DTHDY = c(365, 200, 500, 100),
    DTH = c(0, 1, 0, 1),
    AGE = c(45, 60, 55, 35),
    SEX = c("Male", "Female", "Male", "Female"),
    B_WEIGHT = c(70, 60, 80, 55),
    B_HEIGHT = c(175, 160, 180, 165),
    RACE = c("Race1", "Race2", "Race1", "Race2"),
    BMI = c(22.86, 23.44, 24.69, 20.20),
    KRAS = c("Mutant", "Wild-type", "Mutant", "Wild-type"),
    arm = c("arm1", "arm1","arm2","arm2")
  )
  
  # Check that survivediff runs without error
  expect_no_error(survivediff(data = biodata_mock, whicharm = "arm"))
})
