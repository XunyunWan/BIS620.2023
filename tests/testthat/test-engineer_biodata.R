test_that("engineer_biodata adds expected columns", {
  
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
    KRAS = c("Mutant", "Wild-type", "Mutant", "Wild-type")
  )

  result <- engineer_biodata(biodata_mock)
  expect_true(all(c("age_group", "bmi_group", "arm", "arm_age", "arm_sex", "arm_bmi", "arm_race") %in% names(result)))
})
