test_that("describe_bmi returns correct descriptive statistics", {
  # Create mock data
  mock_data <- data.frame(
    SUBJID = 1:5,
    ATRT = c("Treatment1", "Treatment2", "Treatment1", "Treatment2", "Treatment1"),
    DTHDY = c(365, 200, 500, 100, 300),
    DTH = c(0, 1, 0, 1, 0),
    AGE = c(45, 60, 55, 35, 50),
    SEX = c("Male", "Female", "Male", "Female", "Male"),
    B_WEIGHT = c(70, 60, 80, 55, 75),
    B_HEIGHT = c(175, 160, 180, 165, 170),
    RACE = c("Race1", "Race2", "Race1", "Race2", "Race1")
  )
  
  # Calculate BMI within the test to ensure the values match the function
  mock_data <- mock_data |> 
    mutate(BMI = round(B_WEIGHT / (B_HEIGHT / 100)^2, 2))
  
  dl <- list(adsl = mock_data)
  
  # Expected statistics
  expected_stats <- c(
    Mean = mean(dl$adsl$BMI, na.rm = TRUE),
    Median = median(dl$adsl$BMI, na.rm = TRUE),
    SD = sd(dl$adsl$BMI, na.rm = TRUE),
    Min = min(dl$adsl$BMI, na.rm = TRUE),
    Max = max(dl$adsl$BMI, na.rm = TRUE)
  )
  
  # Run the describe_bmi function on the mock data
  result_stats <- describe_bmi(dl)
  
  # Check that the result matches the expected statistics
  expect_equal(result_stats, expected_stats)
})

