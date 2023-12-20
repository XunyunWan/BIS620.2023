test_that("describe_age returns correct descriptive statistics", {
  adsl <- data.frame(
    AGE = c(30, 40, 50, 60, 70)
  )
  
  # Create the "dl" expected by the describe_age function
  dl <- list(adsl = adsl)
  
  # Run the describe_age function on the mock data
  result_stats <- describe_age(dl)
  
  # Expected statistics calculated directly
  expected_stats <- c(
    Mean = mean(dl$adsl$AGE, na.rm = TRUE),
    Median = median(dl$adsl$AGE, na.rm = TRUE),
    SD = sd(dl$adsl$AGE, na.rm = TRUE),
    Min = min(dl$adsl$AGE, na.rm = TRUE),
    Max = max(dl$adsl$AGE, na.rm = TRUE)
  )
  
  # Check that the result matches the expected statistics
  expect_equal(result_stats, expected_stats)
})


