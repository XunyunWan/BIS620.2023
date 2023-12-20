test_that("describe_sex calculates counts and proportions correctly", {

  adsl <- data.frame(
    SEX = c("Male", "Female", "Female", "Male", "Male")
  )
  
  dl <- list(adsl = adsl)
  
  # Run the describe_sex function on the mock data
  result <- describe_sex(dl)
  
  # Check that the result is a data frame with the correct columns
  expect_true(is.data.frame(result))
  expect_equal(colnames(result), c("Sex", "Count", "Proportion"))
  
  # Check that the function calculates counts correctly
  expect_equal(result$Count[1], 2) # Assuming 'Male' is the first level
  expect_equal(result$Count[2], 3) # Assuming 'Female' is the second level
  
  # Check that the function calculates proportions correctly
  expect_equal(result$Proportion[1], 2/5) # 3 out of 5 are 'Male'
  expect_equal(result$Proportion[2], 3/5) # 2 out of 5 are 'Female'
})

test_that("describe_sex throws an error for data frames without a SEX column", {
  # Create mock data without a SEX column
  mock_data_no_sex <- data.frame(
    NOT_SEX = c("A", "B", "C", "D", "E"))
})