test_that("find_kras function tests", {
  # Test when "Mutant" is present
  expect_equal(find_kras(c("Mutant", "Wild-type", "Unknown")), "Mutant")
  
  # Test when "Wild-type" is predominant
  expect_equal(find_kras(c("Wild-type", "Wild-type", "Unknown")), "Wild-type")
  
  # Test when "Unknown" should be the outcome
  expect_equal(find_kras(c("Failure", "Unknown", "Unknown")), "Unknown")
  
  # Test when "Failure" and "Unknown" outnumber "Wild-type"
  expect_equal(find_kras(c("Wild-type", "Failure", "Unknown")), "Unknown")
  
  # Test with an empty vector leading to "Unknown"
  expect_equal(find_kras(character(0)), "Unknown")
  
  # Test with invalid input leading to "Unknown"
  expect_equal(find_kras(c("Invalid")), "Unknown")
})
