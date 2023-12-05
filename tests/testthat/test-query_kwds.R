# Mock dataset for testing
data <- tibble(
  text_column = c("Example Text", "Another example", "Nothing relevant", "EXAMPLE")
)

# Test basic functionality
test_that("Basic keyword filtering works", {
  result <- query_kwds(data, "Example", "text_column")
  expect_equal(nrow(result), 3)
})

# Test case sensitivity
test_that("Case sensitivity is handled correctly", {
  # Case-sensitive search
  result_sensitive <- query_kwds(data, "Example", "text_column", ignore_case = FALSE)
  expect_equal(nrow(result_sensitive), 1)
  
  # Case-insensitive search
  result_insensitive <- query_kwds(data, "Example", "text_column", ignore_case = TRUE)
  expect_equal(nrow(result_insensitive), 3)
})

# Test edge cases
test_that("Edge cases are handled correctly", {
  # Empty keywords
  result_empty_kwds <- query_kwds(data, "", "text_column")
  expect_equal(nrow(result_empty_kwds), 4)
  
  # No match
  result_no_match <- query_kwds(data, "Nonexistent", "text_column")
  expect_equal(nrow(result_no_match), 0)
})