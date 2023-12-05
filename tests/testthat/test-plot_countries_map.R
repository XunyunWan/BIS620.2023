# Create mock data
mock_data <- tibble(country = c("United States", "France", "Brazil", "India", "United States"))

# Test that the function returns a plot object
test_that("Function returns a plot object", {
  result <- plot_countries_map(mock_data)
  expect_true(is.ggplot(result))
})

# Test with valid data
test_that("Function works with valid data", {
  result <- plot_countries_map(mock_data)
  # Optionally, save the plot for visual inspection
  ggsave("plot_countries_map_test.png", result)
  expect_true(is.ggplot(result))
})

# Test with empty data
test_that("Function handles empty data", {
  empty_data <- tibble(country = character(0))
  result <- plot_countries_map(empty_data)
  expect_true(is.ggplot(result))
  # Note: The plot will be empty, which is expected behavior
})

# Test with missing 'country' column
test_that("Function handles data with missing 'country' column", {
  no_country_data <- tibble(some_other_column = c(1, 2, 3))
  expect_error(plot_countries_map(no_country_data))
})