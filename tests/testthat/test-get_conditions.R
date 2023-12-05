test_that("get conditions works", {
  data("conditions")
  conditions <- conditions %>%
    group_by(nct_id) %>%
    summarize(condition_name = str_flatten(downcase_name, ", "))
  p = get_conditions(conditions)
  expect_equal(nrow(p), 21)
})
