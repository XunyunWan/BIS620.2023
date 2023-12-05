test_that("plot conditions histogram works", {
  conditions <- conditions %>%
    group_by(nct_id) %>%
    summarize(condition_name = str_flatten(downcase_name, ", "))

  countries <- countries %>%
    group_by(nct_id) %>%
    summarize(country = str_flatten(name, ", "))

  sponsors <- sponsors %>%
    mutate(sponsor_name = name) %>%
    select(-name)

  # Join the datasets
  studies <- studies %>%
    left_join(conditions, by = "nct_id") %>%
    left_join(countries, by = "nct_id") %>%
    left_join(sponsors, by = "nct_id") |> head(100)

  p = plot_conditions_histogram(studies)

  expect_true(ggplot2::is.ggplot(p))
})

