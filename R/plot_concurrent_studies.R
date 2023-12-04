#' Plot Concurrent Clinical Trials Over Time
#'
#' @description Creates a line plot representing the number of concurrent clinical trials at any given date.
#' @param studies the studies to get the number of concurrent trials for.
#' @return Line plot object.
plot_concurrent_studies = function(studies) {
  all_dates = studies |>
    select(start_date, completion_date)|>
    pivot_longer(cols = everything()) |>
    select(-name) |>
    distinct() |>
    arrange(value) |>
    na.omit() |>
    rename(date = value)

  within_date = function(date, starts, ends) {
    date >= starts & date <= ends
  }

  # Get the number of concurrent trials at each of the unique dates.
  all_dates$count =
    map_dbl(
      all_dates$date,
      ~ .x |>
        within_date(studies$start_date, studies$completion_date) |>
        sum(na.rm = TRUE)
    )

  all_dates |>
    ggplot(aes(x = date, y = count)) +
    geom_line() +
    xlab("Date") +
    ylab("Count") +
    theme_bw()
}
