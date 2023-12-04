#' Create Histogram of Study Phases
#'
#' @description Generates a histogram of different study phases from a dataset.
#' @param x A database table containing a 'phase' column from which to create the histogram.
#' @return Histogram plot object.
plot_phase_histogram = function(x) {
  y = x |>
    select(phase) |>
    collect()

  all = x |>
    select(phase) |>
    group_by(phase) |>
    distinct() |>
    collect()

  all_phase = y |> bind_rows(all)

  all_phase$phase[is.na(all_phase$phase)] = "NA"

  all_phase = all_phase |> group_by(phase) |> summarize(n=n()-1)

  ggplot(all_phase, aes(x = phase, y = n)) +
    geom_col() +
    theme_bw() +
    xlab("Phase") +
    ylab("Count")

}
