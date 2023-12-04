#' Histogram of Study Conditions
#'
#' @description Creates a histogram displaying the frequency of different conditions in a dataset of
#' clinical studies.
#' @param studies A database table (tibble) containing the study conditions data.
#' @return Histogram plot object.
plot_conditions_histogram = function(studies) {
  studies |>
    get_conditions()|>
    as.data.frame() |>
    ggplot(aes(x = condition_name, y = n)) +
    geom_col() +
    xlab("Name") +
    ylab("Count") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
