#' Pie Chart of Study Status Distribution
#'
#' @description Draws a pie chart to represent the distribution of overall statuses of clinical trials.
#' @param studies A tibble or dataframe with an 'overall_status' column.
#' @return Pie chart plot object.
plot_status_piechart = function(studies) {
  studies |>
    select(overall_status) |>
    group_by(overall_status) |>
    summarize(n=n()) |>
    ggplot(aes(x = "", y = n, fill = overall_status)) +
    geom_bar(stat = "identity") +
    coord_polar("y") +
    labs(title = "Overall Status Distribution")
}
