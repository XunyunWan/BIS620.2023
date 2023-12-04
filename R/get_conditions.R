#' Retrieve and Summarize Study Conditions
#'
#' @description Processes a dataset to count and calculate the percentage of studies for each condition.
#' @param c A database table (tibble) with a 'name' column representing the condition names.
#' @return A tibble with condition names, counts, total count, and their percentage of the total.
get_conditions = function(c){
  condition = c |>
    select(condition_name) |>
    collect() |>
    separate_rows(condition_name, sep = ", ") |>
    group_by(condition_name) |>
    summarize(n=n()) |>
    mutate(sum = sum(n)) |>
    mutate(percentage = n/sum) |>
    filter(percentage > 0.003) |>
    arrange(desc(n))|>
    collect()

  return(condition)
}
