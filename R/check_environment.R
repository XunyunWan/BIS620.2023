#' Create the connection to a database and "studies" and "sponsors" tables.
#'
#' @description Checks the local environment and establishes a database connection.
#' @importFrom duckdb dbConnect duckdb
#' @importFrom devtools install_github
#' @return A database connection object.
#'
check_environment <- function() {
  local_db_path <- "ctgov.duckdb"

  if (file.exists(local_db_path)) {
    con <- dbConnect(duckdb(
      file.path(local_db_path),
      read_only = TRUE
    ))
  } else {
    if (!requireNamespace("devtools", quietly = TRUE)) {
      install.packages("devtools", version = "0.8.0")
    }
    devtools::install_github("presagia-analytics/ctrialsgov")
    con <- dbConnect(duckdb(file.path("ctrialsgov.duckdb"), read_only = TRUE))
  }

  return(con)
}
