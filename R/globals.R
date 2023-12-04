#' @importFrom shiny shinyApp fluidPage titlePanel sidebarLayout sidebarPanel mainPanel
#' tabsetPanel tabPanel plotOutput textInput selectInput dateRangeInput sliderInput
#' checkboxGroupInput h3 reactive renderPlot
#' @importFrom DT renderDataTable dataTableOutput
#' @importFrom dplyr filter group_by summarize mutate select left_join
#' @importFrom DBI dbListTables
#' @importFrom ggplot2 ggplot aes geom_histogram labs theme
#' @importFrom tidyr unite
#' @importFrom purrr map
#' @importFrom sf st_as_sf st_coordinates
#' @importFrom rnaturalearth ne_countries
#' @importFrom utils head install.packages
#' @importFrom stats na.omit
#' @importFrom stringr str_flatten

# Declare global variables used in the Shiny app
# These are typically names of data frame columns or variables used in the app's server function
utils::globalVariables(c("X", "Y", "Z", "Acceleration",
  "brief_title", "source_class", "dates", "slider", "sponsor", "study_type",
  "max_num_studies", "con", "studies", "sponsors", "conditions", "countries",
  "calculated", "nct_id", "condition_name", "country", "sponsor_name",
  "min_age", "max_age", "start_date", "completion_date", "overall_status",
  "phase", "percentage", "value", "downcase_name", "name", "lead_or_collaborator",
  "maximum_age_num","minimum_age_num"
))
