#' Run Shiny Application for Clinical Trials Query
#'
#' @importFrom shiny shinyApp fluidPage titlePanel sidebarLayout sidebarPanel mainPanel
#' tabsetPanel tabPanel plotOutput textInput selectInput dateRangeInput sliderInput
#' checkboxGroupInput h3 reactive renderPlot
#' @importFrom DT renderDataTable dataTableOutput
#' @import dplyr
#' @import duckdb
#' @import DBI
#' @import ggplot2
#' @import tidyr
#' @import purrr
#' @import sf
#' @import rnaturalearth
#' @import rnaturalearthdata
#' @importFrom utils head install.packages
#' @importFrom stats na.omit
#'
#' @return
#' A running Shiny application on the default web browser.
#'
#' @details
#' The application is composed of a sidebar for input controls and a main panel for displaying the plots and data table output. The following filters can be applied:
#' \itemize{
#'   \item \code{Brief title keywords}: Filters studies by title keywords.
#'   \item \code{Sponsor Types}: Filters studies by the type of sponsor.
#'   \item \code{Date Range}: Filters studies within a specified date range.
#'   \item \code{Age Range}: Filters studies by participant age range.
#'   \item \code{Lead or Collaborator}: Filters studies by the role of the sponsor.
#'   \item \code{Study Type}: Filters studies by the type of study.
#' }
#' The outputs include several plots (phase, concurrent studies, conditions, countries, and status) and a data table displaying the filtered studies.
#' @export
#'
runShinyApp <- function(){

  max_num_studies=1000

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
    left_join(sponsors, by = "nct_id")

  # Define UI for application that draws a histogram
  ui <- fluidPage(
    # Application title
    titlePanel("Clinical Trials Query"),

    sidebarLayout(
      sidebarPanel(
        textInput("brief_title_kw", "Brief title keywords"),
        selectInput("source_class", label = h3("Sponsor Types"),
                    choices=list(
                      "Federal" = "FED",
                      "Individual" = "INDIV",
                      "Industry" = "INDUSTRY",
                      "Network" = "NETWORK", "NIH" = "NIH",
                      "Other" = "OTHER",
                      "Other gov" = "OTHER_GOV",
                      "Unknown" = "Unknown"),multiple = TRUE),

        dateRangeInput("dates", label = h3("Date Range")),
        checkboxGroupInput("sponsor", label = h3("Lead or Collaborator"),
                           choices = list("lead" = "lead", "collaborator" = "collaborator"),
        ),
        checkboxGroupInput("study_type", label = h3("Study Type"),
                           choices = list("Interventional" = "Interventional",
                                          "Expanded Access" = "Expanded Access",
                                          "Observational" = "Observational",
                                          "Observational [Patient Registry]" = "Observational [Patient Registry]"),
        )
      ),


      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel("Phase", plotOutput("phase_plot")),
          tabPanel("Concurrent", plotOutput("concurrent_plot")),
          tabPanel("Conditions", plotOutput("conditions_plot")),
          tabPanel("Countries", plotOutput("countries_plot")),
          tabPanel("Status", plotOutput("status_plot"))
        ),
       dataTableOutput("trial_table")
      )
    )
  )



  # Define server logic required to draw a histogram
  server <- function(input, output) {

    get_studies = reactive({
      if (input$brief_title_kw != "") {
        si = input$brief_title_kw |>
          strsplit(",") |>
          unlist() |>
          trimws()
        ret = query_kwds(studies, si, "brief_title", match_all = TRUE)
      } else {
        ret = studies
      }
      if(!is.null(input$source_class)){
        ret = ret |> filter(source_class %in% !!input$source_class)
      }
      input_start <- input$dates[1]
      input_end <- input$dates[2]
      if(!is.null(input$dates)){
        ret = ret |> filter(start_date <= input_start, completion_date >= input_end)
      }
      if(!is.null(input$sponsor)){
        ret = ret |> filter(lead_or_collaborator %in% !!input$sponsor)
      }
      if(!is.null(input$study_type)){
        ret = ret |> filter(study_type %in% !!input$study_type)
      }

      ret |>
        head(max_num_studies)
    })

    output$phase_plot = renderPlot({
      plot_phase_histogram(get_studies())
    })

    output$concurrent_plot = renderPlot({
      get_studies() |>
        plot_concurrent_studies()
    })

    output$conditions_plot = renderPlot({
      get_studies() |>
        plot_conditions_histogram()
    })

    output$countries_plot = renderPlot({
      get_studies() |>
        plot_countries_map()
    })

    output$status_plot = renderPlot({
      get_studies() |>
        plot_status_piechart()
    })

    output$trial_table = renderDataTable({
      get_studies() |>
        head(max_num_studies) |>
        select(nct_id, brief_title, start_date, completion_date) |>
        rename(`NCT ID` = nct_id, `Brief Title` = brief_title,
               `Start Date` = start_date, `Completion Date` = completion_date)
    })

  }


  # Run the application
  shinyApp(ui = ui, server = server)
}

