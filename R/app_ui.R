#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny shinydashboard
#' @importFrom shinydashboard dashboardSidebar
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    dashboardPage(
      ui_header(),
      dashboardSidebar(disable = TRUE),
      ui_body()
    )
  )
}

#' @import shinydashboard
#' @importFrom shinydashboard dashboardHeader
#' @noRd
ui_header <- function() {
  dashboardHeader(title = "Ansible Collections")
}

#' @import shinydashboard
#' @importFrom shinydashboard dashboardBody
#' @importFrom plotly plotlyOutput
#' @noRd
ui_body <- function() {
  dashboardBody(
    fluidRow(
      column(width = 10,
             box(width = NULL, solidHeader = TRUE,
                 plotOutput("survival_plot", height = 550)
             ),
             column(width = 4,
                    box(width = NULL,
                        tableOutput("summary_table")
                    )
             ),
             column(width = 4,
                    box(width = NULL,
                        plotOutput("timeseries_plot", height = 250)
                    )
             ),
             column(width = 4,
                    box(width = NULL,
                        plotlyOutput("bar_plot", height = 250)
                    )
             )
      ),
      column(width = 2,
             box(width = NULL, status = "warning",
                 selectInput("graph_type", "Metric",
                             choices = c(
                               'Time-to-close Issues/PRs' = 'ttclose'
                             ),
                             selected = 'ttclose'
                 ),
                 p(class = "text-muted",
                   br(),
                   "Time-to-comment & time-to-release are still TODO"
                 )
             ),
             box(width = NULL, status = "warning",
                 uiOutput("repoSelect"),
                 uiOutput("timeSinceLastUpdate"),
                 p(class = "text-muted",
                   br(),
                   "Source data updates daily."
                 )
             )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'ansible.collections'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
