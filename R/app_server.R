#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny shinipsum ggplot2
#' @importFrom ggplot2 labs
#' @noRd
app_server <- function( input, output, session ) {

  output$survival_plot <- renderPlot({
    repo <- input$repo
    random_ggplot() + labs(title = repo)
  })

  output$timeseries_plot <- renderPlot({
    repo <- input$repo
    random_ggplot('point')
  })

  output$bar_plot <- renderPlot({
    repo <- input$repo
    random_ggplot('bar')
  })

  output$summary_table <- renderTable({
    random_table(5,3)
  })

  output$timeSinceLastUpdate <- renderUI({
    # Get this from the DB later
    p(
      class = "text-muted",
      "Data refreshed",
      round(digits = 1,
        difftime(
          Sys.time(),
          file.mtime('crawler/ansible-collections%community.general/issues.json'),
          units="hours")
      ),
      " hours ago."
    )
  })
}
