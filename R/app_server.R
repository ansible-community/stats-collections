#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny shinipsum ggplot2 plotly
#' @importFrom dplyr select mutate across pull
#' @importFrom purrr map
#' @importFrom survival survfit Surv
#' @importFrom survminer ggsurvplot surv_median
#' @importFrom stringr str_to_title
#' @noRd
app_server <- function( input, output, session ) {

  colours = c("closed"='#CB333B',"opened"='#5bbdc0')

  repo_data <- reactive({
    req(input$repo)
    get_repo_data(input$repo)
  })

  output$repoSelect <- renderUI({
    #later, see below
    #repositories <- get_repos()
    repositories <- c('ansible-collections/community.general',
                      'ansible-collections/community.grafana',
                      'ansible-collections/azure')

    selectInput("repo", "Collection",
                choices = repositories, selected = repositories[1])
  })

  output$survival_plot <- renderPlot({
    switch (input$graph_type,
      'ttclose'   = issues_survival_plot(repo_data()),
      'ttcomment' = comments_survival_plot(repo_data()),
    )
  })

  output$timeseries_plot <- renderPlot({
    random_ggplot('point') +
      labs(title = 'Survial Trend still TBD',
           subtitle = 'This is just a placeholder graph')
  })

  output$bar_plot <- renderPlotly({
    p <- repo_data() %>%
      get_issue_trends() %>%
      rename(Date = date, Count = n) %>%
      ggplot(aes(x=Date, y = Count, fill = id)) +
      geom_col(position='dodge') +
      scale_x_date(date_breaks = '1 week', date_labels = '%d/%m') +
      scale_fill_manual(name = NULL,
                          values = colours,
                          labels = c("Closed","Opened")) +
      labs(title = 'Issues & PRs Opened/Closed',
           x = 'Week Commencing', y = NULL) +
      theme(legend.position = 'top')

    ggplotly(p, tooltip = c('Date','Count')) %>%
      config(modeBarButtonsToRemove = list('select2d','lasso2d'),
             displaylogo = FALSE, scrollZoom = FALSE) %>%
      layout(xaxis=list(fixedrange=TRUE)) %>%
      layout(yaxis=list(fixedrange=TRUE))
  })

  output$label_table <- DT::renderDT({
    req(input$repo)
    d <- repo_data()

    req(d$labels)
    d %>%
      tidyr::unnest(labels) %>%
      group_by(state) %>%
      dplyr::count(labels,sort=T) %>%
      tidyr::pivot_wider(names_from = state, values_from = n) %>%
      rename(Label = labels) %>%
      DT::datatable(options = list(pageLength = 10,
                                   lengthChange = F, searching = F))
  })

  output$issuesBox <- renderValueBox({
    req(input$repo)
    d <- repo_data()
    valueBox(
      d %>%
        count(type,state) %>%
        filter(type == 'issue' & state == 'OPEN') %>%
        pull(n),
      "Issues Open", icon = icon("exclamation-circle"),
      color = "green"
    )
  })

  output$pullsBox <- renderValueBox({
    req(input$repo)
    d <- repo_data()
    valueBox(
      d %>%
        count(type,state) %>%
        filter(type == 'pull' & state == 'OPEN') %>%
        pull(n),
      "Pull Requests Open", icon = icon("exchange-alt"),
      color = "yellow"
    )
  })

  output$contribBox <- renderValueBox({
    req(input$repo)
    d <- repo_data()

    valueBox(
      d %>%
        distinct(author) %>%
        count() %>%
        pull(n),
      "Unique Contributors", '(by Github login)',
      icon = icon("users"),
      color = "aqua"
    )
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
