#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny shinipsum ggplot2 plotly
#' @importFrom dplyr select mutate across
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
    tmp <- repo_data() %>%
      get_survival_fit()

    fit <- survfit(Surv(time, status) ~ type, data = tmp)

    plot <- ggsurvplot(
      fit, # fitted survfit object
      data = tmp,
      break.x.by  = 10,
      xlim        = c(0,180),
      fun         = 'event',
      surv.scale  = 'percent',
      risk.table  = FALSE,
      conf.int    = TRUE,
      pval        = FALSE,
      pval.method = FALSE,
      surv.median.line = 'hv',
      legend.labs = c('Issues', 'Pull Requests'),
      ggtheme = theme_bw()      # Change ggplot2 theme
    )

    tbl <- surv_median(fit) %>%  mutate(across(is.numeric,round,1))
    tbl$strata <- c('Issues', 'Pull Requests')
    colnames(tbl) <- colnames(tbl) %>% str_to_title()

    ttheme <- gridExtra::ttheme_default(base_size = 20)
    g <- tibble::tibble(x = 0.95, y = 0.05, tbl = list(tbl))

    plot$plot <- plot$plot +
      theme(text = element_text(size=25)) +
      ggpmisc::geom_table_npc(data = g, aes(npcx = x, npcy = y, label = tbl),
                              table.theme = ttheme)
      labs(title = 'Time-to-close for Issues & Pull Requests',
           subtitle = 'For PRs, merged & closed are considered equivalent',
           x = 'Time (days)',
           y = 'Chance to be closed')

    plot
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

  output$summary_table <- DT::renderDT({
    req(input$repo)
    d <- repo_data()

    req(d$labels)
    d %>%
      tidyr::unnest(labels) %>%
      group_by(state) %>%
      dplyr::count(labels,sort=T) %>%
      tidyr::pivot_wider(names_from = state, values_from = n) %>%
      rename(Label = labels)
      DT::datatable(options = list(pageLength = 5, lengthChange = F, searching = F))
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
