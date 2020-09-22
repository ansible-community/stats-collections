#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny shinipsum ggplot2 plotly
#' @importFrom dplyr select mutate
#' @importFrom purrr map
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
    repo <- input$repo
    random_ggplot() + labs(title = repo)
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

  output$summary_table <- renderTable({
    req(input$repo)
    d <- repo_data()

    req(d$labels)
    d %>%
      tidyr::separate_rows(labels,sep=',') %>%
      dplyr::filter(labels != '') %>%
      dplyr::count(labels,sort=T) %>%
      head(5)
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


#' @importFrom mongolite mongo
#' @noRd
setup_mongo <- function(collection) {
  mongo(collection, url = mongo_string())
}

#' @keywords internal
#' @export
#' @importFrom glue glue
#' @noRd
mongo_string <- function() {
  DBUSER <- Sys.getenv('DBUSER')
  DBPASS <- Sys.getenv('DBPASS')
  DBPORT <- Sys.getenv('DBPORT')
  DBNAME <- Sys.getenv('DBNAME')

  glue("mongodb://{DBUSER}:{DBPASS}@172.17.0.1:{DBPORT}/{DBNAME}")
}

#' Takes a repo name and gets the relevant dataframes from Mongo
#' @importFrom glue glue
#' @importFrom dplyr bind_rows
#' @noRd
get_repo_data <- function(repo) {
  query <- glue('{{"repository.nameWithOwner":"{repo}"}}')
  base_fields <- '{
    "number":true,
    "author":true,
    "title":true,
    "state":true,
    "createdAt":true,
    "closedAt":true,
    "labels":true,
    "repository":true
  }'

  db_issues <- setup_mongo('issues')
  issues <- db_issues$find(query, base_fields) %>%
    mutate(type = 'issue')
  db_issues$disconnect() ; rm(db_issues)

  # Add extra fields for PRs
  new_fields <- jsonlite::parse_json(base_fields)
  new_fields$mergedAt <- TRUE
  new_fields$merged   <- TRUE
  new_fields <- jsonlite::toJSON(new_fields, auto_unbox = T)

  db_pulls <- setup_mongo('pulls')
  pulls <- db_pulls$find(query, new_fields) %>%
    mutate(type = 'pull')
  db_pulls$disconnect() ; rm(db_pulls)

  bind_rows(issues, pulls)
}

get_repos <- function() {
  # later, use mapreduce
}
