#' Retrieve a time series from Mongo
#'
#' @keywords internal
#' @param repo the id of the timeseries
#' @noRd
plot_timeseries <- function(repo, graph) {
  if (graph == 'galaxy') {
    return(plotly_empty() %>% add_text(x=1, y=1, size = I(100), text = 'N/A'))
  }

  if (graph == 'ttclose') {
    d   <- 'trend_close'
    txt <- 'Time to close'
  } else {
    d   <- 'trend_comment'
    txt <- 'Time to comment'
  }
  d <- setup_mongo(d)
  d <- d$find(glue('{{"_id":"{repo}"}}'))

  data.frame(
    date  = names(d),
    pull  = map(d,'pull') %>% unlist(),
    issue = map(d,'issue') %>% unlist()
  ) %>%
    mutate(date = lubridate::ymd(date),
           across(where(is.character), as.numeric),
           across(where(is.numeric), round, 2)) %>%
    arrange(date) %>%
    tidyr::pivot_longer(c('pull', 'issue'), names_to = 'type') %>%
    na.omit() -> d

  if (nrow(d) == 0) { return(FALSE)}

  d %>%
    rename(Date = date, Value = value) %>%
    ggplot(aes(Date,Value,colour=type)) +
    facet_wrap(~type, scales = 'free_y') +
    geom_point() +
    geom_smooth() +
 #   scale_x_date(date_breaks = '1 week', date_labels = '%d/%m') +
    labs(title = glue('Trend in {txt} (weekly, from above)'),
         x = NULL, y = glue('75% {txt}')) -> p

  ggplotly(p, tooltip = c('Date','Value')) %>%
    config(modeBarButtonsToRemove = list('select2d','lasso2d'),
           displaylogo = FALSE, scrollZoom = FALSE) %>%
    layout(xaxis=list(fixedrange=TRUE)) %>%
    layout(yaxis=list(fixedrange=TRUE)) %>%
    layout(showlegend=FALSE)
}

#' Cronjob for time-to-close Issues/PRs
#'
#' Call this function from the commandline (i.e. cron) to
#' store the current time-to-close metrics to the DB for
#' all known repos in the DB
#'
#' You'll want an environment file with the Mongo connection info
#' in it, and then call
#'
#' sudo docker run --env-file /path/to/cron.env -it \
#'   --rm collections:tag R -e "ansible.collections::cron_trend_close()"
#'
#' @importFrom purrr possibly map2
#' @export
cron_trend_close <- function() {
  # Safe Purrr::map
  possibly_get   <- possibly(get_repo_data,NA)
  possibly_fit_i <- possibly(issues_survival_fit,NA)
  possibly_fit_c <- possibly(comments_survival_fit,NA)
  fit_func <- function(fit) {
    if (is.na(fit)) {
      NA
    } else {
      survfit(Surv(time, status) ~ type, data = fit)
    }
  }

  # Process the survival curves
  get_repos() %>%
    tibble::as_tibble_col(column_name = 'repo') %>%
    mutate(data = map(repo, possibly_get)) %>% # tiny repos can fail this step
    filter(!is.na(data)) %>%
    mutate(types = map(data, ~{unique(.x$type)}),
           fit_i = map(data, possibly_fit_i),
           fit_c = map(data, possibly_fit_c)) %>%
    mutate(fit_i = map(fit_i, fit_func),
           fit_c = map(fit_c, fit_func)) %>%
    mutate(close   = map2(fit_i, types, get_75_line),
           comment = map2(fit_c, types, get_75_line)) %>%
    select(repo, close, comment) %>%
    tidyr::unnest(c(close,comment), names_sep='_') %>%
    mutate(date = Sys.Date()) -> todays_data

  # Store it
  for (r in 1:nrow(todays_data)) {
    update_db_trend(todays_data[r,c(1,2,3,6)],'trend_close')
    update_db_trend(todays_data[r,c(1,4,5,6)],'trend_comment')
  }
}

#' Calculate the 75% quantile for a given survival dataset
#'
#' @keywords internal
#' @param fit the incoming data
#' @param types the strata for the survival fit
#' @noRd
get_75_line <- function(fit, types) {

  if (is.na(fit)) {
    return(data.frame(pull = NA, issue = NA))
  }

  defaults <- data.frame(strata = c('issue','pull')) # will bring in NAs as needed
  d <- data.frame(time = fit$time,
                  surv = fit$surv)
  if (length(types) == 2) {
    d$strata <- c(rep(types[1], fit$strata[1]), rep(types[2], fit$strata[2]))
  } else {
    d$strata <- types
  }

  d %>%
    dplyr::filter(surv < 0.25) %>%
    dplyr::group_by(strata) %>%
    dplyr::summarise(time = head(time,1), .groups = 'drop') %>%
    dplyr::mutate(strata = stringr::str_remove(strata,'^type=')) %>%
    dplyr::right_join(defaults, by = 'strata') %>%
    tidyr::pivot_wider(names_from = 'strata', values_from = 'time')
}

#' Load the current values into Mongo
#'
#' @keywords internal
#' @param df the incoming data
#' @noRd
update_db_trend <- function(df,table) {
  setup_mongo(table)$update(
    glue::glue('{{"_id":"{df$repo}" }}'),
    glue::glue('{{
                  "$set": {{
                    "{df$date}": {{
                      "pull":  "{df[2]}",
                      "issue": "{df[3]}"
                    }}
                  }}
                }}'),
    upsert = TRUE)
}

