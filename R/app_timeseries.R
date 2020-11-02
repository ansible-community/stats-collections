#' Retrieve a time series from Mongo
#'
#' @keywords internal
#' @param repo the id of the timeseries
#' @noRd
plot_timeseries <- function(repo) {
  d <- setup_mongo('trend_close')
  d <- d$find(glue('{{"_id":"{repo}"}}'))#$timeseries$data[[1]]

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
    ggplot(aes(date,value,colour=type)) +
    facet_wrap(~type, scales = 'free_y') +
    geom_point(size=3) +
    geom_smooth() +
    guides(color = F) +
    scale_x_date(date_breaks = '1 week', date_labels = '%d/%m') +
    theme(text = element_text(size=20)) +
    labs(title = 'Trend in time-to-close (weekly, from above)',
         x = NULL, y = '75% Time-to-close')
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
  possibly_get <- possibly(get_repo_data,NA)

  # Process the survival curves
  get_repos() %>%
    tibble::as_tibble_col(column_name = 'repo') %>%
    mutate(data = map(repo, possibly_get)) %>%
    mutate(types = map(data, ~{unique(.x$type)})) %>%
    mutate(fit = map(data, issues_survival_fit)) %>%
    mutate(fit = map(fit, ~{survfit(Surv(time, status) ~ type, data = .x)})) %>%
    mutate(summary = map2(fit, types, get_75_line)) %>%
    select(repo, summary) %>%
    tidyr::unnest(summary) %>%
    mutate(date = Sys.Date()) -> todays_data

  # Store it
  for (r in 1:nrow(todays_data)) {
    update_db_trend_close(todays_data[r,])
  }
}

#' Calculate the 75% quantile for a given survival dataset
#'
#' @keywords internal
#' @param fit the incoming data
#' @param types the strata for the survival fit
#' @noRd
get_75_line <- function(fit, types) {
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
    dplyr::summarise(time = head(time,1)) %>%
    dplyr::mutate(strata = stringr::str_remove(strata,'^type=')) %>%
    dplyr::right_join(defaults) %>%
    tidyr::pivot_wider(names_from = 'strata', values_from = 'time')
}

#' Calculate the 75% quantile for a given survival dataset
#'
#' @keywords internal
#' @param df the incoming data
#' @noRd
update_db_trend_close <- function(df) {
  setup_mongo('trend_close')$update(
    glue::glue('{{"_id":"{df$repo}" }}'),
    glue::glue('{{
                  "$set": {{
                    "{df$date}": {{
                      "issue": "{df$issue}",
                      "pull": "{df$pull}"
                    }}
                  }}
                }}'),
    upsert = TRUE)
}

