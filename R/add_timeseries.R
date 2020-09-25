#' Retrieve a time series from Mongo
#'
#' @keywords internal
#' @param repo the id of the timeseries
#' @noRd
get_timeseries <- function(repo) {
  d <- setup_mongo('timeseries')
  d <- d$find(glue('{{"_id":"{repo}"}}'))$timeseries$data[[1]]
  d %>%
    mutate(date = lubridate::ymd(date)) %>%
    ggplot(aes(date,median)) +
    geom_point(size=3) +
    geom_smooth() +
    scale_x_date(date_breaks = '1 week', date_labels = '%d/%m') +
    theme(text = element_text(size=20)) +
    labs(title = 'Trend in time-to-merge',
         subtitle = '(this is fake data, the real data will be added over time)',
         x = NULL, y = 'Median time-to-close')
}
