#' Takes a list of issues / PRs from Mongo and computes the survival
#' fit for both types
#'
#' @keywords internal
#' @importFrom dplyr select mutate
#' @noRd
issues_survival_fit <- function(d) {
  d %>%
    mutate(final_time = if_else(is.na(closedAt), Sys.time(), closedAt),
           time       = difftime(final_time, createdAt, units = 'days'),
           status     = if_else(state == 'OPEN', 0, 1)
    ) %>%
    select(time, status, type)
}

#' Takes a list of issues / PRs from Mongo and computes the survival
#' fit for both types
#'
#' @keywords internal
#' @importFrom lubridate ymd_hms
#' @importFrom dplyr select mutate
#' @noRd
comments_survival_fit <- function(d) {
  d %>%
    unnest_comments() -> tmp
  if (nrow(tmp) == 0) {
    tmp
  } else {
    tmp %>%
      mutate(status   = if_else(is.na(comments_author), 0, 1),
             end_time = dplyr::case_when(
               status == 1      ~ comments_createdAt,
               state  == 'OPEN' ~ Sys.time(),
               TRUE             ~ closedAt),
             time     = difftime(end_time, createdAt, units = 'days')
      ) %>%
      select(time, status, type)
  }
}

#' Takes a list of issues / PRs from Mongo and counts them up by
#' day/week/month for both opened (createdAt) and closed (closedAt) values.
#'
#' @keywords internal
#' @importFrom lubridate ymd_hms
#' @importFrom dplyr bind_rows mutate
#' @importFrom tidyr complete nesting unnest
#' @noRd
get_issue_trends <- function(d) {
  # We start with a list of Issues and PRs
  this_week = as.Date(cut(Sys.Date(),'week'))

  bind_rows(
    bin_by_time(d, createdAt, 'week', 'opened'),
    bin_by_time(d, closedAt, 'week', 'closed')
  ) %>%
    # drop open/close for this week, likely incomplete
    filter(date < this_week) %>%
    # filter to timerange
    filter(date >= this_week - lubridate::weeks(8)) -> tmp

  if (nrow(tmp) == 0) return(tibble(date=1,n=1,id=1,.rows=0))

  tmp %>%
    # fill missing for ggplot2
    complete(nesting(id),
             fill = list(n = 0),
             date = seq.Date(from = min(date),
                             to   = max(date),
                             by   ='week'))
}

#' Tidyeval function to do the actual date-cutting and counting on a given
#' column.
#'
#' @keywords internal
#' @import rlang
#' @importFrom dplyr mutate filter count arrange if_else
#' @noRd
bin_by_time <- function(dat,var,period='month',id=NULL) {
  dat %>%
    filter(!(is.na({{var}}))) -> tmp
  if (nrow(tmp) == 0) return(data.frame())
  tmp %>%
    mutate(date = as.Date(cut({{var}},period))) %>%
    count(date) %>%
    mutate(id = if_else(is.null(id),
                        rlang::as_name(ensym(var)),
                        id)) %>%
    arrange(date)
}

#' List of known bots
#'
#' @keywords internal
#' @noRd
bot_list <- function() {
  c(
    'ansibullbot'
  )
}

#' Take a list of issues and unnest the first comment by a different human
#'
#' @keywords internal
#' @importFrom lubridate ymd_hms
#' @importFrom tibble as_tibble
#' @noRd
unnest_comments <- function(d) {
  if (is.null(unlist(d$comments$nodes))) return(data.frame())

  as_tibble(d) %>%
    dplyr::mutate(comments = comments$nodes,
           author   = author$login) %>%
    dplyr::mutate(comments = purrr::map2(comments, author, ~{
      if (nrow(.x) == 0) return(data.frame())
      .x %>%
        dplyr::mutate(author = author$login) %>%
        dplyr::filter((author != .y) & (!(author %in% bot_list()))) %>%
        head(1)
      })
    ) %>%
    unnest(c(comments), names_sep = '_', keep_empty = T) %>%
    mutate(comments_createdAt = ymd_hms(comments_createdAt))
}
