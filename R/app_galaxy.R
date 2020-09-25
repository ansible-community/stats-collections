#' Retrieve galaxy data on collection releases and do something useful with it
#'
#' @keywords internal
#' @noRd
galaxy_plot <- function(org_repo) {
  repo <- stringr::str_split_fixed(org_repo,'/',2)[[2]]
  if (!stringr::str_detect(repo,'\\.')) {
    # base graphics
    text(x = 0.5, y = 0.5, paste("No Galaxy information to display\n\n",
                                 "Galaxy information can currently only be",
                                 "retrieved for collections in 'x.y' format"),
         cex = 1.6, col = "black")
  } else {
    d <- get_galaxy_info(repo)
    if (class(d) == 'character') {
      text(x = 0.5, y = 0.5, paste("No Galaxy information to display\n\n",
                                   "Data retrival from Galaxy API failed\n",
                                   "Please inform the Ansible Community Team"),
           cex = 1.6, col = "black")
    } else {
      # ggplot2
      d <- d$data$collection
      r <- data.frame(
        version = purrr::map_chr(d$all_versions,'version'),
        created = purrr::map_chr(d$all_versions,'created') %>% lubridate::ymd_hms()
      ) %>%
        filter(!stringr::str_detect(version,'dev|beta')) %>%
        dplyr::add_row(version = 'init', created = lubridate::ymd_hms(d$created),
                       .before = 1) %>%
        dplyr::add_row(version = 'next?', created = Sys.time()) %>%
        arrange(created) %>%
        dplyr::mutate(prev_version = dplyr::lag(created)) %>%
        filter(!is.na(prev_version)) %>%
        dplyr::mutate(release_diff = difftime(created, prev_version, units = 'days'),
                      colour = 'grey')

      m <- median(r$release_diff)
      t <- tail(r,1) %>% pull(release_diff)
      c <- dplyr::if_else(t > m, '#CB333B', '#5bbdc0')
      r[nrow(r),5] <- c

      r %>%
        dplyr::mutate(t = as.double(release_diff)) %>%
        ggplot(aes(x=version, y=t, fill=I(colour))) +
        geom_col() +
        geom_hline(yintercept = m) +
        labs(title = 'Release frequency',
             subtitle = 'Horizontal line shows the median release time',
             x = 'Version', y = 'Days since release',
             caption = 'Source: Ansible Galaxy API') +
        theme(text = element_text(size=20))
    }
  }

}

#' Get the Galaxy API data for a repo
#'
#' @keywords internal
#' @noRd
get_galaxy_info <- function(repo) {

  match <- stringr::str_split_fixed(repo,'\\.',2)
  org   <- match[[1]]
  repo  <- match[[2]]

  url <- sprintf("https://galaxy.ansible.com/api/internal/ui/repo-or-collection-detail/?namespace=%s&name=%s",org,repo)

  r <- httr::GET(url)
  if (r$status_code == 200) {
    httr::content(r)
  } else {
    "Failed"
  }
}
