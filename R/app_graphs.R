#' Fit a survival curve for issues/prs
#'
#' @keywords internal
#' @param d input data.frame of issues and prs
#' @noRd
issues_survival_plot <- function(d) {
  if (nrow(d) == 0) return(FALSE)

  tmp <- issues_survival_fit(d)
  labels <- get_labs(tmp)
  fit <- survfit(Surv(time, status) ~ type, data = tmp)

  plot <- ggsurvplot(
    fit, # fitted survfit object
    data = tmp,
    break.x.by  = get_x_break(tmp),
    break.y.by  = .1,
    fun         = 'event',
    surv.scale  = 'percent',
    risk.table  = FALSE,
    conf.int    = TRUE,
    pval        = FALSE,
    pval.method = FALSE,
    surv.median.line = 'hv',
    legend.labs = labels,
    ggtheme = theme_bw()      # Change ggplot2 theme
  )

  tbl <- surv_median(fit) %>%  mutate(across(is.numeric,round,1))
  tbl$strata <- labels
  colnames(tbl) <- colnames(tbl) %>% str_to_title()

  ttheme <- ttheme_default(base_size = 20)
  g <- tibble::tibble(x = 0.95, y = 0.05, tbl = list(tbl))

  plot$plot <- plot$plot +
    theme(text = element_text(size=20)) +
    ggpmisc::geom_table_npc(data = g, aes(npcx = x, npcy = y, label = tbl),
                            table.theme = ttheme) +
    labs(title = 'Time-to-close for Issues & Pull Requests',
         subtitle = 'For PRs, merged & closed are considered equivalent',
         x = 'Time (days)',
         y = 'Chance to be closed')

  plot
}

#' Fit a survival curve for time-to-first-comment
#'
#' @keywords internal
#' @param d input data.frame of issues and prs
#' @importFrom gridExtra ttheme_default
#' @noRd
comments_survival_plot <- function(d) {
  if (nrow(d) == 0) return(FALSE)

  tmp <- comments_survival_fit(d)
  if (nrow(tmp) == 0) return(FALSE)
  labels <- get_labs(tmp)
  fit <- survfit(Surv(time, status) ~ type, data = tmp)

  plot <- ggsurvplot(
    fit, # fitted survfit object
    data = tmp,
    break.x.by  = get_x_break(tmp),
    break.y.by  = .1,
    fun         = 'event',
    surv.scale  = 'percent',
    risk.table  = FALSE,
    conf.int    = TRUE,
    pval        = FALSE,
    pval.method = FALSE,
    surv.median.line = 'hv',
    legend.labs = labels,
    ggtheme = theme_bw()      # Change ggplot2 theme
  )

  tbl <- surv_median(fit) %>%  mutate(across(is.numeric,round,1))
  tbl$strata <- labels
  colnames(tbl) <- colnames(tbl) %>% str_to_title()

  ttheme <- ttheme_default(base_size = 20)
  g <- tibble::tibble(x = 0.95, y = 0.05, tbl = list(tbl))

  plot$plot <- plot$plot +
    theme(text = element_text(size=20)) +
    ggpmisc::geom_table_npc(data = g, aes(npcx = x, npcy = y, label = tbl),
                            table.theme = ttheme) +
    labs(title = 'Time-to-first comment for Issues & Pull Requests',
         subtitle = 'Filtering self-replies and bots',
         x = 'Time (days)',
         y = 'Chance to receive a comment')

  plot
}

#' @importFrom stringr str_replace
get_labs <- function(df) {
  df %>%
    pull(type) %>%
    unique() %>%
    str_replace('issue','Issues') %>%
    str_replace('pull','Pull Requests')
}

get_x_break <- function(df) {
  maxt <- as.numeric(max(df$time))
  order <- floor(log10(maxt))
  10^(order-1)
}
