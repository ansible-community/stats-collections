
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
#' @importFrom lubridate ymd_hms
#' @importFrom dplyr bind_rows
#' @noRd
get_repo_data <- function(repo) {
  query <- glue('{{"repository.nameWithOwner":"{repo}"}}')
  base_fields <- '{
    "number":true,
    "author.login":true,
    "title":true,
    "state":true,
    "createdAt":true,
    "closedAt":true,
    "labels":true,
    "repository":true,
    "comments.nodes.createdAt":true,
    "comments.nodes.author.login":true
  }'

  db_issues <- setup_mongo('issues')
  issues <- db_issues$find(query, base_fields) %>%
    mutate(type = 'issue')
  db_issues$disconnect() ; rm(db_issues)

  # Add extra fields for PRs
  new_fields <- jsonlite::parse_json(base_fields)
  new_fields$mergedAt    <- TRUE
  new_fields$merged      <- TRUE
  new_fields$baseRefName <- TRUE
  new_fields <- jsonlite::toJSON(new_fields, auto_unbox = T)

  db_pulls <- setup_mongo('pulls')
  pulls <- db_pulls$find(query, new_fields) %>%
    mutate(type = 'pull') %>%
    filter(baseRefName %in% c('main', 'master'))
  db_pulls$disconnect() ; rm(db_pulls)

  bind_rows(issues, pulls) %>%
    mutate(createdAt = ymd_hms(createdAt),
           closedAt  = ymd_hms(closedAt))
}

get_repos <- function() {
  # later, use mapreduce
  db_issues <- setup_mongo('issues')
  r1 <- db_issues$distinct('repository.nameWithOwner')
  db_issues$disconnect() ; rm(db_issues)

  db_pulls <- setup_mongo('pulls')
  r2 <- db_pulls$distinct('repository.nameWithOwner')
  db_pulls$disconnect() ; rm(db_pulls)

  d <- unique(c(r1,r2))
  d[!(d %in% c('ansible/ansible'))] # too big!
}
