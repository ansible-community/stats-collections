# Script to run from cron for importing data
# The process is:
# * Read the csv file from crawler-config
# * Call the GraphQL script for each entry
# * Check they all exited 0
# * Call the Mongo import script for each json file

# Requires:
#   * System Environment - GITHUB_TOKEN for the GH GraphQL API

library(readr)
library(purrr)
library(dplyr)

d <- read.csv(here::here('crawler-config/collections_list.csv'),
              stringsAsFactors = F)

if (Sys.getenv('GITHUB_TOKEN') == '') {
  stop('Please set the GITHUB_TOKEN variable')
}

setwd(here::here('crawler'))

scan_repo <- function(org, repo) {
  if (interactive()) print(paste0('\nScanning:',org,'/',repo))
  system2('./issues_and_prs.py',
          c('--git-org', org,
            '--git-repo', repo,
            '--all'),
          stdout = F, stderr = F)
}

d %>%
  select(Org, Repo) %>%
  mutate(exitcode = map2_int(.$Org, .$Repo, with_progress(scan_repo))) -> r

import_repo <- function(org, repo, type) {
  file = if_else(type == 'pulls', 'pull_requests.json', 'issues.json')
  path = paste0(org,'%',repo,'/', file)

  if (interactive()) print(paste0("\n\nImporting:",path))

  system2('./import_to_mongo.py',
          c('--collection', type, path),
          stdout = F, stderr = F)
}

if (sum(r$exitcode) == 0) {
  f <- r %>%
    mutate(exitcode_i = map2_int(.$Org, .$Repo, with_progress(import_repo), 'issues'),
           exitcode_p = map2_int(.$Org, .$Repo, with_progress(import_repo), 'pulls'))
} else {
  print('Error in GH scanning')
}

if (sum(f$exitcode) > 0) {
  print('Error in Mongo imports')
}
