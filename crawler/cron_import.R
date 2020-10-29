# Script to run from cron for importing data
# The process is:
# * Read the csv file from crawler-config
# * Call the GraphQL script for each entry
# * Check they all exited 0
# * Call the Mongo import script for each json file

# Requires:
#   * System Environment - GITHUB_TOKEN for the GH GraphQL API

library(mongolite)
library(readr)
library(purrr)
library(dplyr)

d <- read.csv(here::here('crawler-config/collections_list.csv'),
              stringsAsFactors = F)

# Make sure we have Authentication for GitHub
if (Sys.getenv('GITHUB_TOKEN') == '') {
  stop('Please set the GITHUB_TOKEN variable')
}

setwd(here::here('crawler'))

# The actual crawler is written in Python, shell out to it
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

# Likewise the code that imports the resulting JSON is in Python
import_repo <- function(org, repo, type) {
  file = if_else(type == 'pulls', 'pull_requests.json', 'issues.json')
  path = paste0(org,'%',repo,'/', file)

  if (interactive()) print(paste0("\n\nImporting:",path))

  system2('./import_to_mongo.py',
          c('--collection', type, path),
          stdout = F, stderr = F)
}

# Check the exitcode column is all 0, and then proceed
if (sum(r$exitcode) == 0) {
  f <- r %>%
    mutate(exitcode_i = map2_int(.$Org, .$Repo, with_progress(import_repo), 'issues'),
           exitcode_p = map2_int(.$Org, .$Repo, with_progress(import_repo), 'pulls'))
} else {
  print('Error in GH scanning')
}

# Again, check exitcode before updating the timestamps
if (sum(f$exitcode) == 0) {

  # TODO make this read the configuration
  m <- mongo('cron',url = "mongodb://test:test@172.17.0.1:27017/ansible_collections")

  m$update(
    '{"_id":"crawler"}', sprintf('{"$set":{"last_run": "%s"}}', Sys.time()),
    upsert = TRUE)
} else {
  print('Error in Mongo imports')
}
