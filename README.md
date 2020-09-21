# stats-collections

RShiny app to display statistics for the Ansible Collections

# Structure

This app is a [Shiny](https://shiny.rstudio.com/) application, built as an
[R package](https://cran.r-project.org/) using
[Golem](https://github.com/ThinkR-open/golem/) and deployed as a Docker
container in [ShinyProxy](https://shinyproxy.io/).

# Configuration

The app draws data from GitHub - the list of repos that it indexes is currently
contained in a CSV file. See the [crawler-config](crawler-config/) directory for
details on the format.

(N.B. The crawler is likely to move to its own repo at some point).

# Shinyproxy Config Block

As a Docker container, the app gets it's configuration via environment
variables from ShinyProxy. An example block:

```
- id: collections-survival
  display-name: Survival Metrics for Ansible Collections
  description: Analysis of various time-to-event metrics for the Ansible Collections
  container-image: collections-survival
  container-env:
    DBUSER: test
    DBPASS: test
    DBPORT: 27017
    DBNAME: ansible_collections
```
