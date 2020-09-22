# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Fake Docker environment vars for dev use
# Set these as needed to test against your Mongo instance
Sys.setenv(DBUSER = 'test',
           DBPASS = 'test',
           DBPORT = 27017,
           DBNAME = 'ansible_collections')

# Run the application
run_app()
