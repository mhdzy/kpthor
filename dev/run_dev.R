# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
# run_app()

# run app in background and get open PID's for later
system("R -e \"shiny::runApp('app.R', port = 3838)\"", wait = FALSE)
test_pid <- system("ps aux | grep \"R -e shiny::runApp\" | awk '{print $2}'", intern = TRUE)

# let app boot up
Sys.sleep(2)

# run test in separate window
shinyMobile::preview_mobile(
  appPath = system.file("app.R", package = "kpthor"),
  device = "iphone8"
)

unlist(lapply(paste("kill -9", test_pid), system))
