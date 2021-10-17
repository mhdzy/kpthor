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
Sys.sleep(3)

# run test in separate window
shinyMobile::preview_mobile(
  appPath = system.file("app.R", package = "kpthor"),
  device = "iphone8"
)

# kill the shiny system process
unlist(lapply(paste("kill -9", test_pid), system))

# reload dev libraries from pkg DESCRIPTION file
libs_sub <- function(x) sub(" .*", "", trimws(stringi::stri_split(yaml::read_yaml("DESCRIPTION")[[x]], fixed = ",")[[1]]))
libs_req_sug <- c(libs_sub("Imports"), libs_sub("Suggests"))
invisible(suppressMessages(sapply(c(libs_req_sug), library, character.only = TRUE)))
