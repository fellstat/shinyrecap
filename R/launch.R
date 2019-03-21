
#' Launches the Shiny Application for Population Size
#' @param app Which application to launch.
#' @details
#' The manual for this shiny application is located at https://fellstat.github.io/shinyrecap/
#' @export
launchShinyPopSize <- function(app=c("estimation","power", "convert")){
  if(app[1] == "power")
    application <- "capture_power"
  else if(app[1] == "estimation")
    application <- "rcapture"
  else
    application <- "extract_histories"
  appDir <- system.file("apps", application, package = "shinyrecap")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `shinyrecap`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
