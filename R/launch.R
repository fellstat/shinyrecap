
#' Launches the Shiny Application for Population Size
#' @param app Which application to launch.
#' @details
#' The manual for this shiny application is located at https://fellstat.github.io/shinyrecap/
#' @export
launchShinyPopSize <- function(app=c("estimation","power")){
  if(app[1] == "power")
    application <- "capture_power"
  else
    application <- "rcapture"
  appDir <- system.file("apps", application, package = "shinyrecap")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `shinyrecap`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
