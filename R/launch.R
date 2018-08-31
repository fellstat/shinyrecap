
#' Launches the Shiny Application for Population Size
#' @export
launchShinyPopSize <- function(){
  application <- "rcapture"
  appDir <- system.file("apps", application, package = "shinyrecap")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `shinyrecap`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
