#' @title Run Shiny app
#'
#' @description This function allows you to launch the Shiny app when the
#' package is loaded, making use of the supplied winemag dataset
#' @export
#' @example
#' runDemo_complete()

runDemo_complete <- function() {

  appDir <- system.file("shiny-examples", "Wine_Map",
                        package = "ptdsProjectG3")
  if (appDir == "") {
    stop(
      "Could not find example directory. Try re-installing ptdsProjectG3"
      , call. = FALSE
    )
  }

  shiny::runApp(appDir, display.mode = "normal")

}


#' @title Run Updated Shiny app
#'
#' @description This function allows for launching the Shiny app when the
#' package is loaded, using data that is freshly webscraped from Winemag.com
#' @export
#' @example
#' runDemo_webscraped()

runDemo_webscraped <- function() {

  appDir <- system.file("shiny-examples", "Wine_Map_Simple",
                        package = "ptdsProjectG3")
  if (appDir == "") {
    stop(
      "Could not find example directory. Try re-installing ptdsProjectG3"
      , call. = FALSE
    )
  }

  shiny::runApp(appDir, display.mode = "normal")

}
