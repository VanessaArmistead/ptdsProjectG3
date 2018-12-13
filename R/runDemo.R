#' @title Run Shiny app
#'
#' @description This function allows for launching the Shiny app when the
#' package is loaded.
#' @author Vanessa Armistead
#' @author Bart Roes
#' @author Luisa Pricken
#' @author Ameni Rouatbi
#' @export

runDemo <- function() {

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
