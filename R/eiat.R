#' Economic Impact Assessment Tool
#'
#' @export
#'
eiat <- function() {
  shiny::runApp(appDir = system.file("shiny", package = "eiat"))
}
