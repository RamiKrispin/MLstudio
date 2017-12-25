#' Launch the ML Studio app in the default browser
#' @import shiny shinydashboard
#' @export
#' @details For more details please check the github page: https://github.com/RamiKrispin/MLstudio
#'@examples
#' \dontrun{
#' runML()
#' }
#'
runML <- function(){
  suppressPackageStartupMessages(
    shiny::runApp(system.file(package = "MLstudio"), launch.browser = TRUE)
  )
}
