#' Luanch the ML Studio app in the default browser
#'
#' @examples
#' runML()
#' @details For more details please check the github page: https://github.com/RamiKrispin/MLstudio
#'
runML <- function(){
  suppressPackageStartupMessages(
    shiny::runApp(system.file(package = "MLstudio"), launch.browser = TRUE)
)
}
