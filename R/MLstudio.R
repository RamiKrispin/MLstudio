#' Lunch the ML Studio package
#'
#' @param launch.browser A Boolean variable, if true will lunch the app on a web browser (default is “TRUE”)
#' @examples
#' MLstudio(launch.browser = TRUE)

runML <- function(launch.browser = TRUE){
  suppressWarnings(
    shiny::runApp(system.file(package = "MLstudio"), launch.browser = launch.browser)
  )
}
