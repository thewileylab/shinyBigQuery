#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_example <- function(...) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server,
      options = list(port = 8100,launch.browser=T)),
    golem_opts = list(...)
  )
}
