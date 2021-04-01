#' Run the Shiny Application
#'
#' @param ... A series of options/params to be used inside the app including:
#' \itemize{
#' \item{secrets_json: A string, containing a file path to a Google OAuth 2.0 Client}
#' }
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' 
#' @return No return value, called to start the Shiny Application!
run_app <- function(...) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server,
      options = list(port=1410,launch.browser=T)),
    golem_opts = list(...)
  )
}
#' shinyBigQuery: Access Google BigQuery through R Shiny
#' 
#' A shiny module to authenticate your R Shiny Application with Google BigQuery.
#' 
#' 
#' @docType package
#' @name shinyBigQuery
NULL
#> NULL