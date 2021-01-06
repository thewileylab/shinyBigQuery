#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyWidgets actionBttn
#' @noRd
app_ui <- function(request) {
  tagList(
    # List the first level UI elements here 
    fluidPage(
      h1("shinyBigQuery"),
      shinyWidgets::actionBttn(inputId = "debug",
                               label = "Debug!", 
                               style = "stretch",
                               color = "warning"
                               ),
      bigquery_setup_ui('bq_setup')
    )
  )
}
