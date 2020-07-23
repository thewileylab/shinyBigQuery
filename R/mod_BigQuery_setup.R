# UI ----
#' BigQuery Setup UI
#'
#' This module is designed to guide a user through the process of authenticating with Google BigQuery. It is responsible for returning an authorization token, the user selected project,the user selected dataset, and a DBI connection to a BigQuery Dataset.
#'
#' @param id The module namespace
#' 
#' @return The BigQuery Setup UI
#' @export
#' 
bigquery_setup_ui <- function(id) {
  ns <- NS(id)
  tagList(
    golem_add_external_resources(),
    uiOutput(ns('bq_connect_project_ui'))
  )
}

# Server ----
#' BigQuery Setup Server
#'
#' @param id The Module namespace
#'
#' @return BigQuery connection variables and user information
#' @export
#'
#' @importFrom shiny NS tagList parseQueryString isolate
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_authorize_url oauth2.0_token oauth2.0_access_token
#' @importFrom bigrquery bq_auth bq_projects bq_project_datasets bigquery dbDisconnect
#' @importFrom tibble tibble deframe
#' @importFrom dplyr filter
#' @importFrom magrittr %>% 
#' @importFrom DBI dbConnect
#' @importFrom glue glue
#' 
bigquery_setup_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      ## Allows redirect to Google for Authentication if JS configured to prevent
      allow_nav_jscode <- 'window.onbeforeunload = null;'
      
      ## BigQuery Setup Values ----
      bigquery_setup <- reactiveValues(
        app_url = NULL
        )
      
      ## Client URL Information ----
      protocol <- isolate(session$clientData$url_protocol)
      hostname <- if (isolate(session$clientData$url_hostname) == '127.0.0.1') {
        'localhost'
      } else { isolate(session$clientData$url_hostname)
          }
      port <- isolate(session$clientData$url_port)
      pathname <- isolate(session$clientData$url_pathname)
      client_url <- if(is.null(port) | port == '') {
        glue::glue('{protocol}//{hostname}{pathname}')
        } else {
          glue::glue('{protocol}//{hostname}:{port}{pathname}')
          }
      ### Extract any parameters from the URL (anything that isn't one of the things above)
      params <- parseQueryString(isolate(session$clientData$url_search))

      ## OAuth Dance ----
      #### We can dance if we want to
      ### OAuth 2.0 Client ID
      secrets <- jsonlite::fromJSON(txt = system.file('extdata/OAuth_ClientID/client_secret.json', package = 'shinyBigQuery'))
      app <- oauth_app(appname = "shinyBigQuery",
                       key = secrets$web$client_id,
                       secret = secrets$web$client_secret,
                       redirect_uri = client_url
                       )
      
      ### Define Google as the endpoint (this one is canned)
      api <- oauth_endpoints("google")
      
      ### Always request the minimal scope needed. Here, we are requesting:
      ### - read only access to BigQuery
      ### - read only access to storage api (required to run queries)
      ### - view your email address
      ### - See your personal info, including any personal info you've made publicly available
      scopes <- "https://www.googleapis.com/auth/bigquery.readonly https://www.googleapis.com/auth/devstorage.read_only https://www.googleapis.com/auth/userinfo.email https://www.googleapis.com/auth/userinfo.profile"
      
      ## Google Endpoint
      google_auth_url <- oauth2.0_authorize_url(api, app, scope = scopes)
      redirect <- sprintf("location.replace(\"%s\");", google_auth_url)
      redirect_home <- sprintf("window.location.replace(\"%s\");", client_url)
      
      # Define the reactive BQ Setup UI ----
      bq_setup_ui <- reactive({
        if (is.null(params$code)) {
          tagList(
            # Create an action button to redirect to Google, ask nicely if we can use BigQuery
            actionButton(inputId = 'login',
                         label = 'Sign In with Google',
                         icon = icon(name = 'google'),
                         #### We can leave this app behind
                         onclick = HTML(allow_nav_jscode, redirect)
                         )
            )
          } else {
          # Create a UI offering a disconnect or selecting a project and dataset.
            tagList(
              HTML("You have authenticated with Google BigQuery. Please select from the list of available projects, or sign out and sign in with a different Google Account."),
              br(),
              # Create an action button to redirect to application home, which will clear any access tokens
              actionButton(inputId = 'logout',
                           label = 'Sign Out of Google',
                           icon = icon(name = 'sign-out-alt'),
                           onclick = HTML(allow_nav_jscode, redirect_home)
                           ),
              ## Otherwise, walk the user through selecting a project using reactive selectInput
              select_project_ui()
              )
            }
        })
      
      ### Create an authorization token and authenticate with google
      #### But when we come back, we'll run oauth2.0_token()
      observe({
        if(is.null(params$code)) {
          bigrquery::bq_deauth()
          } else { 
            token <- oauth2.0_token(app = app,
                                    endpoint = api,
                                    credentials = oauth2.0_access_token(api, app, params$code),
                                    cache = FALSE
                                    )
            #### And authenticate the UI
            bigrquery::bq_auth(token)
            bigquery_setup$bq_projects <- bigrquery::bq_projects()
          }
        })
      
      # Create project/dataset selectInputs, using the namespace function extracted from the session info
      select_project_ui <- reactive({
        req(bigquery_setup$bq_projects)
        selectInput(inputId = ns('bq_project_id'),
                    label = 'Select from Available Google Projects:',
                    choices = bigquery_setup$bq_projects)
        })
      
      # Collect the user inputs to pass to other elements of the application
      bq_project <- reactive({input$bq_project_id})
      
      # Create the output UI
      output$bq_connect_project_ui <- renderUI({ bq_setup_ui() })
      
      # Also Return a list of objects for use in other parts of the app. Keep the dataset separate so that bigrquery joins can be performed across datasets.
      return(bigquery_setup)
      
      }
    )
  }
