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
#' @importFrom shinydashboard box
#' @importFrom shinyjs hidden
bigquery_setup_ui <- function(id) {
  ns <- NS(id)
  tagList(
    golem_add_external_resources(),
    div(id = ns('google_connect_div'),
      shinydashboard::box(title = 'Connect to BigQuery',
                          width = '100%',
                          status = 'primary',
                          solidHeader = F,
                          HTML('To connect to Google BigQuery, please sign in with your Google Account.'),
                          br(),
                          actionButton(inputId = ns('login'),
                                       label = 'Sign In with Google',
                                       icon = icon(name = 'google')
                                       )
                          )
      ),
    shinyjs::hidden(
      div(id = ns('google_authenticated_div'),
          uiOutput(ns('bq_authenticated_ui'))
          )
      )
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
#' @importFrom bigrquery bq_auth bq_projects bq_project_datasets bigquery dbDisconnect
#' @importFrom DBI dbConnect
#' @importFrom dplyr filter pull
#' @importFrom gargle token_userinfo
#' @importFrom glue glue
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_authorize_url oauth2.0_token oauth2.0_access_token
#' @importFrom jsonlite fromJSON
#' @importFrom magrittr %>% 
#' @importFrom purrr flatten
#' @importFrom shinyjs runjs
#' @importFrom shinydashboardPlus widgetUserBox
#' @importFrom tibble tibble enframe
#' @importFrom tidyr unnest
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
        user_info = NULL,
        bq_projects = NULL
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
      
      observeEvent(input$login, ignoreInit = T, {
        #### We can leave this app behind
        shinyjs::runjs( HTML(allow_nav_jscode, redirect) )
        })

      observeEvent(input$logout, {
        shinyjs::runjs( HTML(allow_nav_jscode, redirect_home) )
        shinyjs::hide(id = 'google_authenticated_div')
        shinyjs::show(id = 'google_connect_div')
      })
      
      ### Create an authorization token and authenticate with Google
      #### But when we come back, we'll run oauth2.0_token()
      observe({
        if(is.null(params$code)) {
          bigrquery::bq_deauth()
          } else { 
            shinyjs::hide(id = 'google_connect_div')
            shinyjs::show(id = 'google_authenticated_div')
            token <- oauth2.0_token(app = app,
                                    endpoint = api,
                                    credentials = oauth2.0_access_token(api, app, params$code),
                                    cache = FALSE
                                    )
            #### And authenticate the UI
            bigrquery::bq_auth(token = token)
            bigquery_setup$user_info <- gargle::token_userinfo(token = token)
            bigquery_setup$bq_projects <- bigrquery::bq_projects()
            # browser()
          }
        })
      google_connected_ui <- reactive({
        req(bigquery_setup$user_info)
        tagList(
          shinydashboardPlus::widgetUserBox(title = bigquery_setup$user_info$name,
                                            subtitle = bigquery_setup$user_info$email,
                                            src = bigquery_setup$user_info$picture,
                                            type = 2, 
                                            color = 'primary',
                                            collapsible = FALSE,
                                            HTML("You have authenticated with Google BigQuery. Please select from the list of available projects, or sign out and sign in with a different Google Account."),
                                            br(),
                                            actionButton(inputId = ns('logout'),
                                                         label = 'Sign Out of Google',
                                                         icon = icon(name = 'sign-out-alt')
                                                         ),
                                            selectInput(inputId = ns('bq_project_id'),
                                                        label = 'Select from Available Google Projects:',
                                                        choices = bigquery_setup$bq_projects
                                                        ),
                                            selectizeInput(inputId = ns('bq_dataset_id'),
                                                           label = 'Select from Available BigQuery Datasets:',
                                                           choices = NULL)
                                            )
        )
      })
      
      observeEvent(input$bq_project_id, {
        req(input$bq_project_id)
        bigquery_setup$bq_project <- input$bq_project_id
        dataset_choices <- bigrquery::bq_project_datasets(bigquery_setup$bq_project) %>% 
          purrr::flatten() %>% 
          tibble::enframe() %>% 
          dplyr::filter(.data$name == 'dataset') %>% 
          tidyr::unnest(.data$value) %>% 
          dplyr::pull(.data$value)
        # browser()
        updateSelectizeInput(session = session,
                             inputId = 'bq_dataset_id',
                             choices = dataset_choices,
                             server = T,
                             options = list(create = FALSE,
                                            placeholder = 'Please Select A Project')
                             )
      })
      # BigQuery Setup Outputs ----
      output$bq_authenticated_ui <- renderUI({ google_connected_ui() })
      
      # Return Setup Values. Keep the dataset separate so that bigrquery joins can be performed across datasets.
      return(bigquery_setup)
      }
    )
  }
