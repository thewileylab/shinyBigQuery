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
#' @importFrom shinycssloaders withSpinner
bigquery_setup_ui <- function(id) {
  ns <- NS(id)
  tagList(
    golem_add_external_resources(),
    # uiOutput(ns('token_info')), ## Debug
    div(id = ns('google_connect_div'),
      shinydashboard::box(title = 'Connect to BigQuery',
                          width = '100%',
                          status = 'primary',
                          solidHeader = F,
                          HTML('To connect to Google BigQuery, please sign in with your Google Account.<br><br>'),
                          br(),
                          actionButton(inputId = ns('login'),
                                       label = 'Sign In with Google',
                                       icon = icon(name = 'google')
                                       )
                          )
      ),
    shinyjs::hidden(
      div(id = ns('google_authenticated_div'),
          uiOutput(ns('google_authenticated_ui')) %>% shinycssloaders::withSpinner()
          ),
      div(id = ns('google_configured_div'),
          uiOutput(ns('google_configured_ui')) %>% shinycssloaders::withSpinner()
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
#' @importFrom rlang .data
#' @importFrom shinyjs runjs show hide
#' @importFrom shinydashboardPlus widgetUserBox
#' @importFrom shinyWidgets actionBttn
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
        ### Module Info
        moduleName = 'BigQuery',
        moduleType = 'database',
        ui = shinyBigQuery::bigquery_setup_ui(id),
        ### Connection Variables
        user_info = NULL,
        bq_projects = NULL,
        bq_project_id = NULL,
        bq_dataset_id = NULL,
        db_con = NULL,
        is_connected = 'no'
        )
      google_info <- reactiveValues(
        is_authorized = 'no'
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
      params <- reactive({ parseQueryString(isolate(session$clientData$url_search)) })
      observeEvent(params(), {
        if(is.null(params()$code)) {
          google_info$is_authorized <- 'no' 
        } else {
            google_info$is_authorized <- 'yes'
          }
      })
      
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
      
      ### When the login button is pressed, redirect to Google for authentication
      observeEvent(input$login, ignoreInit = T, {
        #### We can leave this app behind
        shinyjs::runjs( HTML(allow_nav_jscode, redirect) )
        })
      
      ### When the disconnect button is pressed, reset the UI by redirecting to the base url, minus the authorization code
      observeEvent(input$logout, {
        shinyjs::runjs( HTML(allow_nav_jscode, redirect_home) )
        shinyjs::hide(id = 'google_authenticated_div')
        shinyjs::show(id = 'google_connect_div')
      })
      observeEvent(input$logout_2, {
        shinyjs::runjs( HTML(allow_nav_jscode, redirect_home) )
        shinyjs::hide(id = 'google_authenticated_div')
        shinyjs::show(id = 'google_connect_div')
      })
      
      ### Create an oauth2.0 token and authenticate with Google, when authorization code is present in client URL. De-authenticate when not present.
      #### But when we come back, we'll run oauth2.0_token()
      observeEvent(google_info$is_authorized, {
        if(google_info$is_authorized == 'no') {
          bigrquery::bq_deauth()
          } else { 
            # shinyjs::hide(id = 'google_connect_div')
            # shinyjs::show(id = 'google_authenticated_div')
            token <- oauth2.0_token(app = app,
                                    endpoint = api,
                                    credentials = oauth2.0_access_token(api, app, params()$code),
                                    cache = FALSE
                                    )
            google_info$token <- token
            #### And authenticate the UI
            bigrquery::bq_auth(token = token)
            bigquery_setup$user_info <- gargle::token_userinfo(token = token)
            bigquery_setup$bq_projects <- bigrquery::bq_projects()
            # browser()
          }
        })
      
      observeEvent(bigquery_setup$user_info, {
        req(bigquery_setup$user_info)
        shinyjs::hide(id = 'google_connect_div')
        shinyjs::show(id = 'google_authenticated_div')
      })
      
      # output$token_info <- renderPrint(google_info$token) ## Debug
      
      ## Define the reactive BQ Setup UI ----
      google_authenticated_ui <- reactive({
        req(bigquery_setup$user_info)
        tagList(
          shinydashboardPlus::widgetUserBox(title = bigquery_setup$user_info$name,
                                            subtitle = bigquery_setup$user_info$email,
                                            src = bigquery_setup$user_info$picture,
                                            type = 2, 
                                            color = 'primary',
                                            collapsible = FALSE,
                                            HTML(glue::glue('{bigquery_setup$user_info$given_name}, you have successfully authenticated with Google BigQuery. Please select a dataset from from the list of available projects, or sign out and sign in with a different Google Account.<br><br>')),
                                            br(),
                                            selectizeInput(inputId = ns('bq_project_id'),
                                                           label = 'Select from Available Google Projects:',
                                                           choices = bigquery_setup$bq_projects,
                                                           options = list(create = FALSE,
                                                                          placeholder = 'No Available Projects')
                                                           ),
                                            selectizeInput(inputId = ns('bq_dataset_id'),
                                                           label = 'Select from Available BigQuery Datasets:',
                                                           choices = NULL
                                                           ),
                                            shinyjs::hidden(
                                              div(id = ns('bq_connect_div'),
                                                actionButton(inputId = ns('bq_connect'),label = 'Connect',icon = icon('cloud'))
                                                )
                                              ),
                                            footer = fluidRow(
                                              div(actionBttn(inputId = ns('logout'),
                                                             label = 'Sign Out of Google',
                                                             style = 'jelly',
                                                             icon = icon(name = 'sign-out-alt')
                                                             ),
                                                  style="float:right"
                                                  )
                                              )
                                            )
          )
      })
      
      ### When the user selects a GCP Project, populate the available dataset selector choices. 
      observeEvent(input$bq_project_id, {
        req(input$bq_project_id)
        dataset_choices <- bigrquery::bq_project_datasets(input$bq_project_id) %>%
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
                                            placeholder = 'Please ensure you have access to a BigQuery dataset in this project.')
                             )
        })
      
      ### When the user selects an available dataset, show the 'connect' button
      observeEvent(input$bq_dataset_id, {
        req(input$bq_dataset_id)
        shinyjs::show(id = 'bq_connect_div')
        })
      
      ### Store configured values as a connection object
      observeEvent(input$bq_connect, {
        bigquery_setup$bq_project_id <- input$bq_project_id
        bigquery_setup$bq_dataset_id <- input$bq_dataset_id
        bigquery_setup$db_con <- DBI::dbConnect(drv = bigquery(),
                                                project = bigquery_setup$bq_project_id,
                                                dataset = bigquery_setup$bq_dataset_id
                                                )
        bigquery_setup$is_connected <- 'yes'
        shinyjs::hide('google_authenticated_div')
        shinyjs::show('google_configured_div')
        })
      
      google_configured_ui <- reactive({
        req(bigquery_setup$is_connected == 'yes')
        tagList(
          shinydashboardPlus::widgetUserBox(title = bigquery_setup$user_info$name,
                                            subtitle = bigquery_setup$user_info$email,
                                            src = bigquery_setup$user_info$picture,
                                            type = 2, 
                                            color = 'primary',
                                            collapsible = FALSE,
                                            HTML(paste('<H3>Success!!</H3>',
                                                       'You have connected to a Google BigQuery database.',
                                                       '<br>',
                                                       '<br>',
                                                       '<H4>Connection Information:</H4>',
                                                       '<b>Project:</b>', bigquery_setup$bq_project_id,
                                                       '<br>',
                                                       '<b>Dataset:</b>', bigquery_setup$bq_dataset_id,
                                                       '<br>'
                                                       )
                                                 ),
                                            actionButton(inputId = ns('disconnect'),label = 'Disconnect'),
                                            footer = fluidRow(
                                              div(actionBttn(inputId = ns('logout_2'),
                                                             label = 'Sign Out of Google',
                                                             style = 'jelly',
                                                             icon = icon(name = 'sign-out-alt')
                                                             ),
                                                  style="float:right"
                                                  )
                                              )
                                            )
          )
        })
      
      observeEvent(input$disconnect, {
        bigquery_setup$bq_project_id <- NULL
        bigquery_setup$bq_dataset_id <- NULL
        bigquery_setup$db_con <- NULL
        bigquery_setup$is_connected <- 'no'
        shinyjs::hide('google_configured_div')
        shinyjs::show('google_authenticated_div')
      })
      
      ## BigQuery Setup Outputs ----
      output$google_authenticated_ui <- renderUI({ google_authenticated_ui() })
      output$google_configured_ui <- renderUI({ google_configured_ui() })
      outputOptions(output, 'google_authenticated_ui', suspendWhenHidden = F)
      outputOptions(output, 'google_configured_ui', suspendWhenHidden = F)
      
      # Return Setup Values ----
      return(bigquery_setup)
      }
    )
  }
