
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinyBigQuery

<!-- badges: start -->

<!-- badges: end -->

The goal of shinyBigQuery is to allow a Shiny application to perform
interactive authentication with Google BigQuery and assist with creating
DBI connection info to projects and datasets that the user has access
to.

## Installation

You can install the released version of shinyBigQuery from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("thewileylab/shinyBigQuery")
```

## Example

To run a demo application:

``` r
shinyBigQuery::run_app()
```

## Usage

To integrate shinyBigQuery with your Shiny application place
`bigquery_setup_ui()` and `bigquery_setup_server()` functions into your
applications ui and server functions respectively. Note, as Google
relies on OAuth 2.0 authentication, this application must run on port
8100 in a browser. An example is given below:

``` r
library(shiny)
library(shinyBigQuery)
ui <- fluidPage(
  tags$h2('Connect to BigQuery UI'),
  bigquery_setup_ui(id = 'setup-namespace')
  )

server <- function(input, output, session) {
  bq_setup_vars <- bigquery_setup_server(id = 'setup-namespace')
}

if (interactive())
  shinyApp(ui = ui, server = server, options = list(port = 1410, launch.browser = T))
```

## Code of Conduct

Please note that the ‘shinyBigQuery’ project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
