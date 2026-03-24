# Register shiny-output options to allow display in stand-alone viewers

Save the output options such that the additional configurations can be
used by stand-alone viewer

## Usage

``` r
standalone_viewer(
  outputId,
  module_session,
  rave_id,
  session = shiny::getDefaultReactiveDomain(),
  wrapper_id = "viewer"
)
```

## Arguments

- outputId:

  the full shiny output ID

- module_session:

  the module shiny session; if not provided, then the session will be
  inferred by `rave_id`

- rave_id:

  the unique identification key for 'RAVE' module sessions, can be
  obtained via
  [`get_active_module_info`](https://dipterix.org/ravedash/reference/get_active_module_info.md)

- session:

  shiny session object

- wrapper_id:

  the wrapping render ID, default is `"viewer"`

## Value

nothing

## Details

'RAVE' dashboard provides powerful stand-alone viewers where users can
display almost any outputs from other modules and interact with these
viewers while sending messages back.

## Examples

``` r
if(interactive()) {

library(shiny)
library(ravedash)

rave_id <- paste(sample(c(letters, LETTERS, 0:9), 20, replace = TRUE),
                 collapse = "")

ui <- function(req) {
  query_string <- req$QUERY_STRING
  if(length(query_string) != 1) {
    query_string <- "/"
  }
  query_result <- httr::parse_url(query_string)

  if(!identical(toupper(query_result$query$standalone), "TRUE")) {
    # normal page
    basicPage(
      actionButton("btn", "Click Me"),
      plotOutput("plot")
    )
  } else {
    # standalone viewer
    uiOutput("viewer")
  }
}

server <- function(input, output, session) {

  bindEvent(
    safe_observe({
      query_string <- session$clientData$url_search
      query_result <- httr::parse_url(query_string)

      if(!identical(toupper(query_result$query$standalone), "TRUE")) {
        # normal page
        register_rave_session(session = session, .rave_id = rave_id)
        output$plot <- renderPlot({
          input$btn
          plot(rnorm(100), pch = 16)
        })
      } else {
        # standalone viewer
        standalone_viewer(outputId = "plot", rave_id = rave_id)
      }
    }),
    session$clientData$url_search
  )


}

shinyApp(ui, server, options = list(port = 8989))

# Now open http://127.0.0.1:8989/?standalone=TRUE

}
```
