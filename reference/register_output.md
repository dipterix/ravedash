# Register output and output options

Enable advanced output gadgets such as expanding the output in another
browser window, or downloading the rendered data.

## Usage

``` r
register_output_options(
  outputId,
  ...,
  .opt = list(),
  extras = list(),
  session = shiny::getDefaultReactiveDomain()
)

get_output_options(outputId, session = shiny::getDefaultReactiveDomain())

register_output(
  render_function,
  outputId,
  ...,
  output_opts = list(),
  quoted = FALSE,
  download_function = NULL,
  download_fileformat =
    "{ outputId }-{ format(Sys.time(), '%b_%d_%Y_%H_%M_%S') }.{ extension }",
  output_type = c("image", "data", "threeBrain", "no-download"),
  extensions = NULL,
  title = "Download widget",
  cancel_btn = "Cancel",
  confirm_btn = "Download",
  session = shiny::getDefaultReactiveDomain()
)

get_output(outputId, session = shiny::getDefaultReactiveDomain())
```

## Arguments

- outputId:

  output ID in the scope of current shiny session

- extras:

  extra information to store

- session:

  shiny session instance

- render_function:

  shiny render function

- output_opts, .opt:

  output options

- quoted:

  whether `render_function` is quoted; default is false

- download_function:

  core function that writes the data into the files; default is set for
  `'image'` and `'threeBrain'` automatically; see 'Default' and
  'Examples'.

- download_fileformat:

  download file format, supports 'glue'

- output_type:

  type of export file formats supported, options are `'image'` (for
  figures, default), `'data'`, `'threeBrain'` (for 'RAVE' 3D viewers),
  and `'no-download'` (do not export).

- extensions:

  a list of file extensions and their descriptions; the names will be
  used to display the modal selectors, and values are the underlying
  extension read by `download_fileformat`

- title, cancel_btn, confirm_btn, ...:

  title, button labels, and additional 'HTML' elements that are to be
  shown in the modal

## Value

Registered output or output options.

## Details

The following steps are done when `register_output` is called:

\* Register the render function to shiny output `output[[outputId]]` \*
Register the render information to session which can be retrieved via
`get_output` \* Register (if `download_function` is a function) a
download handler that listen to the shiny event. The event ID is
`paste0(outputId, '__download2')`.

When downloading event is triggered, a modal will pop up asking for
exporting format (always exists) and image dimensions (if output type is
`'image'`) or title (when output type is `'threeBrain'`). Users will
choose the proper inputs, which will be passed into `download_function`.

The file `extensions` is a named list. Its names are printable
descriptions of the formats, and values are the file extensions (without
the leading `'.'`). for example, `list("compressed CSV" = "csv")`. Users
will see `"compressed CSV"` in the format selector, and
`download_function` sees `"csv"`.

When output type is image, users will be asked to enter the image size
in inches; default width is `7`, and height is calculated based on
current image aspect ratio.

If you would like to show more on the modal, pass 'HTML' elements to
`...`

Function `download_function` is a function containing four inputs:

\* `con`: file where the data should be written into \* `params`: a
named list of `params$extension` (file extension), `width`, `height`
(type is image), or `title` (3D viewer) \* `render_expr` a quoted
rendering expression of the rendering function \* `render_env` the
rendering environment of the rendering function.

Default `download_function` is provided when not specified.

## Examples

``` r

if(interactive()) {

library(shiny)
library(ravedash)

# ---- Use this in RAVE --------

# UI
output_gadget_container(
  plotOutput("plot", brush = shiny::brushOpts("plot__brush")),
)

# server
server <- function(input, output, session) {
  register_output(
    renderPlot({
      # ... plot it
    }),
    outputId = "plot",
    output_opts = list(brush = shiny::brushOpts("plot__brush"))
  )
}


# ---- Low-level method ------------

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
      output_gadget_container(
        plotOutput("plot", brush = shiny::brushOpts("plot__brush")),
      )
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

      if(!identical(toupper(query_result$query$module), "standalone_viewer")) {
        # normal page
        register_rave_session(session = session, .rave_id = rave_id)
        register_output(
          renderPlot({
            input$btn
            plot(rnorm(100), pch = 16)
          }),
          outputId = "plot",
          output_opts = list(brush = shiny::brushOpts("plot__brush"))
        )
      } else {
        # standalone viewer
        standalone_viewer(outputId = "plot", rave_id = rave_id)
      }
    }),
    session$clientData$url_search
  )


}

shinyApp(ui, server, options = list(port = 8989))
}
```
